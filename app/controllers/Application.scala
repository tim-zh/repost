package controllers

import com.ning.http.util.AsyncHttpProviderUtils
import java.io._
import java.util.UUID
import models.User
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.iteratee._
import play.api.libs.ws.WS
import play.api.mvc._
import play.cache.Cache
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.future

object Application extends Controller {
  implicit val impDao = dao

  def index(page: Int) = Action { implicit req =>
    dao.init()
    val user = getUserFromSession
    val (pagesNumber, entries) = dao.getEntries(user, page, getItemsOnPage(user))
    Ok(views.html.index(user, page, pagesNumber, entries))
  }

  def auth() = Action { implicit req =>
    val loginForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText)(LoginData.apply)(LoginData.unapply))
    loginForm.bindFromRequest.fold(
      badForm =>
        Redirect("/"),
      loginData => {
        val user = dao.getUser(loginData.name, loginData.password)
        user match {
          case Some(x) =>
            val key = UUID.randomUUID().toString
            Cache.set(key, x)
            Redirect("/").withSession(req.session +("user", key))
          case None =>
            Redirect("/")
        }
      }
    )
  }

  def logout() = Action { implicit req =>
    Cache.remove(req.session.get("user").getOrElse(""))
    Redirect("/").withNewSession
  }

  def register() = Action { implicit req =>
    val registerForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText, "pass2" -> nonEmptyText)
      (RegisterData.apply)(RegisterData.unapply))
    registerForm.bindFromRequest.fold(
      badForm =>
        Ok(views.html.register(if (req.method == "POST")
          badForm.errors ++ new RegisterData(badForm.data).validate else Nil, badForm.data)),
      registerData => {
        val errors = registerData.validate
        if (errors.isEmpty) {
          val newUser = dao.addUser(registerData.name, registerData.password)
          Redirect("/").withSession(req.session +("user", newUser.id + ""))
        } else
          Ok(views.html.register(errors, registerData.toMap))
      }
    )
  }

  def search(page: Int, query: String) = Action { implicit req =>
    if (query.isEmpty)
      Redirect("/")
    else {
      val user = getUserFromSession
      val (pagesNumber, entries) = dao.getEntriesBySearch(user, query, page, getItemsOnPage(user))
      val tags = dao.getTagsBySearch(query)
      Ok(views.html.search(user, page, pagesNumber, entries, tags, "?query=" + query))
    }
  }

  def tag(title: String, page: Int) = Action { implicit req =>
    val user = getUserFromSession
    val tag = dao.getTag(title)
    renderOption(tag) { x =>
      val (pagesNumber, entries) = dao.getEntriesByTag(user, x, page, getItemsOnPage(user))
      Ok(views.html.index(user, page, pagesNumber, entries, s"/tag/${x.title}", x.title, Some(x)))
    }
  }

  //ajax call from saveEntry form
  def tags(query: String) = Action { implicit req =>
    val tags = dao.getTagsBySearch(query)
    val response = Json.obj("suggestions" -> tags.map(tag => Json.obj("value" -> tag.title)))
    Ok(response)
  }

  def entry(id: Long, commentId: Long = -1) = Action { implicit req =>
    val user = getUserFromSession
    val entry = dao.getEntry(user, id)
    renderOption(entry) { x => Ok(views.html.entry(user, x, commentId)) }
  }

  def saveEntry(entryId: Long) = Action { implicit req =>
    getUserFromSession match {
      case Some(user) =>
        if (entryId == -1) {
          val saveEntryForm = Form(mapping("id" -> longNumber,
                                           "title" -> text(0, 140),
                                           "tagsHiddenString" -> text,
                                           "openForAll" -> boolean,
                                           "content" -> nonEmptyText)(EntryData.apply)(EntryData.unapply))
          saveEntryForm.bindFromRequest.fold(
            badForm =>
              Ok(views.html.saveEntry(Some(user), if (req.method == "POST") badForm.errors else Nil, badForm.data)),
            entryData => {
              val tags = dao.getTagsByTitles(
                titles = entryData.tags.split(",").filter("""^[\w \-]+$""".r.findFirstIn(_).isDefined),
                addNew = true)
              if (entryData.id == -1) {
                val newEntry = dao.addEntry(user, entryData.title, tags, entryData.openForAll,
                  getHtmlFromBbCodeAndEscape(entryData.content))
                Redirect(routes.Application.entry(newEntry.id))
              } else {
                dao.getEntry(Some(user), entryData.id) match {
                  case Some(x) =>
                    dao.updateEntry(Some(user), entryData.id, entryData.title, tags, entryData.openForAll,
                      getHtmlFromBbCodeAndEscape(entryData.content))
                    Ok(views.html.entry(Some(user), x))
                  case None =>
                    Redirect("/")
                }
              }
            }
          )
        } else {
          val entry = dao.getEntry(Some(user), entryId)
          if (entry.isDefined && entry.get.author.id == user.id) {
            val entryData = Map("id" -> entryId.toString,
                                "title" -> entry.get.title,
                                "tagsHiddenString" -> entry.get.tags.mkString(","),
                                "openForAll" -> entry.get.openForAll.toString,
                                "content" -> entry.get.content)
            Ok(views.html.saveEntry(Some(user), Nil, entryData))
          } else
            Redirect("/")
        }
      case None =>
        Redirect("/")
    }
  }

  //ajax call from saveEntry form
  def deleteEntry() = Action { implicit req =>
    val user = getUserFromSession
    val id = Form(single("id", longNumber)).bindFromRequest().get
    if (dao.deleteEntry(user, id))
      Ok("true")
    else
      Ok("false")
  }

  def user(id: Long) = Action { implicit req =>
    val currentUser = getUserFromSession
    val user = dao.getUser(id)
    renderOption(user) { x =>
      Ok(views.html.user(currentUser, x, Nil,
        UserData("", "", "", x.compactEntryList, x.dateFormat, x.itemsOnPage, x.codeTheme).toMap))
    }
  }

  def saveUser() = Action { implicit req =>
    val user = getUserFromSession
    if (user.isDefined) {
      val saveUserForm = Form(mapping("oldPass" -> text,
                                      "newPass" -> text,
                                      "newPass2" -> text,
                                      "compactEntryList" -> boolean,
                                      "dateFormat" -> text,
                                      "itemsOnPage" -> number,
                                      "codeTheme" -> number)(UserData.apply)(UserData.unapply))
      saveUserForm.bindFromRequest().fold(
        badForm =>
          Ok(views.html.user(user, user.get, badForm.errors ++
            new UserData(badForm.data).validate(user.get.password), badForm.data)),
        userData => {
          val errors = userData.validate(user.get.password)
          if (errors.isEmpty) {
            val updatedUser = dao.updateUser(user.get.id, if (userData.newPass != user.get.password) userData.newPass else user.get.password,
              userData.compactEntryList, userData.dateFormat, userData.itemsOnPage, userData.codeTheme)
            if (updatedUser.isDefined)
              Cache.set(req.session.get("user").getOrElse("-1"), updatedUser.get)
            Redirect("/")
          } else
            Ok(views.html.user(user, user.get, errors, userData.toMap))
        }
      )
    } else
      Redirect("/")
  }

  //ajax call from user form
  def deleteUser() = Action { implicit req =>
    val user = getUserFromSession
    if (dao.deleteUser(user))
      Ok("true")
    else
      Ok("false")
  }

  def saveComment() = Action { implicit req =>
    getUserFromSession match {
      case Some(user) =>
        val saveCommentForm = Form(mapping("entryId" -> longNumber,
                                           "content" -> nonEmptyText)(CommentData.apply)(CommentData.unapply))
        saveCommentForm.bindFromRequest.fold(
          badForm => badForm.data.get("entryId") match {
            case Some(entryIdString) =>
              try {
                Redirect(routes.Application.entry(entryIdString.toLong))
              } catch {
                case t: NumberFormatException =>
                  Redirect("/")
              }
            case None =>
              Redirect("/")
          },
          commentData => {
            dao.getEntry(Some(user), commentData.entryId) match {
              case Some(entry) =>
                val newComment = dao.addComment(user, entry, getHtmlFromBbCodeAndEscape(commentData.content))
                Redirect(routes.Application.entry(commentData.entryId, newComment.id))
              case None =>
                Redirect("/")
            }
          }
        )
      case None =>
        Redirect("/")
    }
  }

  //ajax call from entry form
  def deleteComment() = Action { implicit req =>
    val user = getUserFromSession
    val id = Form(single("id", longNumber)).bindFromRequest().get
    if (dao.deleteComment(user, id))
      Ok("true")
    else
      Ok("false")
  }

  //ajax call from favorite tags panel in wrapper
  def addFavoriteTag(title: String) = Action { implicit req =>
    val user = getUserFromSession
    if (dao.addFavoriteTag(user, title))
      Ok("true")
    else
      Ok("false")
  }

  //ajax call from favorite tags panel in wrapper
  def removeFavoriteTag(title: String) = Action { implicit req =>
    val user = getUserFromSession
    if (dao.removeFavoriteTag(user, title))
      Ok("true")
    else
      Ok("false")
  }

  //ajax call from saveEntry form
  def saveImageFromUrl(url: String) = Action.async { implicit req =>
    def fromStream(stream: OutputStream): Iteratee[Array[Byte], Unit] = Cont {
      case e @ Input.EOF =>
        stream.close()
        Done((), e)
      case Input.El(data) =>
        stream.write(data)
        fromStream(stream)
      case Input.Empty =>
        fromStream(stream)
    }

    val user = getUserFromSession
    var result: Future[SimpleResult] = future(Ok(""))
    if (user.isDefined) {
      var filename = util.Random.nextLong().toHexString + """(\.\w{1,5})$""".r.findFirstIn(url).getOrElse("")
      try {
        AsyncHttpProviderUtils.validateSupportedScheme(AsyncHttpProviderUtils.createUri(url))
        val file = new File("public/images/uploaded/", filename)
        if (!file.createNewFile())
          throw new IOException()
        val outputStream = new BufferedOutputStream(new FileOutputStream(file.getAbsoluteFile))
        result = WS.url(url).withRequestTimeout(30000).get(headers => fromStream(outputStream)).flatMap(_.run).map {
          _ => Ok(if (filename.isEmpty) "" else "/assets/images/uploaded/" + filename)
        }
      } catch {
        case e @ (_: IOException | _: SecurityException | _: IllegalArgumentException) =>
          e.printStackTrace()
          filename = ""
      }
    }
    result
  }

  //ajax call from saveEntry form
  def saveImageFromFile() = Action.async(parse.multipartFormData) { implicit req =>
    val user = getUserFromSession
    var filename = ""
    if (user.isDefined) {
      try {
        req.body.file("quickImageFile").map { image =>
          filename = util.Random.nextLong().toHexString + """(\.\w{1,5})$""".r.findFirstIn(image.filename).getOrElse("")
          val file = new File("public/images/uploaded/", filename)
          if (!file.createNewFile())
            throw new IOException()
          image.ref.moveTo(file, replace = true)
        }
      }
      catch {
        case e @ (_: IOException | _: SecurityException) =>
          e.printStackTrace()
          filename = ""
      }
    }
    future(Ok(if (filename.isEmpty) "" else "/assets/images/uploaded/" + filename))
  }
}