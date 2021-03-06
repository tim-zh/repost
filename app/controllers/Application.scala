package controllers

import com.ning.http.util.AsyncHttpProviderUtils
import java.io._
import java.util.concurrent.ConcurrentHashMap
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.mvc._
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

object Application extends Controller {

  def index(page: Int) = Action { implicit req =>
    var user = getUserFromSession
    if (user.isDefined && req.body.asFormUrlEncoded.exists(_.contains("changeListType"))) {
      val listType = models.ListType((user.get.entryListType.id + 1) % 3)
      user = dao.updateUser(user.get.id, user.get.password, listType, user.get.dateFormat, user.get.itemsOnPage, user.get.codeTheme)
      if (user.isDefined)
        AuthController.updateSession(user.get)
    }
    val (pagesNumber, entries) = dao.getEntries(user, page, getItemsOnPage(user))
    Ok(views.html.index(user, page, pagesNumber, entries))
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
          AuthController.authUser(Some(newUser))
        } else
          Ok(views.html.register(errors, registerData.toMap))
      }
    )
  }

  def search(page: Int) = Action { implicit req =>
    val user = getUserFromSession
    val searchForm = Form(mapping("query" -> text,
                                  "isEntries" -> boolean,
                                  "isTags" -> boolean,
                                  "from" -> optional(date),
                                  "to" -> optional(date),
                                  "users" -> text,
                                  "tags" -> text)(SearchData.apply)(SearchData.unapply))
    searchForm.bindFromRequest.fold(
      badForm => {
        if (badForm.data contains "query") {
          val query = badForm.data("query")
          val (pagesNumber, entries) = dao.getEntriesBySearch(user, query, page, getItemsOnPage(user))
          val tags = dao.getTagsBySearch(query)
          Ok(views.html.search(user, page, pagesNumber, entries, tags, "?query=" + query))
        } else
          Ok(views.html.searchForm(user))
      },
      searchData => {
        val tags = if (searchData.isTags) dao.getTagsBySearch(searchData.query) else Nil
        val (pagesNumber, entries) = if (searchData.isEntries) {
          val usersFromString = dao.getUsersByNames(getSafeSeqFromString(searchData.users))
          val tagsFromString = dao.getTagsByTitles(getSafeSeqFromString(searchData.tags), addNew = false)
          dao.getEntriesBySearch(user, searchData.query, searchData.from, searchData.to,
            usersFromString, tagsFromString, page, getItemsOnPage(user))
        } else
          (0L, Nil)
        Ok(views.html.search(user, page, pagesNumber, entries, tags, "?" + searchData.toUrlString))
      })
  }

  def searchForm() = Action { implicit req =>
    val user = getUserFromSession
    Ok(views.html.searchForm(user))
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

  //ajax call from search form
  def users(query: String) = Action { implicit req =>
    val users = dao.getUsersBySearch(query)
    val response = Json.obj("suggestions" -> users.map(user => Json.obj("value" -> user.name)))
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
              val tags = dao.getTagsByTitles(getSafeSeqFromString(entryData.tags), addNew = true)
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
        UserData("", "", "", x.entryListType.id, x.dateFormat, x.itemsOnPage, x.codeTheme).toMap))
    }
  }

  def saveUser() = Action { implicit req =>
    val user = getUserFromSession
    if (user.isDefined) {
      val saveUserForm = Form(mapping("oldPass" -> text,
                                      "newPass" -> text,
                                      "newPass2" -> text,
                                      "entryListType" -> number(0, 2),
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
              models.ListType(userData.entryListType), userData.dateFormat, userData.itemsOnPage, userData.codeTheme)
            if (updatedUser.isDefined)
              AuthController.updateSession(updatedUser.get)
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
    val result = dao.deleteUser(user).toString
    Ok(result)
  }

  def saveComment() = Action { implicit req =>
    getUserFromSession match {
      case Some(user) =>
        val saveCommentForm = Form(mapping("entryId" -> longNumber,
                                           "content" -> nonEmptyText)(CommentData.apply)(CommentData.unapply))
        saveCommentForm.bindFromRequest.fold(
          badForm => badForm.data.get("entryId") match {
            case Some(entryIdString) =>
              try
                Redirect(routes.Application.entry(entryIdString.toLong))
              catch {
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
    val result = dao.deleteComment(user, id).toString
    Ok(result)
  }

  //ajax call from favorite tags panel in wrapper
  def addFavoriteTag(title: String) = Action { implicit req =>
    val user = getUserFromSession
    val result = dao.addFavoriteTag(user, title).toString
    Ok(result)
  }

  //ajax call from favorite tags panel in wrapper
  def removeFavoriteTag(title: String) = Action { implicit req =>
    val user = getUserFromSession
    val result = dao.removeFavoriteTag(user, title).toString
    Ok(result)
  }

  //ajax call from saveEntry form
  def saveImageFromUrl(url: String) = Action.async { implicit req =>
    val user = getUserFromSession
    saveImage(url, user).map(s => Ok(s.getOrElse("")))
  }

  //ajax call from saveEntry form
  def saveImageFromFile() = Action.async(parse.multipartFormData) { implicit req =>
    val user = getUserFromSession
    var filename = ""
    Future {
      if (user.isDefined)
        try
          req.body.file("quickImageFile").foreach { image =>
            val file = createImageFile(image.filename)
            filename = file.getName
            image.ref.moveTo(file, replace = true)
          }
        catch {
          case e @ (_: IOException | _: SecurityException) =>
            e.printStackTrace()
            filename = ""
        }
      Ok(if (filename.isEmpty) "" else "/assets/images/uploaded/" + filename)
    }
  }

  //ajax call from saveEntry form
  def saveContentFromUrl(url: String, startText: String, endText: String) = Action.async { implicit req =>
    def getBbWrapFromTag(e: Element): (String, String) = e.tagName match {
      case "br" => ("[br]", "")
      case "strong" | "b" => ("[b]", "[/b]")
      case t @ ("i" | "u" | "s" | "blockquote" | "ol" | "li" | "p") => ("[" + t + "]", "[/" + t + "]")
      case "font" => if (e.hasAttr("size")) ("[size=" + e.attr("size") + "]", "[/size]") else ("", "")
      case "a" => if (e.hasAttr("href")) ("[url=" + e.attr("href") + "]", "[/url]") else ("", "")
      case "img" =>
        if (e.hasAttr("src")) {
          var size = ""
          if (e.hasAttr("width") || e.hasAttr("height"))
            size = "=" + (if (e.hasAttr("width")) e.attr("width") else "") + "," + (if (e.hasAttr("height")) e.attr("height") else "")
          ("[img" + size + "]" + e.attr("src"), "[/img]")
        } else
          ("", "")
      case "pre" | "code" => ("[code]", "[/code]")
      case "h1" | "h2" | "h3" | "h4" | "h5" => ("[size=" + (55 - e.tagName.charAt(1)) + "]", "[/size]")
    }
    val validTagSet = Set("br", "strong", "b", "i", "u", "s", "font", "a", "img", "blockquote", "ol", "li", "pre",
      "code", "p", "h1", "h2", "h3", "h4", "h5")
    def parseTree(elements: Elements) {
      elements.iterator.foreach { e =>
        if (validTagSet contains e.tagName) {
          val bbCodes = getBbWrapFromTag(e)
          e.prependText(bbCodes._1)
          e.appendText(bbCodes._2)
        }
        parseTree(e.children)
        e.unwrap()
      }
    }

    getUserFromSession match {
      case user @ Some(_) =>
        try {
          AsyncHttpProviderUtils.validateSupportedScheme(AsyncHttpProviderUtils.createUri(url))
          WS.url(url).withFollowRedirects(true).withRequestTimeout(30000).get().map {
            response => {
              var start = if (startText.isEmpty) 0 else response.body.indexOf(startText)
              var end = if (endText.isEmpty) 0 else response.body.indexOf(endText, start) + endText.length
              while (response.body.lastIndexOf(">", start) == start - 1 &&
                  response.body.lastIndexOf("</", start) < response.body.lastIndexOf("<", start - 1))
                start -= response.body.lastIndexOf(">", start) - response.body.lastIndexOf("<", start) + 1
              while (response.body.indexOf("</", end) == end)
                end += response.body.indexOf(">", end) - response.body.indexOf("</", end) + 1
              val content = if (end == 0) response.body.substring(start) else response.body.substring(start, end)
              val doc = Jsoup.parseBodyFragment(content)
              val urlMap = new ConcurrentHashMap[String, String]
              var futureList = List[Future[Unit]]()
              for (img <- doc.select("img").iterator)
                futureList = saveImage(img.attr("src"), user).map[Unit](s => if (s.isDefined) urlMap.put(img.attr("src"), s.get)) :: futureList
              Future.sequence(futureList) map { _ =>
                parseTree(doc.body.children)
                  var text = doc.body.html
                  urlMap.foreach(pair => text = text.replaceAll(pair._1, pair._2))
                  text = text.replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\\[br]", "\r\n").
                    replaceAll("\\[code]\\[code]", "[code]").replaceAll("\\[/code]\\[/code]", "[/code]")
                  Ok(text)
              }
            }
          }.flatMap(f => f)
        } catch {
          case e: IllegalArgumentException =>
            e.printStackTrace()
            Future(Ok(""))
        }
      case None =>
        Future(Ok(""))
    }
  }

  def importForm() = Action { implicit req =>
    val user = getUserFromSession
    Ok(views.html.importForm(user))
  }

  def importContent() = Action.async(parse.multipartFormData) { implicit req =>
    def replaceC(s: String, c: Int) = s.replaceAll("\\[\\[C]]", c.toString)
    def replaceBr(s: String) = s.replaceAll("""(\r\n|\r|\n|\n\r)""", "<br/>")
    val user = getUserFromSession
    val importForm = Form(mapping("title" -> text,
                                  "openForAll" -> boolean,
                                  "tagsHiddenString" -> text,
                                  "startText" -> text,
                                  "endText" -> text,
                                  "texts" -> text,
                                  "separator" -> text)(ImportData.apply)(ImportData.unapply))
    Future {
      importForm.bindFromRequest.fold(
        badForm => Ok(views.html.importList(user)),
        importData => {
          val tags = dao.getTagsByTitles(getSafeSeqFromString(importData.tags), addNew = true)
          if (user.isDefined) {
            var entries: List[models.Entry] = Nil
            var counter = 0
            val startText = replaceBr(escape(importData.startText.replaceAll("\\[\\[N]]", "\r\n")))
            val endText = replaceBr(escape(importData.endText.replaceAll("\\[\\[N]]", "\r\n")))
            val separator = importData.separator.replaceAll("\\[\\[N]]", "\r\n")
            req.body.files.foreach { f =>
              try {
                val file = createImageFile(f.filename)
                f.ref.moveTo(file, replace = true)
                counter += 1
                entries = dao.addEntry(user.get, replaceC(importData.title, counter), tags, importData.openForAll,
                  replaceC(startText, counter) + "<img src='/assets/images/uploaded/" + file.getName + "'/>" +
                    replaceC(endText, counter)) :: entries
              } catch {
                case e @ (_: IOException | _: SecurityException) => e.printStackTrace()
              }
            }
            if (!importData.texts.isEmpty)
              if (importData.separator.isEmpty)
                entries = dao.addEntry(user.get, replaceC(importData.title, counter), tags, importData.openForAll,
                  replaceC(startText, counter) + replaceBr(escape(importData.texts)) +
                    replaceC(endText, counter)) :: entries
              else
                importData.texts.split(separator).foreach { str =>
                  counter += 1
                  entries = dao.addEntry(user.get, replaceC(importData.title, counter), tags, importData.openForAll,
                    replaceC(startText, counter) + replaceBr(escape(str)) +
                      replaceC(endText, counter)) :: entries
                }
            Ok(views.html.importList(user, entries))
          } else
            Ok(views.html.importList(user))
        }
      )
    }
  }
}