package controllers

import play.api.mvc._
import models.Dao
import play.api.data._
import play.api.data.Forms._
import models.squeryl.SquerylDao

object Application extends Controller {

  implicit val dao = SquerylDao
  val itemsOnPage = 2

  def index(page: Int) = Action { implicit req =>
    dao.init()
    val user = getUserFromSession
    val (pagesNumber, entries) = dao.getEntries(user, page, itemsOnPage)
    Ok(views.html.entries(user, page, pagesNumber, entries))
  }

  def auth() = Action { implicit req =>
    val loginForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText)(LoginData.apply)(LoginData.unapply))
    loginForm.bindFromRequest.fold(
      badForm => Redirect("/"),
      loginData => {
        val user = dao.getUser(loginData.name, loginData.password)
        user match {
          case Some(x) =>
            Redirect("/").withSession(req.session +("user", x.id + ""))
          case None =>
            Redirect("/")
        }
      }
    )
  }

  def logout() = Action { implicit req =>
    if (req.method == "POST")
      Redirect("/").withNewSession
    else
      Redirect("/")
  }

  def register() = Action { implicit req =>
    val registerForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText, "pass2" -> nonEmptyText)
      (RegisterData.apply)(RegisterData.unapply))
    registerForm.bindFromRequest.fold(
      badForm => Ok(views.html.register(if (req.method == "POST")
        badForm.errors ++ new RegisterData(badForm.data).validate else Nil, badForm.data)),
      registerData => {
        val errors = registerData.validate
        if (errors.isEmpty) {
          val newUser = dao.addUser(registerData.name, registerData.password)
          Redirect("/").withSession(req.session +("user", newUser.id + ""))
        }
        else
          Ok(views.html.register(errors, registerData.toMap))
      }
    )
  }

  def search(page: Int) = Action { implicit req =>
    val searchForm = Form(single("query", text))
    searchForm.bindFromRequest().get match {
      case query: String =>
        if (query.isEmpty)
          Redirect("/")
        else {
          val user = getUserFromSession
          val (pagesNumber, entries) = dao.getEntriesBySearch(user, query, 0, Int.MaxValue)
          Ok(views.html.entries(user, page, pagesNumber, entries, "/search"))
        }
      case _ => Redirect("/")
    }
  }

  def tag(title: String, page: Int) = Action { implicit req =>
    val user = getUserFromSession
    val tag = dao.getTag(title)
    renderOption(tag) { x =>
      val (pagesNumber, entries) = dao.getEntriesByTag(user, x, page, itemsOnPage)
      Ok(views.html.entries(user, page, pagesNumber, entries, s"/tag/${x.title}", x.title))
    }
  }

  def entry(id: Long, commentId: Long = -1) = Action { implicit req =>
    val user = getUserFromSession
    val entry = dao.getEntry(user, id)
    renderOption(entry) { x => Ok(views.html.entry(user, x, commentId)) }
  }

  def saveEntry(entryId: Long) = Action { implicit req =>
    getUserFromSession match {
      case Some(user) =>
        if (entryId != -1) {
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
        } else {
          val saveEntryForm = Form(mapping("id" -> longNumber,
                                           "title" -> nonEmptyText(1, 140),
                                           "tagsHiddenString" -> text,
                                           "openForAll" -> boolean,
                                           "content" -> nonEmptyText)(EntryData.apply)(EntryData.unapply))
          saveEntryForm.bind(Map("id" -> "1",
                                          "title" -> "entry1",
                                          "tagsHiddenString" -> "",
                                          "openForAll" -> "true",
                                          "content" -> "con")).fold(
            badForm =>
              Redirect("/"),
            entryData => {
              Redirect("/")
            })
          saveEntryForm.bindFromRequest.fold(
            badForm => Ok(views.html.saveEntry(Some(user), if (req.method == "POST") badForm.errors else Nil, badForm.data)),
            entryData => {
              val tags = dao.getTags(entryData.tags.split(",").filter("""^[\w \-]+$""".r.findFirstIn(_).isDefined))
              if (entryData.id == -1) {
                val newEntry = dao.addEntry(user, entryData.title, tags, entryData.openForAll, entryData.content)
                Redirect(routes.Application.entry(newEntry.id))
              } else {
                dao.getEntry(Some(user), entryData.id) match {
                  case Some(x) =>
                    dao.updateEntry(Some(user), entryData.id, entryData.title, tags, entryData.openForAll, entryData.content)
                    Redirect("/")
                  case None =>
                    Redirect("/")
                }
              }
            }
          )
        }
      case None => Redirect("/")
    }
  }

  def deleteEntry() = Action { implicit req =>
    Redirect("/")
  }

  def user(id: Long) = Action { implicit req =>
    val currentUser = getUserFromSession
    val user = dao.getUser(id)
    renderOption(user) { x => Ok(views.html.user(currentUser, x)) }
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
                case t: NumberFormatException => Redirect("/")
              }
            case None => Redirect("/")
          },
          commentData => {
            dao.getEntry(Some(user), commentData.entryId) match {
              case Some(entry) =>
                val newComment = dao.addComment(user, entry, commentData.content)
                Redirect(routes.Application.entry(commentData.entryId, newComment.id))
              case None =>
                Redirect("/")
            }
          }
        )
      case None => Redirect("/")
    }
  }

  private def renderOption[A](o: Option[A])(r: A => Result): Result = o match {
    case Some(x) => r(x)
    case None => NotFound(views.html.notFound())
  }

  private def getUserFromSession(implicit dao: Dao, req: Request[AnyContent]) = {
    val userId = Integer.parseInt(req.session.get("user").getOrElse("-1"))
    dao.getUser(userId)
  }

  case class LoginData(name: String, password: String)
  case class RegisterData(name: String, password: String, password2: String) {
    def this(badData: Map[String, String]) =
      this(badData.get("name").getOrElse(""), badData.get("pass").getOrElse(""), badData.get("pass2").getOrElse(""))

    def validate: Seq[FormError] = {
      var errors = List[FormError]()
      if (dao.getUser(name).isDefined)
        errors = FormError("name", "user already exists") :: errors
      if (password != password2)
        errors = FormError("pass2", "password mismatch") :: errors
      errors
    }

    def toMap: Map[String, String] = {
      Map("name" -> name, "pass" -> password, "pass2" -> password2)
    }
  }
  case class EntryData(id: Long, title: String, tags: String, openForAll: Boolean, content: String)
  case class CommentData(entryId: Long, content: String)
}