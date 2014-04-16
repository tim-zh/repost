package controllers

import play.api.mvc._
import models.Dao
import play.api.data._
import play.api.data.Forms._
import models.test.TestDao

object Application extends Controller {

  implicit val dao = TestDao
  val itemsOnPage = 2

  case class LoginData(name: String, password: String)

  def index(page: Int) = Action { implicit req =>
    val user = getUserFromSession
    val (pagesNumber, entries) = dao.getEntries(user, page, itemsOnPage)
    Ok(views.html.entries(user, page, pagesNumber, entries))
  }

  def auth() = Action { implicit req =>
    val loginForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText)(LoginData.apply)(LoginData.unapply))
    loginForm.bindFromRequest.fold(
      badForm => Redirect(routes.Application.index(0)),
      loginData => {
        val user = dao.getUser(loginData.name, loginData.password)
        user match {
          case Some(x) =>
            Redirect(routes.Application.index(0)).withSession(req.session +("user", x.id + ""))
          case None =>
            Redirect(routes.Application.index(0))
        }
      }
    )
  }

  def logout() = Action { implicit req =>
    if (req.method == "POST")
      Redirect(routes.Application.index(0)).withNewSession
    else
      Redirect(routes.Application.index(0))
  }

  def search(page: Int) = Action { implicit req =>
    val searchForm = Form(single("query", text))
    searchForm.bindFromRequest().get match {
      case query: String =>
        val user = getUserFromSession
        val (pagesNumber, entries) = dao.getEntriesBySearch(user, query, 0, Int.MaxValue)
        Ok(views.html.search(user, page, pagesNumber, entries))
      case _ =>
        Redirect(routes.Application.index(0))
    }
  }

  def tag(title: String, page: Int) = Action { implicit req =>
    val user = getUserFromSession
    val tag = dao.getTag(title)
    renderOption(tag) { x =>
      val (pagesNumber, entries) = dao.getEntriesByTag(user, x, page, itemsOnPage)
      Ok(views.html.tag(user, page, pagesNumber, entries, x))
    }
  }

  def entry(id: Long) = Action { implicit req =>
    val user = getUserFromSession
    val entry = dao.getEntry(user, id)
    renderOption(entry) { x => Ok(views.html.entry(user, x)) }
  }

  def user(id: Long) = Action { implicit req =>
    val currentUser = getUserFromSession
    val user = dao.getUser(id)
    renderOption(user) { x => Ok(views.html.user(currentUser, x)) }
  }

  private def renderOption[A](o: Option[A])(r: A => Result): Result = o match {
    case Some(x) => r(x)
    case None => NotFound(views.html.notFound())
  }

  private def getUserFromSession(implicit dao: Dao, req: Request[AnyContent]) = {
    val userId = Integer.parseInt(req.session.get("user").getOrElse("-1"))
    dao.getUser(userId)
  }

}