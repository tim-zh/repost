package controllers

import play.api.mvc._
import models.{Dao, TestDao}
import play.api.data._
import play.api.data.Forms._

object Application extends Controller {

  case class LoginData(name: String, password: String)

  def index = Action { implicit req =>
      implicit val dao = TestDao
      if (req.method == "POST" && (req.body.asFormUrlEncoded map { _.get("logout") } getOrElse None map { _(0) } getOrElse null) == "1")
        Ok(views.html.entries(None, 0, 0, Nil)).withNewSession
      else {
        val loginForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText)(LoginData.apply)(LoginData.unapply))
        loginForm.bindFromRequest.fold(
          badForm => {
            val user = getUserFromSession
            val (pagesNumber, entries) = dao.getEntries(user, null, 0, 3)
            Ok(views.html.entries(user, 1, pagesNumber, entries))
          },
          loginData => {
            val user = dao.getUser(loginData.name, loginData.password)
            user match {
              case Some(x) =>
                val (pagesNumber, entries) = dao.getEntries(user, null, 0, 3)
                Ok(views.html.entries(user, 1, pagesNumber, entries)).withSession(req.session +("user", x.id + ""))
              case None =>
                Ok(views.html.entries(None, 0, 0, Nil))
            }
          }
        )
      }
  }

  def tag(id: Long) = Action { implicit req =>
    implicit val dao = TestDao
    val user = getUserFromSession
    val tag = dao.getTag(id)
    renderOption(tag) { x =>
      val (pagesNumber, entries) = dao.getEntriesByTag(user, x, 0, 3)
      Ok(views.html.entries(user, 1, pagesNumber, entries, x.getTitle))
    }
  }

  def entry(id: Long) = Action { implicit req =>
    implicit val dao = TestDao
    val user = getUserFromSession
    val entry = dao.getEntry(id)
    renderOption(entry) { x => Ok(views.html.entry(user, x)) }
  }

  private def renderOption[A](o: Option[A])(r: A => Result): Result = o match {
    case Some(x) => r(x)
    case None => BadRequest(views.html.badRequest())
  }

  private def getUserFromSession(implicit dao: Dao, req: Request[AnyContent]) = {
    val userId = Integer.parseInt(req.session.get("user").getOrElse("-1"))
    dao.getUser(userId)
  }

}