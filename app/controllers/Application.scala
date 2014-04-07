package controllers

import play.api.mvc._
import models.{Dao, TestDao}
import play.api.data._
import play.api.data.Forms._

object Application extends Controller {

  case class LoginData(name: String, password: String)

  def index = Action {
    implicit req =>
      implicit val dao = TestDao
      if (req.method == "POST" && (req.body.asFormUrlEncoded map { _.get("logout") } getOrElse None map { _(0) } getOrElse null) == "1")
        Ok(views.html.entries(None, 0, Nil)).withNewSession
      else {
        val loginForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText)(LoginData.apply)(LoginData.unapply))
        loginForm.bindFromRequest.fold(
          badForm => {
            val user = getUserFromSession
            val (pagesNumber, entries) = dao.getEntries(user, null, 0, 3)
            Ok(views.html.entries(user, pagesNumber, entries))
          },
          loginData => {
            val user = dao.getUser(loginData.name, loginData.password)
            user match {
              case Some(x) =>
                val (pagesNumber, entries) = dao.getEntries(user, null, 0, 3)
                Ok(views.html.entries(user, pagesNumber, entries)).withSession(req.session +("user", x.id + ""))
              case None =>
                Ok(views.html.entries(None, 0, Nil))
            }
          }
        )
      }
  }

  def tag(id: Long) = Action {
    implicit req =>
      implicit val dao = TestDao
      val user = getUserFromSession
      val tag = dao.getTag(id)
      val (pagesNumber, entries) = dao.getEntriesByTag(user, tag getOrElse null, 0, 3)
      Ok(views.html.entries(user, pagesNumber, entries, tag map { _.getTitle } getOrElse ""))
  }

  private def getUserFromSession(implicit dao: Dao, req: Request[AnyContent]) = {
    val userId = Integer.parseInt(req.session.get("user").getOrElse("-1"))
    dao.getUser(userId)
  }

}