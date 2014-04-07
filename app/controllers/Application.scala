package controllers

import play.api.mvc._
import models.TestDao
import play.api.data._
import play.api.data.Forms._

object Application extends Controller {

  case class LoginData(name: String, password: String)

  def index = Action {
    implicit req =>
      val dao = TestDao
      if (req.method == "POST" && (req.body.asFormUrlEncoded map { _.get("logout") } getOrElse None map { _(0) } getOrElse null) == "1")
        Ok(views.html.entries(None, 0, Nil)).withNewSession
      else {
        val loginForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText)(LoginData.apply)(LoginData.unapply))
        loginForm.bindFromRequest.fold(
          badForm => {
            val userId = Integer.parseInt(req.session.get("user").getOrElse("-1"))
            val user = dao.getUser(userId)
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

}