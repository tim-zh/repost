package controllers

import play.api.mvc._
import models.TestModel
import play.api.data._
import play.api.data.Forms._

object Application extends Controller {

  case class LoginData(name: String, password: String)

  def index = Action {
    implicit req =>
      if (req.method == "POST" && (req.body.asFormUrlEncoded map { _.get("logout") } getOrElse None map { _(0) } getOrElse null) == "1")
        Ok(views.html.index(None)).withNewSession
      else {
        val loginForm = Form(mapping("name" -> nonEmptyText, "pass" -> nonEmptyText)(LoginData.apply)(LoginData.unapply))
        loginForm.bindFromRequest.fold(
          badForm => {
            val user = if (req.session.get("user").getOrElse("") == TestModel.user1.id + "")
              Some(TestModel.user1)
            else
              None
            Ok(views.html.index(user))
          },
          loginData => {
            if (checkUser(loginData.name, loginData.password))
              Ok(views.html.index(Some(TestModel.user1))).withSession(req.session +("user", TestModel.user1.id + ""))
            else
              Ok(views.html.index(None))
          }
        )
      }
  }

  def checkUser(name: String, password: String) = name == TestModel.user1.name && password == TestModel.user1.password

}