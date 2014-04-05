package controllers

import play.api.mvc._
import models.TestModel

object Application extends Controller {

  def index = Action {
    Ok(views.html.index(Some(TestModel.user1)))
  }

}