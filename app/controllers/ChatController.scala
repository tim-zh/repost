package controllers

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import play.api.libs.concurrent.Akka
import play.api.libs.iteratee._
import play.api.libs.json._
import play.api.mvc._
import play.api.Play.current
import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object ChatController extends Controller {
  implicit val timeout = Timeout(1 second)
  val (out, channel) = Concurrent.broadcast[JsValue]
  val tagActors = new mutable.HashMap[String, ActorRef]()

  private def processInput(jsMessage: JsValue, user: Option[models.User]) {
    if (user.isDefined) {
      val tagTitle = (jsMessage \ "tag").as[String]
      val message = user.get.name + ": " + escape((jsMessage \ "text").as[String])
      if (!tagActors.contains(tagTitle))
        tagActors.put(tagTitle, Akka.system.actorOf(Props[TagActor]))
      tagActors(tagTitle) ! AddMessage(message)
      channel push JsObject(Seq("text" -> JsString(message)))
    }
  }

  def connect() = WebSocket.using[JsValue] { implicit req =>
    val user = getUserFromSession
    val inProcessor = Iteratee.foreach[JsValue](processInput(_, user))
    (inProcessor, out)
  }

  //ajax call from chat init in wrapper
  def history(tagTitle: String) = Action.async { implicit req =>
    if (tagActors.contains(tagTitle)) {
      tagActors(tagTitle) ? GetMessages() map {
        case s: String => Ok(s)
      }
    } else
      Future(Ok(""))
  }

  case class GetMessages()
  case class AddMessage(message: String)

  class TagActor extends Actor {
    val messages = new mutable.Queue[String]()

    def receive = {
      case GetMessages() => {
        val sb = new mutable.StringBuilder()
        messages.foreach(s => sb append s append "<br/>")
        sender ! sb.toString
      }
      case AddMessage(message) => {
        if (messages.length == maxMessageNumber)
          messages dequeue()
        messages enqueue message
      }
    }
  }
}