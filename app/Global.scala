import controllers.dao
import play.api._
import play.api.Application
import play.api.libs.concurrent.Akka
import play.api.mvc._
import play.api.mvc.Results._
import play.api.mvc.SimpleResult
import scala.concurrent.duration.DurationInt
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Global extends GlobalSettings {
  override def onHandlerNotFound(request: RequestHeader): Future[SimpleResult] = {
    Logger.info("handler not found for: " + request.path)
    scala.concurrent.Future(NotFound(views.html.notFound()))
  }

  override def onStart(app: Application) {
    dao.init()
    Akka.system(app).scheduler.schedule(0.seconds, app.configuration.getInt("mem.monitor.interval").getOrElse(30).seconds) {
      Logger.info("free memory: " + Runtime.getRuntime.freeMemory / 1024 / 1024 + " Mb")
    }
  }
}