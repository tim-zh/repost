import com.ning.http.util.AsyncHttpProviderUtils
import java.io._
import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date
import models.User
import models.squeryl.SquerylDao
import play.api.data.FormError
import play.api.libs.iteratee.{Done, Input, Cont, Iteratee}
import play.api.libs.ws.WS
import play.api.mvc._
import play.cache.Cache
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.concurrent.Future

package object controllers {
  val dao = SquerylDao
  val codeThemeMap = TreeMap(0 -> "dark", 1 -> "github", 2 -> "google code", 3 -> "idea", 4 -> "ir black",
    5 -> "monokai", 6 -> "monokai sublime", 7 -> "obsidian", 8 -> "vs", 9 -> "xcode")
  final val defaultDateFormat = "dd MMM yyyy HH:mm:ss"
  val maxMessageNumber = 50

  def now = new Timestamp((new java.util.Date).getTime)

  def getUserFromSession(implicit req: RequestHeader) = {
    val sessionId = req.session.get("user").getOrElse("-1")
    val user = Cache.get(sessionId)
    if (user == null) None else Some(user.asInstanceOf[User])
  }

  def renderOption[A](o: Option[A])(r: A => Result): Result = o match {
    case Some(x) =>
      r(x)
    case None =>
      Results.NotFound(views.html.notFound())
  }

  def getHtmlFromBbCodeAndEscape(text: String): String = {
    var html: String = text
    val bbMap = new mutable.HashMap[String, String]
    bbMap.put("""(\r\n|\r|\n|\n\r)""", "<br/>")
    bbMap.put("""(?s)\[b\](.+?)\[/b\]""", "<strong>$1</strong>")
    bbMap.put("""(?s)\[i\](.+?)\[/i\]""", "<i>$1</i>")
    bbMap.put("""(?s)\[u\](.+?)\[/u\]""", "<u>$1</u>")
    bbMap.put("""(?s)\[s\](.+?)\[/s\]""", "<s>$1</s>")
    bbMap.put("""(?s)\[size=(\d+?)\](.+?)\[/size\]""", "<font size=$1>$2</font>")
    bbMap.put("""\[url\]([\S&&[^']]+?)\[/url\]""", "<a href='$1'>$1</a>")
    bbMap.put("""(?s)\[url=([\S&&[^']]+?)\](.+?)\[/url\]""", "<a href='$1'>$2</a>")
    bbMap.put("""\[img\]([\S&&[^']]+?)\[/img\]""", "<img src='$1'/>")
    bbMap.put("""\[img=(\d*?),(\d*?)\]([\S&&[^']]+?)\[/img\]""", "<img width='$1' height='$2' src='$3'/>")
    bbMap.put("""(?s)\[quote\](.+?)\[/quote\]""", "<blockquote>$1</blockquote>")
    bbMap.put("""(?s)\[ol\](.+?)\[/ol\]""", "<ol>$1</ol>")
    bbMap.put("""(?s)\[li\](.+?)\[/li\]""", "<li>$1</li>")
    bbMap.put("""(?s)\[center\](.+?)\[/center\]""", "<div align='center'>$1</div>")
    bbMap.put("""(?s)\[code=([\S&&[^']]*?)\](.+?)\[/code\]""", "<pre><code class='$1'>$2</code></pre>")
    bbMap.put("""(?s)\[p\](.+?)\[/p\]""", "<p>$1</p>")

    html = escape(html)
    var temp = ""
    while (temp != html) {
      temp = html
      bbMap.foreach(entry => html = html.replaceAll(entry._1, entry._2))
    }
    html
  }

  def getItemsOnPage(user: Option[User]) = {
    if (user.isDefined) {
      if (user.get.entryListType == models.ListType.oneByOne)
        1
      else
        user.get.itemsOnPage
    } else
      dao.defaultItemsOnPage
  }

  def getSafeSeqFromString(s: String): Seq[String] = s.split(",").filter("""^[\w \-]+$""".r.findFirstIn(_).isDefined)

  def escape(s: String) = s.replaceAll("<", "&lt;").replaceAll("'", "&#39;")

  @throws(classOf[IOException])
  def createImageFile(originalFilename: String) = {
    val filename = util.Random.nextLong().toHexString + """(\.\w{1,5})$""".r.findFirstIn(originalFilename).getOrElse("")
    val file = new File("public/images/uploaded/", filename)
    if (!file.getParentFile.mkdirs() && !file.createNewFile())
      throw new IOException()
    file
  }

  def saveImage(url: String, user: Option[User]): Future[Option[String]] = {
    def fromStream(stream: OutputStream): Iteratee[Array[Byte], Unit] = Cont {
      case e @ Input.EOF =>
        stream.close()
        Done((), e)
      case Input.El(data) =>
        stream.write(data)
        fromStream(stream)
      case Input.Empty =>
        fromStream(stream)
    }

    var result: Future[Option[String]] = Future(None)
    if (user.isDefined) {
      var filename = ""
      try {
        AsyncHttpProviderUtils.validateSupportedScheme(AsyncHttpProviderUtils.createUri(url))
        val file = createImageFile(url)
        filename = file.getName
        val outputStream = new BufferedOutputStream(new FileOutputStream(file.getAbsoluteFile))
        result = WS.url(url).withFollowRedirects(true).withRequestTimeout(30000).get(headers => fromStream(outputStream)).flatMap(_.run).map {
          _ => if (filename.isEmpty) None else Some("/assets/images/uploaded/" + filename)
        }
      } catch {
        case e @ (_: IOException | _: SecurityException | _: IllegalArgumentException) =>
          e.printStackTrace()
          filename = ""
      }
    }
    result
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
  case class UserData(oldPass: String, newPass: String, newPass2: String, entryListType: Int,
                      dateFormat: String, itemsOnPage: Int, codeTheme: Int) {
    def this(badData: Map[String, String]) =
      this(badData.get("oldPass").getOrElse(""),
          badData.get("newPass").getOrElse(""),
          badData.get("newPass2").getOrElse(""),
          try { Integer.parseInt(badData.get("itemsOnPage").getOrElse(""))} catch { case _: NumberFormatException => -1},
          badData.get("dateFormat").getOrElse(""),
          try { Integer.parseInt(badData.get("itemsOnPage").getOrElse(""))} catch { case _: NumberFormatException => -1},
          try { Integer.parseInt(badData.get("codeTheme").getOrElse(""))} catch { case _: NumberFormatException => -1})

    def validate(userPass: String): Seq[FormError] = {
      var errors = List[FormError]()
      if (!oldPass.isEmpty && oldPass != userPass)
        errors = FormError("oldPass", "wrong password") :: errors
      if (newPass != newPass2)
        errors = FormError("pass2", "password mismatch") :: errors
      try {
        new SimpleDateFormat(dateFormat).format(new Date)
      } catch {
        case t: IllegalArgumentException =>
          errors = FormError("dateFormat", "incorrect date format") :: errors
      }
      if (itemsOnPage < 1 || itemsOnPage > 50)
        errors = FormError("itemsOnPage", "should be >0 and <=50") :: errors
      if (codeTheme < 0 || codeTheme > codeThemeMap.size - 1)
        errors = FormError("codeTheme", "incorrect code block theme") :: errors
      errors
    }

    def toMap: Map[String, String] = {
      Map("oldPass" -> oldPass, "newPass" -> newPass, "newPass2" -> newPass2, "entryListType" -> entryListType.toString,
        "dateFormat" -> dateFormat, "itemsOnPage" -> itemsOnPage.toString, "codeTheme" -> codeTheme.toString)
    }
  }
  case class SearchData(query: String, isEntries: Boolean, isTags: Boolean, from: Option[Date], to: Option[Date], users: String, tags: String) {
    def toUrlString = {
      "query=" + query + "&isEntries=" + isEntries + "&isTags=" + isTags +
        "&from=" + from.map(_.formatted("yyyy-MM-dd")).getOrElse("") +
        "&to=" + to.map(_.formatted("yyyy-MM-dd")).getOrElse("") +
        "&users=&usersHiddenString=" + users + "&tags=&tagsHiddenString=" + tags
    }
  }
  case class ImportData(title: String, openForAll: Boolean, tags: String, startText: String, endText: String, texts: String, separator: String)
}
