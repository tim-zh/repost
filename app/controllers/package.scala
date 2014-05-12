import java.text.SimpleDateFormat
import java.util.Date
import models.User
import models.squeryl.SquerylDao
import play.api.data._
import play.api.mvc._
import play.cache.Cache
import scala.collection.immutable.TreeMap
import scala.collection.mutable

package object controllers {
  val dao = SquerylDao
  val codeThemeMap = TreeMap(0 -> "dark", 1 -> "github", 2 -> "google code", 3 -> "idea", 4 -> "ir black",
    5 -> "monokai", 6 -> "monokai sublime", 7 -> "obsidian", 8 -> "vs", 9 -> "xcode")

  def getUserFromSession(implicit req: Request[_]) = {
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
    val escapeMap = new mutable.HashMap[String, String]
    escapeMap.put("<", "&lt;")
    escapeMap.put(">", "&gt;")
    val bbMap = new mutable.HashMap[String, String]
    bbMap.put("""(\r\n|\r|\n|\n\r)""", "<br/>")
    bbMap.put("""\[b\]([\s\S]+?)\[/b\]""", "<strong>$1</strong>")
    bbMap.put("""\[i\]([\s\S]+?)\[/i\]""", "<i>$1</i>")
    bbMap.put("""\[u\]([\s\S]+?)\[/u\]""", "<u>$1</u>")
    bbMap.put("""\[s\]([\s\S]+?)\[/s\]""", "<s>$1</s>")
    bbMap.put("""\[size=(\S+?)\]([\s\S]+?)\[/size\]""", "<font size=$1>$2</font>")
    bbMap.put("""\[url\](\S+?)\[/url\]""", "<a href='$1'>$1</a>")
    bbMap.put("""\[url=(\S+?)\]([\s\S]+?)\[/url\]""", "<a href='$1'>$2</a>")
    bbMap.put("""\[img\](\S+?)\[/img\]""", "<img src='$1'/>")
    bbMap.put("""\[img=(\d*?),(\d*?)\](\S+?)\[/img\]""", "<img width='$1' height='$2' src='$3'/>")
    bbMap.put("""\[quote\]([\s\S]+?)\[/quote\]""", "<blockquote>$1</blockquote>")
    bbMap.put("""\[ol\]([\s\S]+?)\[/ol\]""", "<ol>$1</ol>")
    bbMap.put("""\[li\]([\s\S]+?)\[/li\]""", "<li>$1</li>")
    bbMap.put("""\[center\]([\s\S]+?)\[/center\]""", "<div align='center'>$1</div>")
    bbMap.put("""\[code=(\S*?)\]([\s\S]+?)\[/code\]""", "<pre><code class='$1'>$2</code></pre>")
    bbMap.put("""\[p\]([\s\S]+?)\[/p\]""", "<p>$1</p>")

    escapeMap.foreach(entry => html = html.replaceAll(entry._1, entry._2))
    bbMap.foreach(entry => html = html.replaceAll(entry._1, entry._2))
    html
  }

  def getItemsOnPage(user: Option[User]) = user.map(_.itemsOnPage).getOrElse(dao.defaultItemsOnPage)

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
  case class UserData(oldPass: String, newPass: String, newPass2: String, compactEntryList: Boolean,
                      dateFormat: String, itemsOnPage: Int, codeTheme: Int) {
    def this(badData: Map[String, String]) =
      this(badData.get("oldPass").getOrElse(""), badData.get("newPass").getOrElse(""),
          badData.get("newPass2").getOrElse(""), badData.get("compactEntryList").getOrElse("") == "true",
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
      }
      catch {
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
      Map("oldPass" -> oldPass, "newPass" -> newPass, "newPass2" -> newPass2, "compactEntryList" -> compactEntryList.toString,
        "dateFormat" -> dateFormat, "itemsOnPage" -> itemsOnPage.toString, "codeTheme" -> codeTheme.toString)
    }
  }
}