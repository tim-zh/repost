package models.slick

import scala.slick.driver.H2Driver.simple._
import java.sql.Date
import scala.slick.lifted

class User(ltag: lifted.Tag) extends Table[(Long, Long, String, String, Boolean, String, Int, Int)](ltag, "users") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def name = column[String]("name")
  def password = column[String]("password")
  def compactEntryList = column[Boolean]("compact_entry_list", O.Default(false))
  def dateFormat = column[String]("date_format", O.Default("dd MMM yyyy hh:mm:ss"))
  def itemsOnPage = column[Int]("items_on_page", O.Default(SlickDao.defaultItemsOnPage))
  def codeTheme = column[Int]("code_theme", O.Default(0))
  def * = (id, version, name, password, compactEntryList, dateFormat, itemsOnPage, codeTheme)
  def nameIdx = index("idx_user_name", name, unique = true)
  def namePasswordIdx = index("idx_user_name_password", (name, password), unique = true)
}

class Entry(ltag: lifted.Tag) extends Table[(Long, Long, Long, String, String, Date, Boolean)](ltag, "entries") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def author = column[Long]("author_id")
  def title = column[String]("title")
  def content = column[String]("content", O.DBType("varchar(40960)"))
  def date = column[Date]("date", O.Default(new Date((new java.util.Date).getTime)))
  def openForAll = column[Boolean]("open_for_all")
  def * = (id, version, author, title, content, date, openForAll)
  def titleIdx = index("idx_entry_title", title)
  def authorFk = foreignKey("fk_entry_author", author, SlickDao.users)(_.id)
}

class Comment(ltag: lifted.Tag) extends Table[(Long, Long, Long, Date, String, Long)](ltag, "comments") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def author = column[Long]("author_id")
  def date = column[Date]("date", O.Default(new Date((new java.util.Date).getTime)))
  def content = column[String]("content", O.DBType("varchar(4096)"))
  def entry = column[Long]("entry_id")
  def * = (id, version, author, date, content, entry)
  def authorFk = foreignKey("fk_comment_author", author, SlickDao.users)(_.id)
  def entryFk = foreignKey("fk_comment_entry", entry, SlickDao.entries)(_.id)
}

class Tag(ltag: lifted.Tag) extends Table[(Long, Long, String)](ltag, "tags") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def title = column[String]("title")
  def * = (id, version, title)
  def titleIdx = index("idx_tag_title", title)
}

class UserFavoriteTag(ltag: lifted.Tag) extends Table[(Long, Long)](ltag, "user_tag") {
  def user = column[Long]("userid")
  def tag = column[Long]("tagid")
  def * = (user, tag)
  def userFk = foreignKey("fk_user_favorite_tag_user", user, SlickDao.users)(_.id)
  def tagFk = foreignKey("fk_user_favorite_tag_tag", tag, SlickDao.tags)(_.id)
}

class EntryTag(ltag: lifted.Tag) extends Table[(Long, Long)](ltag, "entry_tag") {
  def entry = column[Long]("entryid")
  def tag = column[Long]("tagid")
  def * = (entry, tag)
  def entryFk = foreignKey("fk_entry_tag_entry", entry, SlickDao.entries)(_.id)
  def tagFk = foreignKey("fk_entry_tag_tag", tag, SlickDao.tags)(_.id)
}

sealed abstract class Entity {
  var id: Long = _
  var version: Long = _
}

case class SlickUser(name: String,
                     password: String,
                     compactEntryList: Boolean,
                     dateFormat: String,
                     itemsOnPage: Int,
                     codeTheme: Int) extends Entity with models.User {
  def entries: Seq[models.Entry] = SlickDao.getEntriesByUser(id)
  def comments: Seq[models.Comment] = SlickDao.getCommentsByUser(id)
  def favoriteTags: Seq[models.Tag] = SlickDao.getFavoriteTagsByUser(id)
}

case class SlickEntry(authorId: Long,
                      title: String,
                      content: String,
                      date: Date,
                      openForAll: Boolean) extends Entity with models.Entry {
  def author: models.User = SlickDao.getUser(authorId).get
  def tags: Seq[models.Tag] = SlickDao.getTagsByEntry(id)
  def comments: Seq[models.Comment] = SlickDao.getCommentsByEntry(id)
}

case class SlickComment(authorId: Long,
                        date: Date,
                        content: String,
                        entryId: Long) extends Entity with models.Comment {
  def author: models.User = SlickDao.getUser(authorId).get
  def entry: models.Entry = SlickDao.getEntry(entryId).get
}

case class SlickTag(title: String) extends Entity with models.Tag

object ModelConverter {
  def getUser(t: (Long, Long, String, String, Boolean, String, Int, Int)): SlickUser = {
    val x = SlickUser(t._3, t._4, t._5, t._6, t._7, t._8)
    x.id = t._1
    x.version = t._2
    x
  }

  def getEntry(t: (Long, Long, Long, String, String, Date, Boolean)): SlickEntry = {
    val x = SlickEntry(t._3, t._4, t._5, t._6, t._7)
    x.id = t._1
    x.version = t._2
    x
  }

  def getComment(t: (Long, Long, Long, Date, String, Long)): SlickComment = {
    val x = SlickComment(t._3, t._4, t._5, t._6)
    x.id = t._1
    x.version = t._2
    x
  }

  def getTag(t: (Long, Long, String)): SlickTag = {
    val x = SlickTag(t._3)
    x.id = t._1
    x.version = t._2
    x
  }
}