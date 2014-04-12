package models.slick

import scala.slick.driver.H2Driver.simple._
import java.sql.Date
import scala.slick.lifted

class User(ltag: lifted.Tag) extends Table[(Long, Long, String, String)](ltag, "users") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def name = column[String]("name")
  def password = column[String]("password")
  def * = (id, version, name, password)
  def nameUnique = index("name_idx", name, unique = true)
  def namePasswordIdx = index("name_password_idx", (name, password), unique = true)
}

class Entry(ltag: lifted.Tag) extends Table[(Long, Long, Long, String, String, Boolean)](ltag, "entries") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def author = column[Long]("author")
  def title = column[String]("title")
  def content = column[String]("content")
  def openForAll = column[Boolean]("open_for_all")
  def * = (id, version, author, title, content, openForAll)
  def titleIdx = index("title_idx", title)
  def authorFk = foreignKey("entry_author_fk", author, SlickDao.users)(_.id)
}

class Comment(ltag: lifted.Tag) extends Table[(Long, Long, Long, Date, String, Long)](ltag, "comments") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def author = column[Long]("author")
  def date = column[Date]("date", O.Default(new Date((new java.util.Date).getTime)))
  def content = column[String]("content")
  def entry = column[Long]("entry")
  def * = (id, version, author, date, content, entry)
  def authorFk = foreignKey("comment_author_fk", author, SlickDao.users)(_.id)
  def entryFk = foreignKey("comment_entry_fk", entry, SlickDao.entries)(_.id)
}

class Tag(ltag: lifted.Tag) extends Table[(Long, Long, String)](ltag, "tags") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def title = column[String]("title")
  def * = (id, version, title)
}

class Filter(ltag: lifted.Tag) extends Table[(Long, Long, String, Option[Date], Option[Date])](ltag, "filters") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def version = column[Long]("version", O.Default(0))
  def title = column[String]("title")
  def startDate = column[Option[Date]]("start_date")
  def endDate = column[Option[Date]]("end_date")
  def * = (id, version, title, startDate, endDate)
}

class EntryTag(ltag: lifted.Tag) extends Table[(Long, Long)](ltag, "entries_tags") {
  def entry = column[Long]("entry_id")
  def tag = column[Long]("tag_id")
  def * = (entry, tag)
  def entryFk = foreignKey("entry_tag_entry_fk", entry, SlickDao.entries)(_.id)
  def tagFk = foreignKey("entry_tag_tag_fk", tag, SlickDao.tags)(_.id)
}

class FilterUser(ltag: lifted.Tag) extends Table[(Long, Long)](ltag, "filters_users") {
  def filter = column[Long]("filter_id")
  def user = column[Long]("user_id")
  def * = (filter, user)
  def filterFk = foreignKey("filter_user_entry_fk", filter, SlickDao.filters)(_.id)
  def userFk = foreignKey("filter_user_user_fk", user, SlickDao.users)(_.id)
}