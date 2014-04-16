package models.squeryl

import org.squeryl.KeyedEntity
import java.util.Date
import org.squeryl.dsl.{OneToMany, ManyToOne}

sealed trait Entity {
  var id: Long = _
  var version: Long = _
}

case class User(name: String,
                password: String) extends KeyedEntity[Long] with Entity with models.User {
  lazy val entries: OneToMany[Entry] = SquerylDao.userEntry.left(this)
  lazy val comments: OneToMany[Comment] = SquerylDao.userComment.left(this)
}

case class Entry(authorId: Long,
                 title: String,
                 content: String,
                 date: Date,
                 openForAll: Boolean,
                 tags: Seq[Tag]) extends KeyedEntity[Long] with Entity with models.Entry {
  lazy val _author: ManyToOne[User] = SquerylDao.userEntry.right(this)
  lazy val _comments: OneToMany[Comment] = SquerylDao.entryComment.left(this)

  def author: models.User = _author.single
  def comments: Iterable[models.Comment] = _comments.toSeq
}

case class Comment(authorId: Long,
                   date: Date,
                   content: String,
                   entryId: Long) extends KeyedEntity[Long] with Entity with models.Comment {
  lazy val _author: ManyToOne[User] = SquerylDao.userComment.right(this)
  lazy val _entry: ManyToOne[Entry] = SquerylDao.entryComment.right(this)

  def author: models.User = _author.single
  def entry: models.Entry = _entry.single
}

case class Tag(title: String) extends KeyedEntity[Long] with Entity with models.Tag {
}

case class Filter(title: String,
                  tags: Seq[Tag],
                  authors: Seq[User],
                  startDate: Option[Date],
                  endDate: Option[Date]) extends KeyedEntity[Long] with Entity with models.Filter {
  def this() = this("", Nil, Nil, Some(new Date), Some(new Date))
}