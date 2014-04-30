package models.squeryl

import org.squeryl.KeyedEntity
import java.util.Date
import org.squeryl.dsl.{OneToMany, ManyToOne}
import org.squeryl.PrimitiveTypeMode._

sealed trait Entity {
  var id: Long = _
  var version: Long = _
}

case class User(name: String,
                password: String,
                compactEntryList: Boolean,
                dateFormat: String,
                itemsOnPage: Int,
                codeTheme: Int) extends KeyedEntity[Long] with Entity with models.User {
  lazy val _entries: OneToMany[Entry] = SquerylDao.userEntry.left(this)
  lazy val _comments: OneToMany[Comment] = SquerylDao.userComment.left(this)
  lazy val _favoriteTags = SquerylDao.userFavoriteTag.left(this)

  def entries: Iterable[models.Entry] = inTransaction(_entries.toList)
  def comments: Iterable[models.Comment] = inTransaction(_comments.toList)
  def favoriteTags: Iterable[models.Tag] = inTransaction(_favoriteTags.toList)
}

case class Entry(authorId: Long,
                 title: String,
                 content: String,
                 date: Date,
                 openForAll: Boolean) extends KeyedEntity[Long] with Entity with models.Entry {
  lazy val _author: ManyToOne[User] = SquerylDao.userEntry.right(this)
  lazy val _comments: OneToMany[Comment] = SquerylDao.entryComment.left(this)
  lazy val _tags = SquerylDao.entryTag.left(this)

  def author: models.User = inTransaction(_author.single)
  def comments: Iterable[models.Comment] = inTransaction(_comments.toList)
  def tags: Iterable[models.Tag] = inTransaction(_tags.toList)
}

case class Comment(authorId: Long,
                   date: Date,
                   content: String,
                   entryId: Long) extends KeyedEntity[Long] with Entity with models.Comment {
  lazy val _author: ManyToOne[User] = SquerylDao.userComment.right(this)
  lazy val _entry: ManyToOne[Entry] = SquerylDao.entryComment.right(this)

  def author: models.User = inTransaction(_author.single)
  def entry: models.Entry = inTransaction(_entry.single)
}

case class Tag(title: String) extends KeyedEntity[Long] with Entity with models.Tag {
  lazy val _users = SquerylDao.userFavoriteTag.right(this)
  lazy val _entries = SquerylDao.entryTag.right(this)
}