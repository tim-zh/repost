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
                password: String) extends KeyedEntity[Long] with Entity with models.User {
  lazy val _entries: OneToMany[Entry] = SquerylDao.userEntry.left(this)
  lazy val _comments: OneToMany[Comment] = SquerylDao.userComment.left(this)

  lazy val entries: Iterable[models.Entry] = inTransaction(_entries.toList)
  lazy val comments: Iterable[models.Comment] = inTransaction(_comments.toList)
}

case class Entry(authorId: Long,
                 title: String,
                 content: String,
                 date: Date,
                 openForAll: Boolean) extends KeyedEntity[Long] with Entity with models.Entry {
  lazy val _author: ManyToOne[User] = SquerylDao.userEntry.right(this)
  lazy val _comments: OneToMany[Comment] = SquerylDao.entryComment.left(this)
  lazy val _tags = SquerylDao.entryTag.left(this)

  lazy val author: models.User = inTransaction(_author.single)
  lazy val comments: Iterable[models.Comment] = inTransaction(_comments.toList)
  lazy val tags: Iterable[models.Tag] = inTransaction(_tags.toList)
}

case class Comment(authorId: Long,
                   date: Date,
                   content: String,
                   entryId: Long) extends KeyedEntity[Long] with Entity with models.Comment {
  lazy val _author: ManyToOne[User] = SquerylDao.userComment.right(this)
  lazy val _entry: ManyToOne[Entry] = SquerylDao.entryComment.right(this)

  lazy val author: models.User = inTransaction(_author.single)
  lazy val entry: models.Entry = inTransaction(_entry.single)
}

case class Tag(title: String) extends KeyedEntity[Long] with Entity with models.Tag {
  lazy val _entries = SquerylDao.entryTag.right(this)
}