package models.squeryl

import org.squeryl.KeyedEntity
import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.dsl.{OneToMany, ManyToOne}
import org.squeryl.PrimitiveTypeMode._

sealed trait Entity {
  var id: Long = _
  var version: Long = _
}

case class User(@Column("name") name: String,
                @Column("password") password: String,
                @Column("compact_entry_list") compactEntryList: Boolean,
                @Column("date_format") dateFormat: String,
                @Column("items_on_page") itemsOnPage: Int,
                @Column("code_theme") codeTheme: Int) extends KeyedEntity[Long] with Entity with models.User {
  lazy val _entries: OneToMany[Entry] = SquerylDao.userEntry.left(this)
  lazy val _comments: OneToMany[Comment] = SquerylDao.userComment.left(this)
  lazy val _favoriteTags = SquerylDao.userFavoriteTag.left(this)

  def entries: Iterable[models.Entry] = inTransaction(_entries.toList)
  def comments: Iterable[models.Comment] = inTransaction(_comments.toList)
  def favoriteTags: Iterable[models.Tag] = inTransaction(_favoriteTags.toList)
}

case class Entry(@Column("author_id") authorId: Long,
                 @Column("title") title: String,
                 @Column("content") content: String,
                 @Column("date") date: Date,
                 @Column("open_for_all") openForAll: Boolean) extends KeyedEntity[Long] with Entity with models.Entry {
  lazy val _author: ManyToOne[User] = SquerylDao.userEntry.right(this)
  lazy val _comments: OneToMany[Comment] = SquerylDao.entryComment.left(this)
  lazy val _tags = SquerylDao.entryTag.left(this)

  def author: models.User = inTransaction(_author.single)
  def comments: Iterable[models.Comment] = inTransaction(_comments.toList)
  def tags: Iterable[models.Tag] = inTransaction(_tags.toList)
}

case class Comment(@Column("author_id") authorId: Long,
                   @Column("date") date: Date,
                   @Column("content") content: String,
                   @Column("entry_id") entryId: Long) extends KeyedEntity[Long] with Entity with models.Comment {
  lazy val _author: ManyToOne[User] = SquerylDao.userComment.right(this)
  lazy val _entry: ManyToOne[Entry] = SquerylDao.entryComment.right(this)

  def author: models.User = inTransaction(_author.single)
  def entry: models.Entry = inTransaction(_entry.single)
}

case class Tag(@Column("title") title: String) extends KeyedEntity[Long] with Entity with models.Tag {
  lazy val _users = SquerylDao.userFavoriteTag.right(this)
  lazy val _entries = SquerylDao.entryTag.right(this)
}