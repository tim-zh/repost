package models.squeryl

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Session, SessionFactory, Schema}
import java.util.Date
import org.squeryl.adapters.H2Adapter
import play.api.db.DB
import play.api.Play.current
import models.Dao

object SquerylDao extends Schema with Dao {
  val users = table[User]
  val entries = table[Entry]
  val comments = table[Comment]
  val tags = table[Tag]

  val userEntry = oneToManyRelation(users, entries).via((u, e) => u.id === e.authorId)
  val userComment = oneToManyRelation(users, comments).via((u, c) => u.id === c.authorId)
  val entryComment = oneToManyRelation(entries, comments).via((e, c) => e.id === c.entryId)

  def isEntryVisible(entry: Entry)(implicit user: Option[models.User]) =
    entry.openForAll === true or entry.authorId === (user map (_.id) getOrElse -1L)

  on(users)(user => declare(
    user.id is (primaryKey, autoIncremented),
    columns(user.name, user.password) are indexed,
    user.name is unique
  ))

  on(entries)(entry => declare(
    entry.id is (primaryKey, autoIncremented),
    entry.title is indexed
  ))

  on(comments)(comment => declare(
    comment.id is (primaryKey, autoIncremented),
    comment.date defaultsTo new Date
  ))

  on(tags)(tag => declare(
    tag.id is (primaryKey, autoIncremented),
    tag.title is indexed
  ))

  def init() {
    Class.forName("org.h2.Driver")
    SessionFactory.concreteFactory = Some(() => Session.create(DB.getConnection(), new H2Adapter))
    transaction {
      try {
        create
        populate()
      }
      catch {
        case _ =>
          drop
          create
          populate()
      }
    }
  }

  def populate() {
    val tag1 = Tag("tag1")
    tag1.id = 1
    val tag2 = Tag("tag2")
    tag2.id = 2
    val user1 = User("user1", "pass")
    user1.id = 1
    val entry1 = Entry(1, "entry1", "content1<br/>bla1", new Date, true, Seq(tag1, tag2))
    entry1.id = 1
    val entry2 = Entry(1, "entry2", "content2<br/>bla2", new Date, false, Seq(tag1))
    entry2.id = 2
    val entry3 = Entry(1, "entry3", "content3<br/>bla3", new Date, true, Seq(tag2))
    entry3.id = 3
    val comment1 = Comment(1, new Date, "comment1<br/>c1", 1)
    val comment2 = Comment(1, new Date, "comment2<br/>c2", 1)
    val comment3 = Comment(1, new Date, "comment3<br/>c3", 1)
    val comment4 = Comment(1, new Date, "comment4<br/>c4", 2)

    users.insert(Seq(
      user1))
    entries.insert(Seq(
      entry1,
      entry2,
      entry3))
    comments.insert(Seq(
      comment1,
      comment2,
      comment3,
      comment4))
    tags.insert(Seq(
      tag1,
      tag2))
  }

  def getUser(name: String, password: String): Option[models.User] = inTransaction {
    from(users)(user =>
      where(user.name === name) select user
    ).headOption
  }

  def getUser(id: Long): Option[models.User] = inTransaction {
    users.lookup(id)
  }

  def getEntries(user: Option[models.User], page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    implicit val impUser = user
    val size = from(entries)(entry =>
      where(isEntryVisible(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(isEntryVisible(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq)
  }

  def getTag(title: String): Option[models.Tag] = inTransaction {
    tags.where(_.title === title).headOption
  }

  def getEntriesByTag(user: Option[models.User], tag: models.Tag, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    implicit val impUser = user
    val size = from(entries)(entry =>
      where(isEntryVisible(entry) and (entry.tags.contains(tag.id) === true)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(isEntryVisible(entry) and (entry.tags.contains(tag.id) === true)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq)
  }

  def getEntriesBySearch(user: Option[models.User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    implicit val impUser = user
    val size = from(entries)(entry =>
      where(isEntryVisible(entry) and (entry.title like query)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(isEntryVisible(entry) and (entry.title like query)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq)
  }

  def getEntry(user: Option[models.User], id: Long): Option[models.Entry] = inTransaction {
    entries.lookup(id) filter (entry => entry.openForAll || (user.isDefined && entry.authorId == user.get.id))
  }
}
