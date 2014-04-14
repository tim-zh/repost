package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Session, SessionFactory, Schema}
import java.util.Date
import org.squeryl.adapters.H2Adapter
import play.api.db._
import play.api.Play.current

object SquerylDao extends Schema with Dao {
  val users = table[User]
  val entries = table[Entry]
  val comments = table[Comment]
  val tags = table[Tag]
  val filters = table[Filter]

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

  on(filters)(filter => declare(
    filter.id is (primaryKey, autoIncremented)
  ))

  def init() {
    Class.forName("org.h2.Driver")
    SessionFactory.concreteFactory = Some(() => Session.create(DB.getConnection(), new H2Adapter))
    transaction {
      try {
        create
        users.insert(Seq(TestDao.user1))
        entries.insert(Seq(TestDao.entry1, TestDao.entry2, TestDao.entry3))
        comments.insert(Seq(TestDao.comment1, TestDao.comment2, TestDao.comment3, TestDao.comment4))
        tags.insert(Seq(TestDao.tag1, TestDao.tag2))
      }
      catch {
        case _ =>
          drop
          create
          users.insert(Seq(TestDao.user1))
          entries.insert(Seq(TestDao.entry1, TestDao.entry2, TestDao.entry3))
          comments.insert(Seq(TestDao.comment1, TestDao.comment2, TestDao.comment3, TestDao.comment4))
          tags.insert(Seq(TestDao.tag1, TestDao.tag2))
      }
    }
  }

  def getUser(name: String, password: String): Option[User] = inTransaction {
    from(users)(user =>
      where(user.name === name) select user
    ).headOption
  }

  def getUser(id: Long): Option[User] = inTransaction {
    users.lookup(id)
  }

  def getEntries(user: Option[User], page: Int, itemsOnPage: Int): (Long, Seq[Entry]) = inTransaction {
    val condition = (entry: Entry) =>
      entry.openForAll === true or entry.author.id === (user map { _.id } getOrElse -1L)
    val size = from(entries)(entry =>
      where(condition(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(condition(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq)
  }

  def getTag(title: String): Option[Tag] = inTransaction {
    tags.where(_.title === title).headOption
  }

  def getEntriesByTag(user: Option[User], tag: Tag, page: Int, itemsOnPage: Int): (Long, Seq[Entry]) = inTransaction {
    val condition = (entry: Entry) =>
      entry.openForAll === true or entry.author.id === (user map { _.id } getOrElse -1L) and (entry.tags.contains(tag.id) === true)
    val size = from(entries)(entry =>
      where(condition(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(condition(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq)
  }

  def getEntriesBySearch(user: Option[User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[Entry]) = inTransaction {
    val condition = (entry: Entry) =>
      entry.openForAll === true or entry.author.id === (user map { _.id} getOrElse -1L) and (entry.title like query)
    val size = from(entries)(entry =>
      where(condition(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(condition(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq)
  }

  def getEntry(user: Option[User], id: Long): Option[Entry] = inTransaction {
    entries.lookup(id) filter (entry => entry.openForAll || (user.isDefined && entry.author.id == user.get.id))
  }
}
