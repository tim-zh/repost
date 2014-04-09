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
    tag.id is (primaryKey, autoIncremented)
  ))

  on(filters)(filter => declare(
    filter.id is (primaryKey, autoIncremented)
  ))

  def init() {
    Class.forName("org.h2.Driver")
    SessionFactory.concreteFactory = Some(() => Session.create(DB.getConnection(), new H2Adapter))
    transaction {
      create
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

  def getEntries(user: Option[User], filter: Filter, page: Int, itemsOnPage: Int): (Long, Seq[Entry]) = inTransaction {
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

  def getTag(id: Long): Option[Tag] = inTransaction {
    tags.lookup(id)
  }

  def getEntriesByTag(user: Option[User], tag: Tag, page: Int, itemsOnPage: Int): (Long, Seq[Entry]) = inTransaction {
    val condition = (entry: Entry) =>
      entry.openForAll === true or entry.author.id === (user map { _.id } getOrElse -1L) and (tag.id in entry.tags)
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

  def getEntry(id: Long): Option[Entry] = inTransaction {
    entries.lookup(id)
  }
}
