package models.squeryl

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Session, SessionFactory, Schema}
import java.util.Date
import org.squeryl.adapters.H2Adapter
import play.api.db._
import play.api.Play.current
import models.{TestDao, Dao}

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
    users.insert(Seq(
      ModelConverter.get(TestDao.user1)))
    entries.insert(Seq(
      ModelConverter.get(TestDao.entry1),
      ModelConverter.get(TestDao.entry2),
      ModelConverter.get(TestDao.entry3)))
    comments.insert(Seq(
      ModelConverter.get(TestDao.comment1),
      ModelConverter.get(TestDao.comment2),
      ModelConverter.get(TestDao.comment3),
      ModelConverter.get(TestDao.comment4)))
    tags.insert(Seq(
      ModelConverter.get(TestDao.tag1),
      ModelConverter.get(TestDao.tag2)))
  }

  def getUser(name: String, password: String): Option[models.User] = inTransaction {
    from(users)(user =>
      where(user.name === name) select user
    ).headOption map ModelConverter.get
  }

  def getUser(id: Long): Option[models.User] = inTransaction {
    users.lookup(id) map ModelConverter.get
  }

  def getEntries(user: Option[User], page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    val condition = (entry: Entry) =>
      entry.openForAll === true or entry.author.id === (user map { _.id } getOrElse -1L)
    val size = from(entries)(entry =>
      where(condition(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(condition(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq map ModelConverter.get)
  }

  def getTag(title: String): Option[models.Tag] = inTransaction {
    tags.where(_.title === title).headOption map ModelConverter.get
  }

  def getEntriesByTag(user: Option[User], tag: Tag, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    val condition = (entry: Entry) =>
      entry.openForAll === true or entry.author.id === (user map { _.id } getOrElse -1L) and (entry.tags.contains(tag.id) === true)
    val size = from(entries)(entry =>
      where(condition(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(condition(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq map ModelConverter.get)
  }

  def getEntriesBySearch(user: Option[User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    val condition = (entry: Entry) =>
      entry.openForAll === true or entry.author.id === (user map { _.id} getOrElse -1L) and (entry.title like query)
    val size = from(entries)(entry =>
      where(condition(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(condition(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (size, xs.toSeq map ModelConverter.get)
  }

  def getEntry(user: Option[User], id: Long): Option[models.Entry] = inTransaction {
    entries.lookup(id) filter (entry => entry.openForAll || (user.isDefined && entry.author.id == user.get.id)) map ModelConverter.get
  }

  object ModelConverter {
    def get(user: models.User): User =
      User(user.name, user.password, user.entries map get, user.comments map get)

    def get(entry: models.Entry): Entry =
      Entry(get(entry.author), entry.title, entry.content, entry.date, entry.openForAll, entry.tags map get, entry.comments map get)

    def get(comment: models.Comment): Comment =
      Comment(get(comment.author), comment.date, comment.content, get(comment.entry))

    def get(tag: models.Tag): Tag =
      Tag(tag.title)

    def get(user: User): models.User =
      models.User(user.name, user.password, user.entries map get, user.comments map get)

    def get(entry: Entry): models.Entry =
      models.Entry(get(entry.author), entry.title, entry.content, entry.date, entry.openForAll, entry.tags map get, entry.comments map get)

    def get(comment: Comment): models.Comment =
      models.Comment(get(comment.author), comment.date, comment.content, get(comment.entry))

    def get(tag: Tag): models.Tag =
      models.Tag(tag.title)
  }
}
