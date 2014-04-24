package models.squeryl

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{KeyedEntity, Session, SessionFactory, Schema}
import java.util.Date
import org.squeryl.adapters.H2Adapter
import play.api.db.DB
import play.api.Play.current
import models.Dao
import org.squeryl.dsl.CompositeKey2

object SquerylDao extends Schema with Dao {
  val users = table[User]
  val entries = table[Entry]
  val comments = table[Comment]
  val tags = table[Tag]

  val userEntry = oneToManyRelation(users, entries).via((u, e) => u.id === e.authorId)
  val userComment = oneToManyRelation(users, comments).via((u, c) => u.id === c.authorId)
  val entryComment = oneToManyRelation(entries, comments).via((e, c) => e.id === c.entryId)
  val entryTag = manyToManyRelation(entries, tags).via[EntryTagKey]((e, t, etk) => (e.id === etk.entryId, t.id === etk.tagId))

  class EntryTagKey(val entryId: Long, val tagId: Long) extends KeyedEntity[CompositeKey2[Long, Long]] {
    def id = compositeKey(entryId, tagId)
  }

  def isEntryVisible(entry: Entry)(implicit user: Option[models.User]) =
    entry.openForAll === true or entry.authorId === (user map (_.id) getOrElse -1L)

  def getPagesNumber(size: Long, itemsOnPage: Long): Long = {
    require(itemsOnPage != 0)
    Math.ceil(size / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
  }

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
    tag.title is (indexed, unique)
  ))

  userEntry.foreignKeyDeclaration.constrainReference(onDelete cascade)
  userComment.foreignKeyDeclaration.constrainReference(onDelete cascade)
  entryComment.foreignKeyDeclaration.constrainReference(onDelete cascade)
  entryTag.leftForeignKeyDeclaration.unConstrainReference()
  entryTag.rightForeignKeyDeclaration.unConstrainReference()

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
    val entry1 = Entry(1, "entry1", "content1<br/>bla1", new Date, true)
    entry1.id = 1
    val entry2 = Entry(1, "entry2", "content2<br/>bla2", new Date, false)
    entry2.id = 2
    val entry3 = Entry(1, "entry3", "content3<br/>bla3", new Date, true)
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

    entry1._tags.associate(tag1)
    entry1._tags.associate(tag2)
    entry2._tags.associate(tag1)
    entry3._tags.associate(tag2)
  }

  def getUser(name: String, password: String): Option[models.User] = inTransaction {
    from(users)(user =>
      where(user.name === name and user.password === password) select user
    ).headOption
  }

  def getUser(id: Long): Option[models.User] = inTransaction {
    users.lookup(id)
  }

  def getUser(name: String): Option[models.User] = inTransaction {
    from(users)(user =>
      where(user.name === name) select user
    ).headOption
  }

  def getEntries(user: Option[models.User], page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    implicit val impUser = user
    val size = from(entries)(entry =>
      where(isEntryVisible(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(isEntryVisible(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (getPagesNumber(size, itemsOnPage), xs.toList)
  }

  def getTag(title: String): Option[models.Tag] = inTransaction {
    tags.where(_.title === title).headOption
  }

  def getEntriesByTag(user: Option[models.User], tag: models.Tag, page: Int,
                      itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    implicit val impUser = user
    val size = from(entries, entryTag)((entry, et) =>
      where(isEntryVisible(entry) and (entry.id === et.entryId) and (tag.id === et.tagId)) compute count
    ).single.measures
    val xs = from(entries, entryTag)((entry, et) =>
      where(isEntryVisible(entry) and (entry.id === et.entryId) and (tag.id === et.tagId)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (getPagesNumber(size, itemsOnPage), xs.toList)
  }

  def getEntriesBySearch(user: Option[models.User], query: String, page: Int,
                         itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    implicit val impUser = user
    val size = from(entries)(entry =>
      where(isEntryVisible(entry) and (entry.title like "%" + query + "%")) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(isEntryVisible(entry) and (entry.title like "%" + query + "%")) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (getPagesNumber(size, itemsOnPage), xs.toList)
  }

  def getEntry(user: Option[models.User], id: Long): Option[models.Entry] = inTransaction {
    entries.lookup(id) filter (entry => entry.openForAll || (user.isDefined && entry.authorId == user.get.id))
  }

  def addUser(name: String, password: String): models.User = inTransaction {
    users.insert(User(name, password))
  }

  def addEntry(author: models.User, title: String, tags: Seq[models.Tag], openForAll: Boolean,
               content: String): models.Entry = inTransaction {
    val entry = entries.insert(Entry(author.id, title, content, new Date, openForAll))
    entry._author.assign(author.asInstanceOf[User])
    tags.foreach(tag => entry._tags.associate(tag.asInstanceOf[Tag]))
    entry
  }

  def getTagsByTitles(titles: Seq[String], addNew: Boolean): Seq[models.Tag] = inTransaction {
    val existingTags = tags.where(_.title in titles).toList
    if (addNew) {
      val existingTitles = existingTags.map(_.title)
      val existingTagMap = existingTitles.zip(existingTags).toMap
      titles.map { title =>
        if (existingTagMap.contains(title))
          existingTagMap(title)
        else
          tags.insert(Tag(title))
      }
    } else
      existingTags
  }

  def getTagsBySearch(query: String): Seq[models.Tag] = inTransaction {
    tags.where(_.title like "%" + query + "%").page(0, numberOfTagsBySearch).toList
  }

  def addComment(author: models.User, entry: models.Entry, content: String): models.Comment = inTransaction {
    val comment = comments.insert(Comment(author.id, new Date, content, entry.id))
    comment._author.assign(author.asInstanceOf[User])
    comment._entry.assign(entry.asInstanceOf[Entry])
    comment
  }

  def updateEntry(user: Option[models.User], id: Long, title: String, tags: Seq[models.Tag], openForAll: Boolean,
                  content: String): Option[models.Entry] = inTransaction {
    getEntry(user, id) map { entry =>
      entry.asInstanceOf[Entry]._tags.dissociateAll
      tags.foreach(tag => entry.asInstanceOf[Entry]._tags.associate(tag.asInstanceOf[Tag]))
      update(entries)(entry => where(entry.id === id) set(entry.title := title, entry.openForAll := openForAll,
        entry.content := content))
    }
    getEntry(user, id)
  }

  def deleteEntry(user: Option[models.User], id: Long): Boolean = inTransaction {
    entries.deleteWhere(entry => entry.id === id and entry.authorId === user.map(_.id).getOrElse(-1L)) != 0
  }

  def deleteComment(user: Option[models.User], id: Long): Boolean = inTransaction {
    comments.deleteWhere(comment => comment.id === id and comment.authorId === user.map(_.id).getOrElse(-1L)) != 0
  }

  def deleteUser(user: Option[models.User], id: Long): Boolean = inTransaction {
    if (user.map(_.id).getOrElse(-1L) != id)
      return false
    users.deleteWhere(u => u.id === id) != 0
  }
}