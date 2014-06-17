package models.squeryl

import java.sql.Timestamp
import java.util.Date
import models.Dao
import org.squeryl.{KeyedEntity, Session, SessionFactory, Schema}
import org.squeryl.adapters.H2Adapter
import org.squeryl.dsl.CompositeKey2
import org.squeryl.PrimitiveTypeMode._
import play.api.db.DB
import play.api.Play.current

object SquerylDao extends Schema with Dao {
  val users = table[User]("users")
  val entries = table[Entry]("entries")
  val comments = table[Comment]("comments")
  val tags = table[Tag]("tags")

  val userEntry = oneToManyRelation(users, entries).via((u, e) => u.id === e.authorId)
  val userComment = oneToManyRelation(users, comments).via((u, c) => u.id === c.authorId)
  val userFavoriteTag = manyToManyRelation(users, tags, "user_tag").via[UserTagKey]((u, t, utk) => (u.id === utk.userId, t.id === utk.tagId))
  val entryComment = oneToManyRelation(entries, comments).via((e, c) => e.id === c.entryId)
  val entryTag = manyToManyRelation(entries, tags, "entry_tag").via[EntryTagKey]((e, t, etk) => (e.id === etk.entryId, t.id === etk.tagId))

  case class UserTagKey(userId: Long, tagId: Long) extends KeyedEntity[CompositeKey2[Long, Long]] {
    def id = compositeKey(userId, tagId)
  }

  case class EntryTagKey(entryId: Long, tagId: Long) extends KeyedEntity[CompositeKey2[Long, Long]] {
    def id = compositeKey(entryId, tagId)
  }

  private def isEntryVisible(entry: Entry, user: Option[models.User]) =
    entry.openForAll === true or entry.authorId === (user map (_.id) getOrElse -1L)

  on(users)(user => declare(
    user.id is (primaryKey, autoIncremented),
    columns(user.name, user.password) are (unique, indexed("idx_user_name_password")),
    user.name is (unique, indexed("idx_user_name")),
    user.entryListType defaultsTo models.ListType.list,
    user.dateFormat defaultsTo controllers.defaultDateFormat,
    user.itemsOnPage defaultsTo defaultItemsOnPage,
    user.codeTheme defaultsTo 0
  ))

  on(entries)(entry => declare(
    entry.id is (primaryKey, autoIncremented),
    entry.title is indexed("idx_entry_title"),
    entry.content is dbType("varchar(40960)")
  ))

  on(comments)(comment => declare(
    comment.id is (primaryKey, autoIncremented),
    comment.content is dbType("varchar(4096)")
  ))

  on(tags)(tag => declare(
    tag.id is (primaryKey, autoIncremented),
    tag.title is (unique, indexed("idx_tag_title"))
  ))

  userEntry.foreignKeyDeclaration.constrainReference(onDelete cascade)
  userComment.foreignKeyDeclaration.constrainReference(onDelete cascade)
  entryComment.foreignKeyDeclaration.constrainReference(onDelete cascade)
  userFavoriteTag.leftForeignKeyDeclaration.unConstrainReference()
  userFavoriteTag.rightForeignKeyDeclaration.unConstrainReference()
  entryTag.leftForeignKeyDeclaration.unConstrainReference()
  entryTag.rightForeignKeyDeclaration.unConstrainReference()

  SessionFactory.concreteFactory = Some(() => Session.create(DB.getConnection("embedded"), new H2Adapter))

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
    val size = from(entries)(entry =>
      where(isEntryVisible(entry, user)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(isEntryVisible(entry, user)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (getPagesNumber(size, itemsOnPage), xs.toList)
  }

  def getTag(title: String): Option[models.Tag] = inTransaction {
    tags.where(_.title === title).headOption
  }

  def getEntriesByTag(user: Option[models.User], tag: models.Tag, page: Int,
                      itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    val filterQuery = (entry: Entry, et: EntryTagKey) =>
      isEntryVisible(entry, user) and (entry.id === et.entryId) and (tag.id === et.tagId)
    val size = from(entries, entryTag)((entry, et) =>
      where(filterQuery(entry, et)) compute count
    ).single.measures
    val xs = from(entries, entryTag)((entry, et) =>
      where(filterQuery(entry, et)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (getPagesNumber(size, itemsOnPage), xs.toList)
  }

  def getEntriesBySearch(user: Option[models.User], query: String, page: Int,
                         itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    val filterQuery = (entry: Entry) =>
      isEntryVisible(entry, user) and (entry.title like "%" + query + "%")
    val size = from(entries)(entry =>
      where(filterQuery(entry)) compute count
    ).single.measures
    val xs = from(entries)(entry =>
      where(filterQuery(entry)) select entry
    ).page(page * itemsOnPage, itemsOnPage)
    (getPagesNumber(size, itemsOnPage), xs.toList)
  }

  def getEntriesBySearch(user: Option[models.User], query: String, _from: Option[Date], _to: Option[Date],
                         users: Seq[models.User], tags: Seq[models.Tag], page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = inTransaction {
    val filterQuery = (entry: Entry, et: EntryTagKey) => {
     var q = isEntryVisible(entry, user)
      if (!query.isEmpty)
        q = q and (entry.title like "%" + query + "%")
      if (!users.isEmpty)
        q = q and (entry.authorId in users.map(_.id))
      if (!tags.isEmpty)
        q = q and (entry.id === et.entryId and (et.tagId in tags.map(_.id)))
      if (_from.isDefined && _to.isDefined)
        q = q and (entry.date between(new Timestamp(_from.get.getTime), new Timestamp(_to.get.getTime)))
      else if (_from.isDefined)
        q = q and (entry.date between(new Timestamp(_from.get.getTime), controllers.now))
      else if (_to.isDefined)
        q = q and (entry.date between(new Timestamp(0), new Timestamp(_to.get.getTime)))
      q
    }
    var size = 0L
    var xs: List[Entry] = Nil
    if (tags.isEmpty) {
      size = from(entries)(entry =>
        where(filterQuery(entry, null)) compute count
      ).single.measures
      xs = from(entries)(entry =>
        where(filterQuery(entry, null)) select entry
      ).page(page * itemsOnPage, itemsOnPage).toList
    } else {
      size = from(entries, entryTag)((entry, et) =>
        where(filterQuery(entry, et)) compute count
      ).single.measures
      xs = from(entries, entryTag)((entry, et) =>
        where(filterQuery(entry, et)) select entry
      ).page(page * itemsOnPage, itemsOnPage).toList
    }
    (getPagesNumber(size, itemsOnPage), xs)
  }

  def getEntry(user: Option[models.User], id: Long): Option[models.Entry] = inTransaction {
    entries.lookup(id) filter (entry => entry.openForAll || (user.isDefined && entry.authorId == user.get.id))
  }

  def addUser(name: String, password: String): models.User = inTransaction {
    users.insert(User(name, password, models.ListType.list, controllers.defaultDateFormat, defaultItemsOnPage, 0))
  }

  def addEntry(author: models.User, title: String, tags: Seq[models.Tag], openForAll: Boolean,
               content: String): models.Entry = inTransaction {
    val entry = entries.insert(Entry(author.id, if (title.isEmpty) "_" else title, content, controllers.now, openForAll))
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
    tags.where(_.title like "%" + query + "%").page(0, rowNumberInPopupSearch).toList
  }

  def getUsersByNames(names: Seq[String]): Seq[models.User] = inTransaction {
    users.where(_.name in names).page(0, rowNumberInPopupSearch).toList
  }

  def getUsersBySearch(query: String): Seq[models.User] = inTransaction {
    users.where(_.name like "%" + query + "%").page(0, rowNumberInPopupSearch).toList
  }

  def addComment(author: models.User, entry: models.Entry, content: String): models.Comment = inTransaction {
    val comment = comments.insert(Comment(author.id, controllers.now, content, entry.id))
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
        entry.content := content, entry.date := controllers.now))
    }
    getEntry(user, id)
  }

  def deleteEntry(user: Option[models.User], id: Long): Boolean = inTransaction {
    entries.deleteWhere(entry => entry.id === id and entry.authorId === user.map(_.id).getOrElse(-1L)) != 0
  }

  def deleteComment(user: Option[models.User], id: Long): Boolean = inTransaction {
    comments.deleteWhere(comment => comment.id === id and comment.authorId === user.map(_.id).getOrElse(-1L)) != 0
  }

  def deleteUser(user: Option[models.User]): Boolean = inTransaction {
    if (!user.isDefined)
      return false
    users.deleteWhere(u => u.id === user.get.id) != 0
  }

  def addFavoriteTag(user: Option[models.User], title: String): Boolean = inTransaction {
    val tag = getTag(title)
    var result = user.isDefined && tag.isDefined
    if (result)
      result = userFavoriteTag.insert(UserTagKey(user.get.id, tag.get.id)) != null
    result
  }

  def removeFavoriteTag(user: Option[models.User], title: String): Boolean = inTransaction {
    val tag = getTag(title)
    var result = user.isDefined && tag.isDefined
    if (result)
      result = userFavoriteTag.deleteWhere(ut => ut.userId === user.get.id and ut.tagId === tag.get.id) != 0
    result
  }

  def updateUser(id: Long, password: String, entryListType: models.ListType.LT, dateFormat: String, itemsOnPage: Int,
                 codeTheme: Int): Option[models.User] = inTransaction {
    update(users)(user => where(user.id === id) set(user.password := password, user.entryListType := entryListType,
      user.dateFormat := dateFormat, user.itemsOnPage := itemsOnPage, user.codeTheme := codeTheme))
    getUser(id)
  }
}