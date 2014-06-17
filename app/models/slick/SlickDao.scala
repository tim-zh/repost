package models.slick

import java.sql.Timestamp
import models.Dao
import play.api.db.DB
import play.api.Play.current
import scala.slick.driver.H2Driver.simple._
import Database.dynamicSession

object SlickDao extends Dao {
  val users = TableQuery[User]
  val entries = TableQuery[Entry]
  val comments = TableQuery[Comment]
  val tags = TableQuery[Tag]
  val userFavoriteTagRelation = TableQuery[UserFavoriteTag]
  val entryTagRelation = TableQuery[EntryTag]
  private val db = Database.forDataSource(DB.getDataSource("embedded"))

  private def isEntryVisible(e: Entry, user: Option[models.User]) = e.openForAll || (user match {
    case Some(x) => e.author === x.id
    case None => false
  })

  def getUser(name: String, password: String): Option[models.User] = {
    db withDynTransaction {
      users.filter(u => u.name === name && u.password === password).firstOption map ModelConverter.getUser
    }
  }

  def getUser(id: Long): Option[models.User] = {
    db withDynTransaction {
      users.filter(_.id === id).firstOption map ModelConverter.getUser
    }
  }

  def getUser(name: String): Option[models.User] = {
    db withDynTransaction {
      users.filter(_.name === name).firstOption map ModelConverter.getUser
    }
  }

  def getEntries(user: Option[models.User], page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    db withDynTransaction {
      val size = entries.filter(e => isEntryVisible(e, user)).length.run
      val xs = entries.filter(e => isEntryVisible(e, user)).drop(page * itemsOnPage).take(itemsOnPage).list map ModelConverter.getEntry
      (getPagesNumber(size, itemsOnPage), xs)
    }
  }

  def getTag(title: String): Option[models.Tag] = {
    db withDynTransaction {
      tags.filter(_.title === title).firstOption map ModelConverter.getTag
    }
  }

  def getEntriesByTag(user: Option[models.User], tag: models.Tag, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    db withDynTransaction {
      val query = for {
        entry <- entries
        etr <- entryTagRelation
        if etr.tag === tag.id && entry.id === etr.entry && isEntryVisible(entry, user)
      } yield entry
      val size = query.length.run
      val xs = query.drop(page * itemsOnPage).take(itemsOnPage).list map ModelConverter.getEntry
      (getPagesNumber(size, itemsOnPage), xs)
    }
  }

  def getEntriesBySearch(user: Option[models.User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    db withDynTransaction {
      val filterQuery = (e: Entry) => e.title.like("%" + query + "%") && isEntryVisible(e, user)
      val size = entries.filter(filterQuery).length.run
      val xs = entries.filter(filterQuery).drop(page * itemsOnPage).take(itemsOnPage).list map ModelConverter.getEntry
      (getPagesNumber(size, itemsOnPage), xs)
    }
  }

  def getEntriesBySearch(user: Option[models.User], query: String, from: Option[java.util.Date], to: Option[java.util.Date],
                         users: Seq[models.User], tags: Seq[models.Tag], page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    db withDynTransaction {
      val _from = from.map(d => new Timestamp(d.getTime))
      val _to = to.map(d => new Timestamp(d.getTime))
      val filterQuery = (e: Entry, etr: EntryTag) => {
        var q = isEntryVisible(e, user)
        if (!query.isEmpty)
          q = q && e.title.like("%" + query + "%")
        if (!users.isEmpty)
          q = q && (e.author inSet users.map(_.id))
        if (!tags.isEmpty)
          q = q && (e.id === etr.entry && (etr.tag inSet tags.map(_.id)))
        if (_from.isDefined && _to.isDefined)
          q = q && (e.date between(_from.get, _to.get))
        else if (_from.isDefined)
          q = q && (e.date between(_from.get, controllers.now))
        else if (_to.isDefined)
          q = q && (e.date between(new Timestamp(0), _to.get))
        q
      }
      val q =
        if (tags.isEmpty)
          entries.filter(filterQuery(_, null))
        else
          for {
            entry <- entries
            etr <- entryTagRelation
            if filterQuery(entry, etr)
          } yield entry
      val size = q.length.run
      val xs = q.drop(page * itemsOnPage).take(itemsOnPage).list map ModelConverter.getEntry
      (getPagesNumber(size, itemsOnPage), xs)
    }
  }

  def getEntry(user: Option[models.User], id: Long): Option[models.Entry] = {
    db withDynTransaction {
      entries.filter(e => e.id === id && isEntryVisible(e, user)).firstOption map ModelConverter.getEntry
    }
  }

  def addUser(name: String, password: String): models.User = {
    var id = -1L
    db withDynTransaction {
      id = (users.map(x => (x.name, x.password)) returning users.map(_.id)) += (name, password)
    }
    getUser(id).get
  }

  def addEntry(author: models.User, title: String, entryTags: Seq[models.Tag], openForAll: Boolean, content: String): models.Entry = {
    var id = -1L
    db withDynTransaction {
      id = (entries.map(x => (x.author, x.title, x.content, x.date, x.openForAll)) returning entries.map(_.id)) +=
        (author.id, if (title.isEmpty) "_" else title, content, controllers.now, openForAll)
      entryTags.foreach(tag => entryTagRelation += (id, tag.id))
    }
    getEntry(id).get
  }

  def getTagsByTitles(titles: Seq[String], addNew: Boolean): Seq[models.Tag] = {
    db withDynTransaction {
      val existingTags = tags.filter(_.title inSet titles).list map ModelConverter.getTag
      if (addNew) {
        val existingTitles = existingTags.map(_.title)
        val existingTagMap = existingTitles.zip(existingTags).toMap
        titles.map { title =>
          if (existingTagMap.contains(title))
            existingTagMap(title)
          else {
            val id = (tags.map(_.title) returning tags.map(_.id)) += title
            val result = SlickTag(title)
            result.id = id
            result
          }
        }
      } else
        existingTags
    }
  }

  def getTagsBySearch(query: String): Seq[models.Tag] = {
    db withDynTransaction {
      tags.filter(_.title like "%" + query + "%").take(rowNumberInPopupSearch).list map ModelConverter.getTag
    }
  }

  def getUsersByNames(names: Seq[String]): Seq[models.User] = {
    db withDynTransaction {
      users.filter(_.name inSet names).take(rowNumberInPopupSearch).list map ModelConverter.getUser
    }
  }

  def getUsersBySearch(query: String): Seq[models.User] = {
    db withDynTransaction {
      users.filter(_.name like "%" + query + "%").take(rowNumberInPopupSearch).list map ModelConverter.getUser
    }
  }

  def addComment(author: models.User, entry: models.Entry, content: String): models.Comment = {
    var id = -1L
    db withDynTransaction {
      id = (comments.map(x => (x.author, x.date, x.content, x.entry)) returning comments.map(_.id)) +=
        (author.id, controllers.now, content, entry.id)
    }
    getComment(id).get
  }

  def updateEntry(user: Option[models.User], id: Long, title: String, entryTags: Seq[models.Tag], openForAll: Boolean,
                  content: String): Option[models.Entry] = {
    db withDynTransaction {
      val query = for (entry <- entries if entry.id === id && entry.author === user.map(_.id).getOrElse(-1L))
        yield entry
      query.map(entry => (entry.title, entry.openForAll, entry.content, entry.date)).update(title, openForAll, content,
        controllers.now)
      (for (etr <- entryTagRelation if etr.entry === id) yield etr).delete
      entryTags.foreach(tag => entryTagRelation += (id, tag.id))
    }
    getEntry(user, id)
  }

  def deleteEntry(user: Option[models.User], id: Long): Boolean = {
    db withDynTransaction {
      val query = for (entry <- entries if entry.id === id && entry.author === user.map(_.id).getOrElse(-1L)) yield entry
      if (query.length.run == 0)
        return false
      (for (comment <- comments if comment.entry === id) yield comment).delete
      (for (etr <- entryTagRelation if etr.entry === id) yield etr).delete
      query.delete
      true
    }
  }

  def deleteComment(user: Option[models.User], id: Long): Boolean = {
    db withDynTransaction {
      val query = for (comment <- comments if comment.id === id && comment.author === user.map(_.id).getOrElse(-1L)) yield comment
      if (query.length.run == 0)
        return false
      query.delete
      true
    }
  }

  def deleteUser(user: Option[models.User]): Boolean = {
    if (!user.isDefined)
      return false
    db withDynTransaction {
      val query = for (u <- users if u.id === user.get.id) yield u
      if (query.length.run == 0)
        return false
      (for (entry <- entries if entry.author === user.get.id) yield entry.id).list.foreach(entryId => deleteEntry(user, entryId))
      (for (comment <- comments if comment.author === user.get.id) yield comment).delete
      (for (utr <- userFavoriteTagRelation if utr.user === user.get.id) yield utr).delete
      query.delete
      true
    }
  }

  def addFavoriteTag(user: Option[models.User], title: String): Boolean = {
    db withDynTransaction {
      val tag = getTag(title)
      val result = user.isDefined && tag.isDefined
      if (result)
        userFavoriteTagRelation += (user.get.id, tag.get.id)
      result
    }
  }

  def removeFavoriteTag(user: Option[models.User], title: String): Boolean = {
    db withDynTransaction {
      val tag = getTag(title)
      var result = user.isDefined && tag.isDefined
      if (result)
        result = (for (utr <- userFavoriteTagRelation if utr.user === user.get.id && utr.tag === tag.get.id) yield utr).
          delete != 0
      result
    }
  }

  def updateUser(id: Long, password: String, entryListType: models.ListType.LT, dateFormat: String, itemsOnPage: Int,
                 codeTheme: Int): Option[models.User] = {
    db withDynTransaction {
      val query = for (user <- users if user.id === id) yield user
      query.map(user => (user.password, user.entryListType, user.dateFormat, user.itemsOnPage, user.codeTheme)).
        update(password, entryListType.id, dateFormat, itemsOnPage, codeTheme)
    }
    getUser(id)
  }

  private[slick] def getTagsByEntry(entryId: Long): Seq[models.Tag] = {
    db withDynTransaction {
      val query = for {
        tag <- tags
        etr <- entryTagRelation
        if etr.entry === entryId && etr.tag === tag.id
      } yield tag
      query.list map ModelConverter.getTag
    }
  }

  private[slick] def getEntriesByUser(userId: Long): Seq[models.Entry] = {
    db withDynTransaction {
      entries.filter(_.author === userId).list map ModelConverter.getEntry
    }
  }

  private[slick] def getCommentsByUser(userId: Long): Seq[models.Comment] = {
    db withDynTransaction {
      comments.filter(_.author === userId).list map ModelConverter.getComment
    }
  }

  private[slick] def getFavoriteTagsByUser(userId: Long): Seq[models.Tag] = {
    db withDynTransaction {
      val query = for {
        tag <- tags
        utr <- userFavoriteTagRelation
        if utr.user === userId && utr.tag === tag.id
      } yield tag
      query.list map ModelConverter.getTag
    }
  }

  private[slick] def getCommentsByEntry(entryId: Long): Seq[models.Comment] = {
    db withDynTransaction {
      comments.filter(_.entry === entryId).list map ModelConverter.getComment
    }
  }

  private[slick] def getEntry(id: Long): Option[models.Entry] = {
    db withDynTransaction {
      entries.filter(_.id === id).firstOption map ModelConverter.getEntry
    }
  }

  private[slick] def getComment(id: Long): Option[models.Comment] = {
    db withDynTransaction {
      comments.filter(_.id === id).firstOption map ModelConverter.getComment
    }
  }
}