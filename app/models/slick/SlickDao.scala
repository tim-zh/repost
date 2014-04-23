package models.slick

import scala.slick.driver.H2Driver.simple._
import Database.dynamicSession
import play.api.db.DB
import play.api.Play.current
import models.Dao

object SlickDao extends Dao {
  val users = TableQuery[User]
  val entries = TableQuery[Entry]
  val comments = TableQuery[Comment]
  val tags = TableQuery[Tag]
  val entryTagRelation = TableQuery[EntryTag]
  private val db = Database.forDataSource(DB.getDataSource("default"))
  private val ddl = users.ddl ++ entries.ddl ++ comments.ddl ++ tags.ddl ++ entryTagRelation.ddl
  private def isEntryVisible(e: Entry)(implicit user: Option[models.User]) = e.openForAll || (user match {
    case Some(x) => e.author === x.id
    case None => false
  })

  def init() {
    db withDynSession {
      ddl.create

      users map (x => (x.name, x.password)) ++= Seq(
        ("user1", "pass"),
        ("user2", "passs"))
      entries map (x => (x.author, x.title, x.content, x.openForAll)) ++= Seq(
        (1, "entry1", "content1<br/>bla1", true),
        (1, "entry2", "content2<br/>bla2", false),
        (2, "entry3", "content3<br/>bla3", true),
        (1, "entry4", "content4<br/>bla4", true))
      comments map (x => (x.author, x.content, x.entry)) ++= Seq(
        (1, "comment1<br/>c1", 1),
        (1, "comment2<br/>c2", 1),
        (2, "comment3<br/>c3", 1),
        (1, "comment4<br/>c4", 2))
      tags map (x => (x.title)) ++= Seq(
        ("tag1"),
        ("tag2"))
      entryTagRelation ++= Seq(
        (1, 1),
        (1, 2),
        (2, 1),
        (3, 1))
    }
  }

  def dropSchema() {
    db withDynTransaction {
      ddl.drop
    }
  }

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
    require(itemsOnPage != 0)
    db withDynTransaction {
      implicit val impUser = user
      val pagesNumber = Math.ceil(entries.filter(isEntryVisible).length.run / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
      val xs = entries.filter(isEntryVisible).drop(page * itemsOnPage).take(itemsOnPage).list map ModelConverter.getEntry
      (pagesNumber, xs)
    }
  }

  def getTag(title: String): Option[models.Tag] = {
    db withDynTransaction {
      tags.filter(_.title === title).firstOption map ModelConverter.getTag
    }
  }

  def getEntriesByTag(user: Option[models.User], tag: models.Tag, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    require(itemsOnPage != 0)
    db withDynTransaction {
      implicit val impUser = user
      val q = for {
        entry <- entries
        etr <- entryTagRelation
        if etr.tag === tag.id && entry.id === etr.entry && isEntryVisible(entry)
      } yield entry
      val pagesNumber = Math.ceil(q.length.run / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
      val xs = q.drop(page * itemsOnPage).take(itemsOnPage).list map ModelConverter.getEntry
      (pagesNumber, xs)
    }
  }

  def getEntriesBySearch(user: Option[models.User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    require(itemsOnPage != 0)
    db withDynTransaction {
      implicit val impUser = user
      val q = (e: Entry) => e.title.like("%" + query + "%") && isEntryVisible(e)
      val pagesNumber = Math.ceil(entries.filter(q).length.run / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
      val xs = entries.filter(q).drop(page * itemsOnPage).take(itemsOnPage).list map ModelConverter.getEntry
      (pagesNumber, xs)
    }
  }

  def getEntry(user: Option[models.User], id: Long): Option[models.Entry] = {
    db withDynTransaction {
      implicit val impUser = user
      entries.filter(e => e.id === id && isEntryVisible(e)).firstOption map ModelConverter.getEntry
    }
  }

  def getTagsByEntry(entryId: Long): Seq[models.Tag] = {
    db withDynTransaction {
      val q = for {
        tag <- tags
        etr <- entryTagRelation
        if etr.entry === entryId && etr.tag === tag.id
      } yield tag
      q.list map ModelConverter.getTag
    }
  }

  def getComments(entryId: Long): Seq[models.Comment] = {
    db withDynTransaction {
      comments.filter(_.id === entryId).list map ModelConverter.getComment
    }
  }

  def getEntriesByUser(userId: Long): Seq[models.Entry] = {
    db withDynTransaction {
      entries.filter(_.author === userId).list map ModelConverter.getEntry
    }
  }

  def getCommentsByUser(userId: Long): Seq[models.Comment] = {
    db withDynTransaction {
      comments.filter(_.author === userId).list map ModelConverter.getComment
    }
  }

  def getCommentsByEntry(entryId: Long): Seq[models.Comment] = {
    db withDynTransaction {
      comments.filter(_.entry === entryId).list map ModelConverter.getComment
    }
  }

  def getEntry(id: Long): Option[models.Entry] = {
    db withDynTransaction {
      entries.filter(_.id === id).firstOption map ModelConverter.getEntry
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
      id = (entries.map(x => (x.author, x.title, x.content, x.openForAll)) returning entries.map(_.id)) +=
        (author.id, title, content, openForAll)
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
            val id = (tags.map(x => (x.title)) returning tags.map(_.id)) += title
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
      tags.filter(_.title like "%" + query + "%").take(numberOfTagsBySearch).list map ModelConverter.getTag
    }
  }

  def addComment(author: models.User, entry: models.Entry, content: String): models.Comment = {
    var id = -1L
    db withDynTransaction {
      id = (comments.map(x => (x.author, x.content, x.entry)) returning comments.map(_.id)) +=
        (author.id, content, entry.id)
    }
    getComment(id).get
  }

  def getComment(id: Long): Option[models.Comment] = {
    db withDynTransaction {
      comments.filter(_.id === id).firstOption map ModelConverter.getComment
    }
  }

  def updateEntry(user: Option[models.User], id: Long, title: String, entryTags: Seq[models.Tag], openForAll: Boolean,
                  content: String): Option[models.Entry] = {
    db withDynTransaction {
      val q = for (entry <- entries if entry.id === id && entry.author === user.map(_.id).getOrElse(-1L))
        yield entry
      q.map(entry => (entry.title, entry.openForAll, entry.content)).update(title, openForAll, content)
      (for (etr <- entryTagRelation if etr.entry === id) yield etr).delete
      entryTags.foreach(tag => entryTagRelation += (id, tag.id))
    }
    getEntry(user, id)
  }

  def deleteEntry(user: Option[models.User], id: Long): Boolean = {
    db withDynTransaction {
      val q = for (entry <- entries if entry.id === id && entry.author === user.map(_.id).getOrElse(-1L)) yield entry
      if (q.length.run == 0)
        return false
      (for (comment <- comments if comment.entry === id) yield comment).delete
      (for (etr <- entryTagRelation if etr.entry === id) yield etr).delete
      q.delete
      true
    }
  }
}