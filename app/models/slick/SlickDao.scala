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
}