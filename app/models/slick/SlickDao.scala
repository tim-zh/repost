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
  val filters = TableQuery[Filter]
  val entryTagRelation = TableQuery[EntryTag]
  val filterUserRelation = TableQuery[FilterUser]
  private val db = Database.forDataSource(DB.getDataSource("default"))
  private val ddl = users.ddl ++ entries.ddl ++ comments.ddl ++ tags.ddl ++ filters.ddl ++
    entryTagRelation.ddl ++ filterUserRelation.ddl
  private def isEntryVisible(e: Entry)(implicit user: Option[models.User]) =
    e.openForAll || (LiteralColumn(user.isDefined) && e.author === user.get.id)

  def init() {
    db withDynSession {
      ddl.create

      users ++= Seq(
        (0, 0, "user1", "pass"),
        (1, 0, "user2", "passs"))
      entries ++= Seq(
        (0, 0, 0, "entry1", "content1<br/>bla1", true),
        (1, 0, 0, "entry2", "content2<br/>bla2", false),
        (2, 0, 1, "entry3", "content3<br/>bla3", true))
      comments map(x => (x.id, x.version, x.author, x.content, x.entry)) ++= Seq(
        (0, 0, 0, "comment1<br/>c1", 0),
        (1, 0, 0, "comment2<br/>c2", 0),
        (2, 0, 1, "comment3<br/>c3", 0),
        (3, 0, 0, "comment4<br/>c4", 1))
      tags ++= Seq(
        (0, 0, "tag1"),
        (1, 0, "tag2"))
      entryTagRelation ++= Seq(
        (0, 0),
        (0, 1),
        (1, 0),
        (2, 1))
    }
  }

  def dropSchema() {
    db withDynTransaction {
      ddl.drop
    }
  }

  def getUser(name: String, password: String): Option[models.User] = {
    db withDynTransaction {
      users.filter(u => u.name === name && u.password === password).firstOption map { t =>
        val x = models.User(t._3, t._4, Nil, Nil)
        x.id = t._1
        x.version = t._2
        x
      }
    }
  }

  def getUser(id: Long): Option[models.User] = {
    db withDynTransaction {
      users.filter(_.id === id).firstOption map { t =>
        val x = models.User(t._3, t._4, Nil, Nil)
        x.id = t._1
        x.version = t._2
        x
      }
    }
  }

  def getEntries(user: Option[models.User], filter: models.Filter, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    require(itemsOnPage != 0)
    db withDynTransaction {
      implicit val impUser = user
      val pagesNumber = entries.filter(isEntryVisible).length.run
      val xs = entries.filter(isEntryVisible).drop(page * itemsOnPage).take(itemsOnPage).list map { t =>
        val x = models.Entry(null, t._4, t._5, t._6, Nil, Nil)
        x.id = t._1
        x.version = t._2
        x
      }
      (pagesNumber, xs)
    }
  }

  def getTag(title: String): Option[models.Tag] = {
    db withDynTransaction {
      tags.filter(_.title === title).firstOption map { t =>
        val x = models.Tag(t._3)
        x.id = t._1
        x.version = t._2
        x
      }
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
      val pagesNumber = q.length.run
      val xs = q.drop(page * itemsOnPage).take(itemsOnPage).list map { t =>
        val x = models.Entry(null, t._4, t._5, t._6, Nil, Nil)
        x.id = t._1
        x.version = t._2
        x
      }
      (pagesNumber, xs)
    }
  }

  def getEntriesBySearch(user: Option[models.User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    require(itemsOnPage != 0)
    db withDynTransaction {
      implicit val impUser = user
      val q = (e: Entry) => e.title.like("%" + query + "%") && isEntryVisible(e)
      val pagesNumber = entries.filter(q).length.run
      val xs = entries.filter(q).drop(page * itemsOnPage).take(itemsOnPage).list map { t =>
        val x = models.Entry(null, t._4, t._5, t._6, Nil, Nil)
        x.id = t._1
        x.version = t._2
        x
      }
      (pagesNumber, xs)
    }
  }

  def getEntry(user: Option[models.User], id: Long): Option[models.Entry] = {
    db withDynTransaction {
      implicit val impUser = user
      entries.filter(e => e.id === id && isEntryVisible(e)).firstOption map { t =>
        val x = models.Entry(null, t._4, t._5, t._6, Nil, Nil)
        x.id = t._1
        x.version = t._2
        x
      }
    }
  }
}
