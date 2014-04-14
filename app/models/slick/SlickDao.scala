package models.slick

import scala.slick.driver.H2Driver.simple._
import Database.dynamicSession
import play.api.db.DB
import play.api.Play.current
import models.Dao
import java.sql.Date

object SlickDao extends Dao {
  val users = TableQuery[User]
  val entries = TableQuery[Entry]
  val comments = TableQuery[Comment]
  val tags = TableQuery[Tag]
  val filters = TableQuery[Filter]
  val entryTagRelation = TableQuery[EntryTag]
  val filterTagRelation = TableQuery[FilterTag]
  val filterUserRelation = TableQuery[FilterUser]
  private val db = Database.forDataSource(DB.getDataSource("default"))
  private val ddl = users.ddl ++ entries.ddl ++ comments.ddl ++ tags.ddl ++ filters.ddl ++
    entryTagRelation.ddl ++ filterUserRelation.ddl
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
      filters map (x => (x.title)) ++= Seq(
        ("filter1"),
        ("filter2"))
      entryTagRelation ++= Seq(
        (1, 1),
        (1, 2),
        (2, 1),
        (3, 1))
      filterTagRelation ++= Seq(
        (1, 1),
        (1, 2),
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

  def getEntries(user: Option[models.User], page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    require(itemsOnPage != 0)
    db withDynTransaction {
      implicit val impUser = user
      val pagesNumber = Math.ceil(entries.filter(isEntryVisible).length.run / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
      val xs = entries.filter(isEntryVisible).drop(page * itemsOnPage).take(itemsOnPage).list map { t =>
        val x = models.Entry(getUser(t._3).get, t._4, t._5, t._6, t._7, getTagsByEntry(t._1), Nil)
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

  def getFilter(title: String): Option[models.Filter] = {
    db withDynTransaction {
      filters.filter(_.title === title).firstOption map { t =>
        val x = models.Filter(t._3, getTagsByFilter(t._1), Nil, t._4, t._5)
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
      val pagesNumber = Math.ceil(q.length.run / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
      val xs = q.drop(page * itemsOnPage).take(itemsOnPage).list map { t =>
        val x = models.Entry(getUser(t._3).get, t._4, t._5, t._6, t._7, getTagsByEntry(t._1), Nil)
        x.id = t._1
        x.version = t._2
        x
      }
      (pagesNumber, xs)
    }
  }

  def getEntriesByFilter(user: Option[models.User], filter: models.Filter, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = {
    require(itemsOnPage != 0)
    db withDynTransaction {
      implicit val impUser = user
      val fauthors = (for {
        fuser <- users
        fur <- filterUserRelation
        if fur.user === fuser.id && fur.filter === filter.id
      } yield fuser).map(_.id).list
      val ftags = (for {
        tag <- tags
        ftr <- filterTagRelation
        if ftr.tag === tag.id && ftr.filter === filter.id
      } yield tag).map(_.id).list
      val q = for {
        etr <- entryTagRelation
        entry <- entries
        if (LiteralColumn(fauthors.isEmpty) || (entry.author inSet fauthors)) &&
          (LiteralColumn(ftags.isEmpty) || (etr.entry === entry.id && (etr.tag inSet ftags))) &&
          (LiteralColumn(!filter.startDate.isDefined) || (entry.date >= filter.startDate.get.asInstanceOf[Date])) &&
          (LiteralColumn(!filter.endDate.isDefined) || (entry.date <= filter.endDate.get.asInstanceOf[Date]))
      } yield entry
      val pagesNumber = Math.ceil(q.length.run / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
      val xs = q.drop(page * itemsOnPage).take(itemsOnPage).list map { t =>
        val x = models.Entry(getUser(t._3).get, t._4, t._5, t._6, t._7, getTagsByEntry(t._1), Nil)
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
      val pagesNumber = Math.ceil(entries.filter(q).length.run / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
      val xs = entries.filter(q).drop(page * itemsOnPage).take(itemsOnPage).list map { t =>
        val x = models.Entry(getUser(t._3).get, t._4, t._5, t._6, t._7, getTagsByEntry(t._1), Nil)
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
        val x = models.Entry(getUser(t._3).get, t._4, t._5, t._6, t._7, getTagsByEntry(t._1), getComments(t._1))
        x.id = t._1
        x.version = t._2
        x
      }
    }
  }

  def getTagsByEntry(entryId: Long): Seq[models.Tag] = {
    db withDynTransaction {
      val q = for {
        tag <- tags
        etr <- entryTagRelation
        if etr.entry === entryId && etr.tag === tag.id
      } yield tag
      q.list map { t =>
        val x = models.Tag(t._3)
        x.id = t._1
        x.version = t._2
        x
      }
    }
  }

  def getTagsByFilter(filterId: Long): Seq[models.Tag] = {
    db withDynTransaction {
      val q = for {
        tag <- tags
        ftr <- filterTagRelation
        if ftr.filter === filterId && ftr.tag === tag.id
      } yield tag
      q.list map { t =>
        val x = models.Tag(t._3)
        x.id = t._1
        x.version = t._2
        x
      }
    }
  }

  def getComments(entryId: Long): Seq[models.Comment] = {
    db withDynTransaction {
      comments.filter(_.id === entryId).list map { t =>
        val x = models.Comment(getUser(t._3).get, t._4, t._5, null)
        x.id = t._1
        x.version = t._2
        x
      }
    }
  }
}
