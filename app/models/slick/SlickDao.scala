package models.slick

import scala.slick.driver.H2Driver.simple._
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

  def init() {
    db withSession { implicit session =>
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
    ddl.drop
  }

  def getUser(name: String, password: String): Option[models.User] = ???

  def getUser(id: Long): Option[models.User] = ???

  def getEntries(user: Option[models.User], filter: models.Filter, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = ???

  def getTag(id: Long): Option[models.Tag] = ???

  def getEntriesByTag(user: Option[models.User], tag: models.Tag, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = ???

  def getEntriesBySearch(user: Option[models.User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[models.Entry]) = ???

  def getEntry(id: Long): Option[models.Entry] = ???
}
