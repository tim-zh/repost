package models

import java.util.Date

object TestDao extends Dao {
  val tag1 = Tag("tag1")
  tag1.id = 1
  val tag2 = Tag("tag2")
  tag2.id = 2
  val user1 = User("user1", "pass", null, null)
  user1.id = 1
  val entry1 = Entry(user1, "entry1", "content1<br/>bla1", new Date, true, Seq(tag1, tag2), null)
  entry1.id = 1
  val entry2 = Entry(user1, "entry2", "content2<br/>bla2", new Date, false, Seq(tag1), null)
  entry2.id = 2
  val entry3 = Entry(user1, "entry3", "content3<br/>bla3", new Date, true, Seq(tag2), null)
  entry3.id = 3
  val comment1 = Comment(user1, new Date, "comment1<br/>c1", entry1)
  val comment2 = Comment(user1, new Date, "comment2<br/>c2", entry1)
  val comment3 = Comment(user1, new Date, "comment3<br/>c3", entry1)
  val comment4 = Comment(user1, new Date, "comment4<br/>c4", entry2)
  entry1.comments = Seq(comment1, comment2, comment3)
  entry2.comments = Seq(comment4)
  entry3.comments = Seq(comment1, comment2, comment3, comment4)
  user1.entries = Seq(entry1, entry2, entry3)
  user1.comments = Seq(comment1, comment2, comment3, comment4)

  def getUser(name: String, password: String): Option[User] =
    if (name == TestDao.user1.name && password == TestDao.user1.password) Some(user1) else None

  def getUser(id: Long): Option[User] = if (id == user1.id) Some(user1) else None

  def getEntries(user: Option[User], page: Int, itemsOnPage: Int): (Long, Seq[Entry]) = {
    require(itemsOnPage != 0)
    var list = Seq(entry1, entry2, entry3)
    val pagesNumber = list.size / itemsOnPage + (if (list.size % itemsOnPage != 0) 1 else 0)
    list = list drop (page * itemsOnPage) take itemsOnPage
    (pagesNumber, list)
  }

  def getTag(title: String): Option[Tag] = Seq(tag1, tag2) find { _.title == title }

  def getEntriesByTag(user: Option[User], tag: Tag, page: Int, itemsOnPage: Int): (Long, Seq[Entry]) = {
    require(itemsOnPage != 0)
    var list = Seq(entry1, entry2, entry3) filter { _.tags.contains(tag) }
    val pagesNumber = list.size / itemsOnPage + (if (list.size % itemsOnPage != 0) 1 else 0)
    list = list drop (page * itemsOnPage) take itemsOnPage
    (pagesNumber, list)
  }

  def getEntriesBySearch(user: Option[User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[Entry]) = {
    require(itemsOnPage != 0)
    var list = Seq(entry1, entry2, entry3) filter { _.title contains query }
    val pagesNumber = list.size / itemsOnPage + (if (list.size % itemsOnPage != 0) 1 else 0)
    list = list drop (page * itemsOnPage) take itemsOnPage
    (pagesNumber, list)
  }

  def getEntry(user: Option[User], id: Long): Option[Entry] = Seq(entry1, entry2, entry3) find { _.id == id }
}
