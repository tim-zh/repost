package models

import java.util.Date

object TestDao extends Dao {
  val tag1 = Tag("tag1")
  tag1.id = 1
  val tag2 = Tag("tag2")
  tag2.id = 2
  val user1 = User("user1", "pass", null, null)
  user1.id = 1
  val entry1 = Entry(user1, "entry1", "content1<b/r>bla1", Seq(tag1, tag2), null)
  val entry2 = Entry(user1, "entry2", "content2<br/>bla2", Seq(tag1, tag2), null)
  val comment1 = Comment(user1, new Date, "comment1<br/>c1", entry1)
  val comment2 = Comment(user1, new Date, "comment2<br/>c2", entry1)
  val comment3 = Comment(user1, new Date, "comment3<br/>c3", entry1)
  val comment4 = Comment(user1, new Date, "comment4<br/>c4", entry2)
  entry1.comments = Seq(comment1, comment2, comment3)
  entry2.comments = Seq(comment4)
  user1.entries = Seq(entry1, entry2)
  user1.comments = Seq(comment1, comment2, comment3, comment4)

  def getUser(name: String, password: String): Option[User] =
    if (name == TestDao.user1.name && password == TestDao.user1.password) Some(user1) else None

  def getUser(id: Long): Option[User] = if (id == user1.id) Some(user1) else None

  def getEntries(user: Option[User], filter: Filter, page: Int, itemsOnPage: Int): (Int, Seq[Entry]) = {
    var list = Seq(entry1, entry2)
    list = list drop (page * itemsOnPage) take itemsOnPage
    (1, list)
  }
}
