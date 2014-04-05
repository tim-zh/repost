package models

import java.util.Date

object TestModel {
  val tag1 = Tag("tag1")
  val tag2 = Tag("tag2")
  val user1 = User("user1", "pass", null, null)
  val entry1 = Entry("entry1", "content1<b/r>bla1", List(tag1, tag2), null)
  val entry2 = Entry("entry2", "content2<br/>bla2", List(tag1, tag2), null)
  val comment1 = Comment(user1, new Date, "comment1<br/>c1", entry1)
  val comment2 = Comment(user1, new Date, "comment2<br/>c2", entry1)
  val comment3 = Comment(user1, new Date, "comment3<br/>c3", entry1)
  val comment4 = Comment(user1, new Date, "comment4<br/>c4", entry2)
  entry1.comments = List(comment1, comment2, comment3)
  entry2.comments = List(comment4)
  user1.entries = List(entry1, entry2)
  user1.comments = List(comment1, comment2, comment3, comment4)
}

sealed abstract class Entity {
  var id: Long = _
  var version: Long = _

  def getTitle: String = ""

  def getDescription: String = ""

  override def toString: String = getTitle
}

case class User(name: String, password: String, var entries: List[Entry], var comments: List[Comment]) extends Entity {
  override def getTitle = name
}

case class Entry(title: String, content: String, tags: List[Tag], var comments: List[Comment]) extends Entity {
  override def getTitle = title

  override def getDescription: String = title.take(128)
}

case class Comment(author: User, date: Date, content: String, entry: Entry) extends Entity

case class Tag(title: String) extends Entity {
  override def getTitle: String = title
}

case class Filter(title: String, tags: List[Tag], authors: List[User], startDate: Date, endDate: Date) extends Entity {
  override def getTitle: String = title
}