package models

import java.util.Date

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

case class Entry(author: User, title: String, content: String, tags: List[Tag], var comments: List[Comment]) extends Entity {
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