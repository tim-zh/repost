package models

import java.util.Date
import org.squeryl.KeyedEntity

sealed trait Entity {
  var id: Long = _
  var version: Long = _

  def getTitle: String = ""

  def getDescription: String = ""

  override def toString: String = getTitle
}

case class User(name: String,
                password: String,
                var entries: Seq[Entry],
                var comments: Seq[Comment]) extends KeyedEntity[Long] with Entity {
  override def getTitle = name
}

case class Entry(author: User,
                 title: String,
                 content: String,
                 date: Date,
                 openForAll: Boolean,
                 tags: Seq[Tag],
                 var comments: Seq[Comment]) extends KeyedEntity[Long] with Entity {
  override def getTitle = title

  override def getDescription: String = title take 128
}

case class Comment(author: User,
                   date: Date,
                   content: String,
                   entry: Entry) extends KeyedEntity[Long] with Entity

case class Tag(title: String) extends KeyedEntity[Long] with Entity {
  override def getTitle: String = title
}

case class Filter(title: String,
                  tags: Seq[Tag],
                  authors: Seq[User],
                  startDate: Option[Date],
                  endDate: Option[Date]) extends KeyedEntity[Long] with Entity {
  override def getTitle: String = title

  def this() = this("", Nil, Nil, Some(new Date), Some(new Date))
}