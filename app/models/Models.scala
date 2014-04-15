package models

import java.util.Date

sealed trait Entity {
  var id: Long = _
  var version: Long = _
}

case class User(name: String,
                password: String,
                var entries: Seq[Entry],
                var comments: Seq[Comment]) extends Entity {
  override def toString: String = name
}

case class Entry(author: User,
                 title: String,
                 content: String,
                 date: Date,
                 openForAll: Boolean,
                 tags: Seq[Tag],
                 var comments: Seq[Comment]) extends Entity {
  override def toString: String = title
}

case class Comment(author: User,
                   date: Date,
                   content: String,
                   entry: Entry) extends Entity

case class Tag(title: String) extends Entity {
  override def toString: String = title
}

case class Filter(title: String,
                  tags: Seq[Tag],
                  authors: Seq[User],
                  startDate: Option[Date],
                  endDate: Option[Date]) extends Entity {
  override def toString: String = title
}