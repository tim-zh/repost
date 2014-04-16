package models.test

import java.util.Date

sealed abstract class Entity {
  var _id: Long = _
  var _version: Long = _

  def id: Long = _id
  def version: Long = _version
}

case class User(name: String,
                password: String,
                var entries: Seq[models.Entry],
                var comments: Seq[models.Comment]) extends Entity with models.User

case class Entry(author: models.User,
                 title: String,
                 content: String,
                 date: Date,
                 openForAll: Boolean,
                 tags: Seq[models.Tag],
                 var comments: Seq[models.Comment]) extends Entity with models.Entry

case class Comment(author: models.User,
                   date: Date,
                   content: String,
                   entry: models.Entry) extends Entity with models.Comment

case class Tag(title: String) extends Entity with models.Tag