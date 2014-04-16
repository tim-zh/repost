package models

import java.util.Date

sealed trait Entity {
  def id: Long
  def version: Long
}

trait User extends Entity {
  def name: String
  def password: String
  def entries: Iterable[Entry]
  def comments: Iterable[Comment]

  override def toString: String = name
}

trait Entry extends Entity {
  def author: User
  def title: String
  def content: String
  def date: Date
  def openForAll: Boolean
  def tags: Iterable[Tag]
  def comments: Iterable[Comment]

  override def toString: String = title
}

trait Comment extends Entity {
  def author: User
  def date: Date
  def content: String
  def entry: Entry
}

trait Tag extends Entity {
  def title: String

  override def toString: String = title
}