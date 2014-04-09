package models

trait Dao {
  def getUser(name: String, password: String): Option[User]

  def getUser(id: Long): Option[User]

  def getEntries(user: Option[User], filter: Filter, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getTag(id: Long): Option[Tag]

  def getEntriesByTag(user: Option[User], tag: Tag, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntriesBySearch(user: Option[User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntry(id: Long): Option[Entry]
}
