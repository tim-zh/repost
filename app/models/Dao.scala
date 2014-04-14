package models

trait Dao {
  def getUser(name: String, password: String): Option[User]

  def getUser(id: Long): Option[User]

  def getEntries(user: Option[User], page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getTag(title: String): Option[Tag]

  def getFilter(title: String): Option[Filter]

  def getEntriesByTag(user: Option[User], tag: Tag, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntriesByFilter(user: Option[User], filter: Filter, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntriesBySearch(user: Option[User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntry(user: Option[User], id: Long): Option[Entry]
}
