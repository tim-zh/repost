package models

trait Dao {
  def getUser(name: String, password: String): Option[User]

  def getUser(id: Long): Option[User]

  def getEntries(user: Option[User], filter: Filter, page: Int, itemsOnPage: Int): (Int, Seq[Entry])
}
