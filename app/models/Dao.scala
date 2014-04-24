package models

trait Dao {
  val numberOfTagsBySearch = 5

  def getUser(name: String, password: String): Option[User]

  def getUser(id: Long): Option[User]

  def getUser(name: String): Option[User]

  def getEntries(user: Option[User], page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getTag(title: String): Option[Tag]

  def getEntriesByTag(user: Option[User], tag: Tag, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntriesBySearch(user: Option[User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntry(user: Option[User], id: Long): Option[Entry]

  def addUser(name: String, password: String): User

  def addEntry(author: User, title: String, tags: Seq[Tag], openForAll: Boolean, content: String): Entry

  def getTagsByTitles(titles: Seq[String], addNew: Boolean): Seq[Tag]

  def getTagsBySearch(query: String): Seq[Tag]

  def addComment(author: User, entry: Entry, content: String): Comment

  def updateEntry(user: Option[User], id: Long, title: String, tags: Seq[Tag], openForAll: Boolean,
                  content: String): Option[Entry]

  def deleteEntry(user: Option[User], id: Long): Boolean

  def deleteComment(user: Option[User], id: Long): Boolean

  def deleteUser(user: Option[User], id: Long): Boolean
}