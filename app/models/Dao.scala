package models

import java.util.Date

trait Dao {
  val defaultItemsOnPage = 10
  val rowNumberInPopupSearch = 5

  private[models] def getPagesNumber(size: Long, itemsOnPage: Long): Long = {
    require(itemsOnPage != 0)
    Math.ceil(size / itemsOnPage.asInstanceOf[Double]).asInstanceOf[Long]
  }

  def getUser(name: String, password: String): Option[User]

  def getUser(id: Long): Option[User]

  def getUser(name: String): Option[User]

  def getEntries(user: Option[User], page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getTag(title: String): Option[Tag]

  def getEntriesByTag(user: Option[User], tag: Tag, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntriesBySearch(user: Option[User], query: String, page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntriesBySearch(user: Option[User], query: String, from: Option[Date], to: Option[Date],
                         users: Seq[User], tags: Seq[Tag], page: Int, itemsOnPage: Int): (Long, Seq[Entry])

  def getEntry(user: Option[User], id: Long): Option[Entry]

  def addUser(name: String, password: String): User

  def addEntry(author: User, title: String, tags: Seq[Tag], openForAll: Boolean, content: String): Entry

  def getTagsByTitles(titles: Seq[String], addNew: Boolean): Seq[Tag]

  def getTagsBySearch(query: String): Seq[Tag]

  def getUsersByNames(names: Seq[String]): Seq[User]

  def getUsersBySearch(query: String): Seq[User]

  def addComment(author: User, entry: Entry, content: String): Comment

  def updateEntry(user: Option[User], id: Long, title: String, tags: Seq[Tag], openForAll: Boolean,
                  content: String): Option[Entry]

  def deleteEntry(user: Option[User], id: Long): Boolean

  def deleteComment(user: Option[User], id: Long): Boolean

  def deleteUser(user: Option[User]): Boolean

  def addFavoriteTag(user: Option[User], title: String): Boolean

  def removeFavoriteTag(user: Option[User], title: String): Boolean

  def updateUser(id: Long, password: String, entryListType: models.ListType.LT, dateFormat: String, itemsOnPage: Int, codeTheme: Int): Option[User]
}