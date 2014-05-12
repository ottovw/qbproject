package org.qbproject.api.persistence

import play.api.libs.json.JsObject
import scala.concurrent.Future

// TODO: collection trait seems to be very mongo specific, move to mongo?
trait QBCollection {
  
  type ID = String

  /**
   * Returns the number of items contained in the collection.
   *
   * @return a future containing the number of items within the collection
   */
  def getCount: Future[Int]

  /**
   * Returns all items contained in the collection.
   *
   * @return a future containing a list of all items within the collection
   */
  def getAll(skip: Int = 0, limit: Int = 100): Future[List[JsObject]]

  /**
   * Find an item by its ID.
   *
   * @param id
   *           the ID of the item to be found
   *
   * @return a future containing an Option that holds the result
   */
  def getById(id: ID) : Future[Option[JsObject]]

  /**
   * Find the first item within the collection that matches the query.
   *
   * @param query
   *           the query object containing the criteria to be fulfilled
   *
   * @return a future containing an Option that holds the result
   */
  def findOne(query: JsObject) : Future[Option[JsObject]]

  /**
   * Find all items within the collection that match the query.
   *
   * @param query
   *           the query object containing the criteria to be fulfilled
   *
   * @return a future containing a list of all items that match the query
   */
  def find(query: JsObject, skip: Int = 0, limit: Int = 100)  : Future[List[JsObject]]

  /**
   * Update an item by its ID.
   *
   * @param id
   *           the ID of the item to be updated
   * @param newValue
   *           the new item value
   *
   * @return a future containing the updated item
   */
  def update(id: ID, newValue: JsObject): Future[JsObject]

  /**
   * Creates an item within the collection.
   *
   * @param obj
   *           the item to be created
   *
   * @return the created item
   */
  def create(obj: JsObject): Future[JsObject]
  

}