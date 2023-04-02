package u04lab.code

import u04lab.code.List.*

trait Item {
	def code: Int

	def name: String

	def tags: Seq[String]
}

object Item:
	def apply(code: Int, name: String, tags: String*): Item =
		ItemImpl(code, name, tags: _*)

	private case class ItemImpl(code: Int, name: String, tags: String*) extends Item

end Item

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse {
	/**
	 * Stores an item in the warehouse.
	 *
	 * @param item the item to store
	 */
	def store(item: Item): Unit

	/**
	 * Searches for items with the given tag.
	 *
	 * @param tag the tag to search for
	 * @return the list of items with the given tag
	 */
	def searchItems(tag: String): List[Item]

	/**
	 * Retrieves an item from the warehouse.
	 *
	 * @param code the code of the item to retrieve
	 * @return the item with the given code, if present
	 */
	def retrieve(code: Int): Option[Item]

	/**
	 * Removes an item from the warehouse.
	 *
	 * @param item the item to remove
	 */
	def remove(item: Item): Unit

	/**
	 * Checks if the warehouse contains an item with the given code.
	 *
	 * @param itemCode the code of the item to check
	 * @return true if the warehouse contains an item with the given code, false otherwise
	 */
	def contains(itemCode: Int): Boolean
}

object Warehouse {
	def apply(): Warehouse =
		WarehouseImpl()

	private class WarehouseImpl() extends Warehouse:
		private var _items: List[Item] = Nil()

		def store(item: Item): Unit = _items = Cons(item, _items)

		def searchItems(tag: String): List[Item] =
			List.filter(_items)(i => i.tags.contains(tag))

		def retrieve(code: Int): Option[Item] =
			List.find(_items)(_.code == code)

		def remove(item: Item): Unit =
			_items = filter(_items)(_ != item)

		def contains(itemCode: Int): Boolean =
			List.contains(List.map(_items)(_.code), itemCode)
}

@main def mainWarehouse(): Unit =
	val warehouse = Warehouse()

	val dellXps = Item(33, "Dell XPS 15", "notebook")
	val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
	val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")

	println(warehouse.contains(dellXps.code)) // false
	warehouse.store(dellXps) // side effect, add dell xps to the warehouse
	println(warehouse.contains(dellXps.code)) // true
	warehouse.store(dellInspiron) // side effect, add dell inspiron to the warehouse
	warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
	println(warehouse.searchItems("mobility")) // List(xiaomiMoped)
	println(warehouse.searchItems("notebook")) // List(dellXps, dellInspiron)
	println(warehouse.retrieve(11)) // None
	println(warehouse.retrieve(dellXps.code)) // Some(dellXps)
	println(warehouse.remove(dellXps)) // side effect, remove dell xps from the warehouse
	println(warehouse.retrieve(dellXps.code)) // None