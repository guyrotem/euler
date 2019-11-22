package euler.utils

class Memoizer[Key, Value](fetcher: Key => Value) {

  private val dataSource = scala.collection.mutable.Map[Key, Value]()

  def get(key: Key): Value = {
    dataSource.getOrElse(key, fetchAndStore(key))
  }

  private def fetchAndStore(key: Key) = {
    val r = fetcher(key)
    dataSource.put(key, r)
    r
  }
}