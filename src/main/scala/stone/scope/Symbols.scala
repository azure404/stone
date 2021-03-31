package stone.scope

import scala.collection.mutable

class Symbols {

  var outer: Symbols = _
  var table: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()

  def this(outer: Symbols) = {
    this()
    this.outer = outer
  }

  def size: Int = table.size
  def append(s: Symbols): Unit = table.addAll(s.table)
  def find(key: String): Option[Int] = table.get(key)
  def get(key: String): Symbols.Location = get(key, 0)
  def get(key: String, nest: Int): Symbols.Location = {
    val index: Option[Int] = find(key)
    if(index isEmpty){
      if(outer == null)
        null
      else
        outer.get(key, nest + 1)
    } else {
      new Symbols.Location(nest, index.get)
    }
  }
  def putNew(key: String): Int = {
    val index: Option[Int] = find(key)
    if(index isEmpty)
      add(key)
    else
      index get
  }
  def put(key: String): Symbols.Location = {
    val loc: Symbols.Location = get(key)
    if(loc == null)
      new Symbols.Location(0, add(key))
    else
      loc
  }

  def add(key: String): Int = {
    val i: Int = table.size
    table.put(key, i)
    i
  }
}
object Symbols{
  class Location(var nest: Int, var index: Int)
}

