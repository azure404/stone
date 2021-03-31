package stone.scope

import java.util

import stone.scope.ext.EnvironmentExt._

class ResizableArrayEnvironment extends ArrayEnvironment {

  val names: Symbols = new Symbols

  override def symbols(): Symbols = names

  init(10, null)

  override def get(name: String): Option[Any] = {
    val i: Option[Int] = names.find(name)
    if(i isEmpty){
      if(outer == null)
        Option.empty
      else
        outer.get(name)
    } else {
      Option(values(i.get))
    }
  }

  override def put(name: String, value: Any): Unit = {
    var e: Environment = where(name)
    if(e == null)
      e = this
    e putNew(name, value)
  }

  def putNew(name: String, value: Any): Unit = {
    assign(names.putNew(name), value)
  }

  def where(name: String): Environment = {
    if(names.find(name) isEmpty)
      this
    else if(outer == null)
      null
    else
      outer where name
  }

  override def put(nest: Int, index: Int, value: Any): Unit = {
    if(nest == 0)
      assign(index, value)
    else
      super.put(nest, index, value)
  }

  def assign(index: Int, value: Any): Unit = {
    if(index >= values.length){
      var newLen: Int = values.length << 1
      if(index >= newLen)
        newLen = index + 1
      values = Array.copyOf(values, newLen)
    }
    values(index) = value
  }
}
