package stone.interpreter

import stone.parse.ast.OptClassInfo

class OptStoneObject {
  class AccessException extends Exception
  var class_info: OptClassInfo = _
  var fields: Array[Any] = _

  def this(ci: OptClassInfo, size: Int) = {
    this()
    class_info = ci
    fields = new Array[Any](size)
  }

  def classInfo: OptClassInfo = class_info
  def method(index: Int): Any = class_info.method(this, index)
  def read(name: String): Any = {
    var i: Option[Int] = class_info.fieldIndex(name)
    if(i nonEmpty)
      return fields(i get)
    else {
      i = class_info.methodIndex(name)
      if(i nonEmpty)
        return method(i get)
    }
    throw new AccessException
  }
  def read(index: Int): Any = fields(index)
  def write(name: String, value: Any): Unit = {
    val i: Option[Int] = class_info.fieldIndex(name)
    if(i isEmpty)
      throw new AccessException
    else
      fields(i get) = value
  }
  def write(index: Int, value: Any): Unit = fields(index) = value
}
