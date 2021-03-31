package stone.scope

import stone.exception.StoneException
import stone.scope.ext.EnvironmentExt._

class ArrayEnvironment extends Environment {
  var values: Array[Any] = _
  var outer: Environment = _

  def this(size: Int, outer: Environment) = {
    this()
    init(size, outer)
  }

  def init(size: Int, outer: Environment) = {
    this.outer = outer
    values = new Array[Any](size)
  }

  def symbols(): Symbols = throw new StoneException("no symbols")

  def get(nest: Int, index: Int): Option[Any] = {
    if(nest == 0)
      Option(values(index))
    else if(outer == null)
      Option.empty
    else
      outer get(nest - 1, index)
  }

  def put(nest: Int, index: Int, value: Any): Unit = {
    if(nest == 0)
      values(index) = value
    else if(outer == null)
      throw new StoneException("no outer environment")
    else
      outer put(nest - 1, index, value)
  }

  override def put(name: String, value: Any): Unit = error(name)

  override def get(name: String): Option[Any] = {
    error(name)
    null
  }

  def error(name: String): Unit = {
    throw new StoneException(s"cannot access by name: $name")
  }
}
