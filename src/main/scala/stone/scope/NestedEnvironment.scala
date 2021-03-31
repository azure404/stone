package stone.scope

import stone.scope.ext.EnvironmentExt._

class NestedEnvironment(var outer: Environment) extends BasicEnvironment {

  def this() = this(null)

  override def get(name: String): Option[Any] = {
    val res: Option[Any] = super.get(name)
    if(res.isEmpty && outer != null)
      outer.get(name)
    else
      res
  }

  override def put(name: String, value: Any): Unit = {
    var e: Environment = where(name)
    if(e == null)
      e = this
    e putNew(name, value)
  }

  def putNew(name: String, value: Any) = super.put(name, value)

  def where(name: String): Environment ={
    if(super.get(name) nonEmpty)
      this
    else if(outer == null)
      null
    else
      outer where name
  }

}
