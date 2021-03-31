package stone.scope

import scala.collection.mutable

class BasicEnvironment extends Environment {

  val values: mutable.HashMap[String, Any] = mutable.HashMap()

  override def put(name: String, value: Any): Unit = values.put(name, value)

  override def get(name: String): Option[Any] = values get name
}
