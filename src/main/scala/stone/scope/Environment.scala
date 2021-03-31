package stone.scope

trait Environment {
  def put(name: String, value: Any): Unit
  def get(name: String): Option[Any]
}
