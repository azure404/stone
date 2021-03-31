package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class Arguments(nodes: ArrayBuffer[ASTree]) extends Postfix(nodes) {
  def size: Int = numChildren
}
