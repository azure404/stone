package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class ArrayLiteral(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes) {

  def size: Int = numChildren
}
