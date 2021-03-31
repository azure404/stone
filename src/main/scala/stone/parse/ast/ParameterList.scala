package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class ParameterList(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes) {
  var offsets: Array[Int] = _

  def name(i: Int): String = child(i).asInstanceOf[ASTLeaf].token.getText
  def size: Int = numChildren
}
