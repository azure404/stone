package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class ArrayRef(nodes: ArrayBuffer[ASTree]) extends Postfix(nodes) {

  def index: ASTree = child(0)

  override def toString: String = s"[$index]"
}
