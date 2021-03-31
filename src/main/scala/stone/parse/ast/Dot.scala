package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class Dot(nodes: ArrayBuffer[ASTree]) extends Postfix(nodes) {

  def name: String = child(0).asInstanceOf[ASTLeaf].token.getText

  override def toString: String = s".$name"
}
