package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class BinaryExpr(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes) {

  def left: ASTree = nodes(0)
  def operator: String = nodes(1).asInstanceOf[ASTLeaf].token.getText
  def right: ASTree = nodes(2)

}
