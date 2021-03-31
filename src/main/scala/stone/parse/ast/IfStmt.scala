package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class IfStmt(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes) {

  def condition: ASTree = child(0)
  def thenBlock: ASTree = child(1)
  def elseBlock: ASTree = if(numChildren > 2) child(2) else null

  override def toString: String = {
    var str: String = s"(if $condition $thenBlock"
    if(elseBlock == null)
      str += ")"
    else
      str += s" else $elseBlock)"
    str
  }
}
