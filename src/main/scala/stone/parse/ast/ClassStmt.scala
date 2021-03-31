package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class ClassStmt(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes) {

  def name: String = child(0).asInstanceOf[ASTLeaf].token.getText
  def superClass: Option[String] = {
    if(numChildren < 3)
      Option.empty
    else
      Option(child(1).asInstanceOf[ASTLeaf].token.getText)
  }
  def body: ClassBody = child(numChildren - 1).asInstanceOf[ClassBody]

  override def toString: String = {
    var parent = "*"
    if(superClass nonEmpty)
      parent = superClass.get
    s"(class $name [extends $parent] $body)"
  }
}
