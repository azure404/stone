package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

class PrimaryExpr(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes)

  object PrimaryExpr{
    def create(c: ArrayBuffer[ASTree]): ASTree = {
      if(c.length == 1)
        c(0)
      else
        new PrimaryExpr(c)
  }
}


