package stone.parse.ast

import scala.collection.mutable.ArrayBuffer

abstract class Postfix(nodes: ArrayBuffer[ASTree]) extends ASTList(nodes)
