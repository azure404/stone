package stone.exception

import stone.parse.ast.{ASTree}

class StoneException(message: String) extends RuntimeException(message){
  def this(message: String, ast: ASTree) = this(message + " " + ast.location)
}
