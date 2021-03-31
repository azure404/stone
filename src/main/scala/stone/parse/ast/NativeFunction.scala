package stone.parse.ast

import java.lang.reflect.Method

import stone.exception.StoneException

import scala.util.Try

class NativeFunction(name: String, method: Method) {

  val numParams: Int = method.getParameterTypes length

  def numOfParameters: Int = numParams

  def invoke(tree: ASTree, args: Any*): Any = {
    if(args isEmpty)
      Try(method invoke null) getOrElse(throw new StoneException(s"bad native function call: $name", tree))
    else
      Try(method invoke(null, args: _*)) getOrElse(throw new StoneException(s"bad native function call: $name", tree))
  }

  override def toString: String = s"<native: ${hashCode()}>"
}
