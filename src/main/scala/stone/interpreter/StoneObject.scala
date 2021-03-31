package stone.interpreter

import stone.scope.Environment
import stone.scope.ext.EnvironmentExt._

class StoneObject(env: Environment) {

  class AccessException extends Exception

  override def toString: String = s"<object: $hashCode>"

  def read(member: String): Any = {
    getEnv(member).get(member).get
  }

  def write(member: String, value: Any): Unit = {
    getEnv(member).putNew(member, value)
  }

  def getEnv(member: String): Environment = {
    val e: Environment = env where member
    if(e != null && e == env)
      e
    else
      throw new AccessException
  }
}
