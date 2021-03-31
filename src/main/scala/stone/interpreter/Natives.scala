package stone.interpreter

import java.lang.reflect.Method

import javax.swing.JOptionPane
import stone.exception.StoneException
import stone.parse.ast.NativeFunction
import stone.scope.Environment

import scala.util.Try

class Natives {

  def environment(env: Environment): Environment = {
    appendNatives(env)
    env
  }

  def appendNatives(env: Environment): Unit = {
    append(env, "print", classOf[Natives], "print", classOf[Any])
    append(env, "read", classOf[Natives], "read")
    append(env, "length", classOf[Natives], "length", classOf[String])
    append(env, "toInt", classOf[Natives], "toInt", classOf[Any])
    append(env, "currentTime", classOf[Natives], "currentTime")
  }

  def append(env: Environment, name: String, clazz: Class[_],
             methodName: String, params: Class[_]*): Unit ={
    val m: Method = Try(clazz.getMethod(methodName, params: _*)) getOrElse(
      throw new StoneException(s"cannot found a native function: $methodName"))
    env put(name, new NativeFunction(methodName, m))
  }
}
object Natives {
  def print(obj: Any): Long = {
    println(obj toString)
    0
  }

  def read(): String = JOptionPane.showInputDialog(null)

  def length(str: String): Long = str length

  def toInt(obj: Any): Long = {
    obj match {
      case s: String => s toLong
      case i: Int => i
      case _ => throw new NumberFormatException(obj toString)
    }
  }

  val startTime: Long = System.currentTimeMillis()
  def currentTime(): Long = System.currentTimeMillis() - startTime
}
