package stone.parse.ast

import stone.interpreter.OptStoneObject
import stone.scope.{Environment, Symbols}

import scala.collection.mutable.ArrayBuffer
import stone.interpreter.Evaluator.FuncEvaluator._

class OptClassInfo(definition: ClassStmt,
                   env: Environment,
                   fields: Symbols,
                   methods: Symbols)
  extends ClassInfo(definition, env) {

  var methodDefs: Array[DefStmt] = _

  def size: Int = fields.size
  override def superClass: OptClassInfo = super_class.asInstanceOf[OptClassInfo]
  def copyTo(f: Symbols, m: Symbols, mList: ArrayBuffer[DefStmt]): Unit = {
    f.append(fields)
    m.append(methods)
    methodDefs foreach(d => mList.addOne(d))
  }
  def fieldIndex(name: String): Option[Int] = fields find name
  def methodIndex(name: String): Option[Int] = methods find name
  def method(self: OptStoneObject, index: Int): Any = {
    val defin: DefStmt = methodDefs(index)
    new OptMethod(defin.parameters, defin.body, environment, defin.locals, self)
  }
  def setMethod(methods: ArrayBuffer[DefStmt]): Unit = {
    methodDefs = methods.toArray
  }
}
