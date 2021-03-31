package stone.interpreter

import stone.exception.StoneException
import stone.parse.ast._
import stone.scope.{ArrayEnvironment, Environment, MemberSymbols, NestedEnvironment, SymbolThis, Symbols}
import stone.scope.ext.EnvironmentExt._
import stone.interpreter.Evaluator.BasicEvaluator._
import stone.interpreter.Evaluator.FuncEvaluator._
import stone.interpreter.Evaluator.ClosureEvaluator._
import stone.interpreter.Evaluator.ClassEvaluator._
import stone.interpreter.Evaluator.ArrayEvaluator._

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object Evaluator {

  implicit class ASTreeEvalExt(t: ASTree) {
    def eval(env: Environment): Any = {
      t match {
        case a: ASTLeaf => a eval env
        case a: ASTList => a eval env
        case _ => throw new StoneException(s"cannot eval: ${t.toString}", t)
      }
    }

    def lookup(symbols: Symbols): Unit = {
      t match {
        case d: DefStmt => d lookup symbols
        case f: FunExpr => f lookup symbols
        case p: ParameterList => p lookup symbols
        case n: Name => n lookup symbols
        case b: BinaryExpr => b lookup symbols
        case c: ClassStmt => c lookup symbols
        case l: ASTList => l.children foreach(t => t lookup symbols)
        case _ =>
      }
    }
  }

  implicit class ASTLeafEvalExt(t: ASTLeaf) {
    def eval(env: Environment): Any = {
      t match {
        case e: Name => e eval env
        case e: NumberLiteral => e eval env
        case e: StringLiteral => e eval env
        case _ => throw new StoneException(s"cannot eval: ${t.toString}", t)
      }
    }
  }

  implicit class ASTListEvalExt(t: ASTList) {
    def eval(env: Environment): Any = {
      t match {
        case e: BinaryExpr => e eval env
        case e: NegativeExpr => e eval env
        case e: PrimaryExpr => e eval env
        case e: BlockStmt => e eval env
        case e: IfStmt => e eval env
        case e: WhileStmt => e eval env
        case e: DefStmt => e eval env
        case e: FunExpr => e eval env
        case e: ClassStmt => e eval env
        case e: ArrayLiteral => e eval env
        case _: NullStmt =>
        case _ => throw new StoneException(s"cannot eval: ${t.toString}", t)
      }
    }
  }


  object BasicEvaluator {

    implicit class BinaryExprEvalExt(t: BinaryExpr) {
      def lookup(symbols: Symbols): Unit = {
        if("=".equals(t.operator)){
          t.left match {
            case n: Name =>
              n lookupForAssign symbols
              t.right lookup symbols
              return
            case _ =>
          }
        }
        t.left lookup symbols
        t.right lookup symbols
      }

      def eval(env: Environment): Any = {
        t operator match {
          case "=" => computeAssign(env, t.right eval env)
          case _ => computeOp(t.left eval env, t operator, t.right eval env)
        }
      }

      def computeAssign(env: Environment, rValue: Any): Any = {
        t left match {
          //支持对象操作
          case p: PrimaryExpr =>
            if(p hasPostfix 0){
              p.postfix(0) match {
                case d: Dot =>
                  val t: Any = p evalSubExpr(env, 1)
                  t match {
                    case obj: OptStoneObject => setField(obj, d, rValue)
                    case _ =>
                  }
                case r: ArrayRef =>
                  val array: Any = p evalSubExpr(env, 1)
                  array match {
                    case a: Array[Any] =>
                      r.index eval env match {
                        case i: Int => a(i) = rValue; rValue
                        case _ => throw new StoneException("bad array access", t)
                      }
                    case _ => throw new StoneException("bad array access", t)
                  }
                case _ => throw new StoneException("bad array access", t)
              }

            }
          case n: Name =>
            n evalForAssign (env, rValue)
//            env put(n.name, rValue)
            rValue
          case _ => throw new StoneException("bad assignment", t)
        }
      }

      def setField(obj: OptStoneObject, expr: Dot, rvalue: Any): Any = {
        val name: String  = expr name;
        Try({
          obj write(name, rvalue)
          rvalue
        }).getOrElse(throw new StoneException(s"bad member access ${t.location}: $name"))
      }

      def computeOp(lValue: Any, op: String, rValue: Any): Any = {
        lValue match {
          case l: Long if rValue.isInstanceOf[Long] => computeNum(l, op, rValue.asInstanceOf[Long])
          case _ =>
            op match {
              case "+" => lValue.toString.concat(rValue.toString)
              case "==" =>  if (lValue == null) {
                if (rValue == null) true else false
              } else {
                lValue equals rValue
              }
              case _ => throw new StoneException("bad type", t)
            }
        }
      }

      def computeNum(lValue: Long, op: String, rValue: Long): Any = {
        op match {
          case "+" => lValue + rValue
          case "-" => lValue - rValue
          case "*" => lValue * rValue
          case "/" => lValue / rValue
          case "%" => lValue % rValue
          case "==" => lValue == rValue
          case ">" => lValue > rValue
          case "<" => lValue < rValue
          case _ => throw new StoneException("bad operator", t)
        }
      }
    }

    implicit class BlockStmtEvalExt(b: BlockStmt) {
      def eval(env: Environment): Any = {
        var res: Any = 0
        b.children foreach(t => { if(!t.isInstanceOf[NullStmt]) res = t eval env})
        res
      }
    }

    implicit class IfStmtEvalExt(s: IfStmt) {
      def eval(env: Environment): Any = {
        val c: Any = s.condition eval env
        c match {
          case _: Boolean if c.asInstanceOf[Boolean] => s.thenBlock eval env
          case _: Boolean if !c.asInstanceOf[Boolean] => if(s.elseBlock == null) 0 else s.elseBlock eval env
          case _ => throw new StoneException("condition must be boolean", s)
        }
      }
    }

    implicit class NameEvalExt(n: Name) {
      def lookup(symbols: Symbols): Unit = {
        var loc: Symbols.Location = symbols.get(n.name)
        if(loc == null)
          throw new StoneException(s"undefined name $n.name", n)
        else{
          n.nest = loc.nest
          n.index = loc.index
        }
      }

      def lookupForAssign(symbols: Symbols): Unit = {
        val loc: Symbols.Location = symbols put n.name
        n.nest = loc.nest
        n.index = loc.index
      }

      def eval(env: Environment): Any = {
        if(n.index == n.UNKNOWN)
          env.get(n.name) getOrElse(throw new StoneException(s"undefined name: ${n.name}", n))
        else if(n.nest == MemberSymbols.FIELD)
          getThis(env) read n.index
        else if(n.nest == MemberSymbols.METHOD)
          getThis(env) method n.index
        else
          env get(n.nest, n.index) getOrElse(throw new StoneException(s"undefined name: ${n.name}", n))
      }

      def evalForAssign(env: Environment, value: Any): Unit = {
        if(n.index == n.UNKNOWN)
          env put(n.name, value)
        else if(n.nest == MemberSymbols.FIELD)
          getThis(env) write(n.index, value)
        else if(n.nest == MemberSymbols.METHOD)
          throw new StoneException(s"cannot update a method: ${n.name}", n)
        else
          env put(n.nest, n.index, value)
      }

      def getThis(env: Environment): OptStoneObject = {
        env.get(0, 0).get.asInstanceOf[OptStoneObject]
      }
    }

    implicit class NegativeExprEvalExt(n: NegativeExpr) {
      def eval(env: Environment): Any = {
        val op: Any = n.operand eval env
        op match {
          case _: Long => -op.asInstanceOf[Long]
          case _ => throw new StoneException("bad type for -", n)
        }
      }
    }

    implicit class NumberLiteralEvalExt(n: NumberLiteral) {
      def eval(env: Environment): Any = n.value
    }

    implicit class StringLiteralEvalExt(s: StringLiteral){
      def eval(env: Environment): Any = s.value
    }

    implicit class WhileStmtEvalExt(s: WhileStmt) {
      def eval(env: Environment): Any = {
        var res: Any = 0
        while(true){
          val c: Any = s.condition eval env
          c match {
            case _: Boolean if !c.asInstanceOf[Boolean] => return res
            case _: Boolean if c.asInstanceOf[Boolean] => res = s.body eval env
            case _ => throw new StoneException("condition must be boolean", s)
          }
        }
      }
    }

  }

  object FuncEvaluator {

    implicit class DefStmtEvalExt(s: DefStmt) {

      def lookup(symbols: Symbols): Unit = {
        s.index = symbols putNew s.name
        s.size = ClosureEvaluator.lookup(symbols, s.parameters, s.body)
      }

      def eval(env: Environment): Any = {
//        env putNew(s.name, new Func(s.parameters, s.body, env))
        env put(0, s.index, new OptFunc(s.parameters, s.body, env, s.size))
        s.name
      }

      def locals: Int = s.size

      def lookupAsMethod(syms: Symbols): Unit = {
        val newSyms: Symbols = new Symbols(syms)
        newSyms.putNew(SymbolThis.NAME)
        s.parameters lookup newSyms
        s.body lookup newSyms
        s.size = newSyms.size
      }
    }

    implicit class PrimaryExprEvalExt(p: PrimaryExpr){
      def operand: ASTree = p child 0
      def postfix(nest: Int): Postfix = p.child(p.numChildren - nest - 1).asInstanceOf[Postfix]
      def hasPostfix(nest: Int): Boolean = p.numChildren - nest > 1
      def eval(env: Environment): Any = evalSubExpr(env, 0)
      def evalSubExpr(env: Environment, nest: Int): Any = {
        if(hasPostfix(nest)){
          val target: Any = evalSubExpr(env, nest + 1)
          postfix(nest) eval(env, target)
        } else {
          operand eval env
        }
      }
    }

    implicit class PostfixEvalExt(p: Postfix){
      def eval(env: Environment, value: Any): Any = {
        p match {
          case a: Arguments => a eval(env, value)
          case d: Dot => d eval(env, value)
          case r: ArrayRef => r eval(env, value)
          case _ => throw new StoneException("bad type of postfix", p)
        }
      }
    }

    implicit class ArgumentsEvalExt(a: Arguments){
      def eval(callerEnv: Environment, value: Any): Any = {
        value match {
          case f: Func =>
            if(f.parameters.size != a.size)
              throw new StoneException("bad number of arguments", a)
            val newEnv: Environment = f.makeEnv()
            var num: Int = 0
            a.children foreach(t => {
              f.parameters eval(newEnv, num, t eval callerEnv)
              num += 1
            })
            f.body eval newEnv
          case n: NativeFunction =>
            if(a.size != n.numOfParameters)
              throw new StoneException("bad number of arguments", a)
            val args: Array[Any] = a.children.collect(t => t eval callerEnv).toArray
            n invoke(a, args: _*)
          case _ => throw new StoneException("bad function", a)
        }
      }
    }

    implicit class ParameterListEvalExt(ps: ParameterList){

      def lookup(symbols: Symbols): Unit = {
        val s: Int = ps.size
        ps.offsets = new Array[Int](s)
        var i: Int = 0
        while(i < s){
          ps.offsets(i) = symbols putNew ps.name(i)
          i += 1
        }
      }

      def eval(env: Environment, index: Int, value: Any): Unit = {
//        env putNew(ps.name(index), value)
        env put(0, ps.offsets(index), value)
      }
    }

  }

  object ClosureEvaluator{
    implicit class FunExprEvalExt(f: FunExpr){

      def lookup(symbols: Symbols): Unit = {
        f.size = ClosureEvaluator.lookup(symbols, f.parameters, f.body)
      }

      def eval(env: Environment): Any = {
        new OptFunc(f.parameters, f.body, env, f.size)
      }
    }

    def lookup(symbols: Symbols, params: ParameterList, body: BlockStmt): Int ={
      val newSymbols: Symbols = new Symbols(symbols)
      params lookup newSymbols
      body lookup newSymbols
      newSymbols.size
    }
  }

  object ClassEvaluator{
    implicit class ClassStmtEvalExt(c: ClassStmt){
      def lookup(symbols: Symbols): Unit = {}

      def eval(env: Environment): Any = {
        val methodNames: Symbols = new MemberSymbols(MemberSymbols.METHOD, env symbols)
        val fieldNames: Symbols = new MemberSymbols(MemberSymbols.FIELD, methodNames)
        val ci: OptClassInfo = new OptClassInfo(c, env, fieldNames, methodNames)
        env put(c.name, ci)
        val methods: ArrayBuffer[DefStmt] = new ArrayBuffer[DefStmt]()
        if(ci.superClass != null)
          ci.superClass.copyTo(fieldNames, methodNames, methods)
        val newSyms: Symbols = new SymbolThis(fieldNames)
        c.body lookup(newSyms, methodNames, fieldNames, methods)
        ci.setMethod(methods)
        c name
//        val ci: ClassInfo = new ClassInfo(c, env)
//        env.put(c.name, ci)
//        c name
      }
    }

    implicit class ClassBodyEvalExt(c: ClassBody){

      def eval(env: Environment): Any = {
        c.children foreach(t => if(!t.isInstanceOf[DefStmt]) t eval env)
        null
      }

      def lookup(syms: Symbols, methodNames: Symbols, fieldNames: Symbols, methods: ArrayBuffer[DefStmt]): Unit = {
        c.children foreach {
          case d: DefStmt =>
            val oldSize: Int = methodNames.size
            val i: Int = methodNames putNew d.name
            if (i >= oldSize)
              methods addOne d
            else
              methods update(i, d)
            d lookupAsMethod fieldNames
          case t => t lookup syms
        }
      }
    }

    implicit class DotEvalExt(d: Dot){
      def eval(env: Environment, value: Any): Any = {
        val member: String = d.name
        value match {
          case c: OptClassInfo =>
            if("new" equals member){
              val e: ArrayEnvironment = new ArrayEnvironment(1, c.environment)
              val so: OptStoneObject = new OptStoneObject(c, c.size)
              e put(0, 0, so)
              initObject(c, so, e)
              so
            }
          case s: OptStoneObject =>
            Try(s read member) getOrElse(throw new StoneException(s"bad member access: $member", d))
          case _ => throw new StoneException(s"bad member access: $member", d)
        }
      }

      def initObject(ci: OptClassInfo, obj: OptStoneObject, env: Environment): Unit = {
        if(ci.superClass != null)
          initObject(ci.superClass, obj, env)
        ci.body eval env
      }
    }

  }

  object ArrayEvaluator{
    implicit class ArrayLiteralEvalExt(a: ArrayLiteral){
      def eval(env: Environment): Any = {
        val len: Int = a.numChildren
        val res: Array[Any] = new Array(len)
        var i: Int = 0
        a.children foreach(t => {res(i) = t eval env; i += 1})
        res
      }
    }

    implicit class ArrayRefEvalExt(r: ArrayRef){
      def eval(env: Environment, value: Any): Any = {
        value match {
          case a: Array[Any] =>
            r.index eval env match {
              case i: Long => a(i toInt)
              case _ => throw new StoneException("bad array access", r)
            }
          case _ => throw new StoneException("bad array access", r)
        }
      }
    }
  }
}
