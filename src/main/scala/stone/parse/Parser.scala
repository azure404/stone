package stone.parse

import java.io.IOException
import java.lang.reflect.Method

import stone.exception.ParseException
import stone.lex.Lexer
import stone.lex.token.Token
import stone.parse.ast.{ASTLeaf, ASTList, ASTree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class Parser{

  var elements: ArrayBuffer[Parser#Element] = _
  var factory: Parser#Factory = _

  def this(p: Parser) = {
    this()
    elements = p.elements
    factory = p.factory
  }

  def this(c: Class[_ <: ASTree]) = {
    this()
    reset(c)
  }

  def parse(lexer: Lexer): ASTree = {
    val res: ArrayBuffer[ASTree] = ArrayBuffer()
    elements foreach(e => e parse(lexer, res))
    factory make res
  }

  def number(): Parser = { number(null) }
  def number(c: Class[_ <: ASTLeaf]): Parser = { elements addOne new NumToken(c); this }
  def identifier(r: mutable.Set[String]): Parser = { identifier(null, r) }
  def identifier(c: Class[_ <: ASTLeaf], r: mutable.Set[String]): Parser = { elements addOne new IdToken(r, c); this }
  def string(): Parser = { string(null)}
  def string(c: Class[_ <: ASTLeaf]): Parser = { elements addOne new StrToken(c); this }
  //token添加的符号在AST中占一个节点，sep添加的分隔符不会被包含在生成的AST中
  def token(pat: String*): Parser = { elements addOne new Leaf(pat toArray); this }
  def sep(pat: String*): Parser = { elements addOne new Skip(pat toArray); this }
  def ast(p: Parser): Parser = { elements addOne new Tree(p); this }
  def or(ps: Parser*): Parser = { elements addOne new OrTree(ps toArray); this }
  def option(p: Parser): Parser = { elements addOne new Repeat(p, true); this }
  def repeat(p: Parser): Parser = { elements addOne new Repeat(p, false); this }
  def maybe(p: Parser): Parser = {
    val p2: Parser = new Parser(p)
    p2.reset()
    elements addOne new OrTree(Array(p, p2))
    this
  }
  def expression(subexp: Parser, operators: Operators): Parser = { expression(subexp, operators) }
  def expression(c: Class[_ <: ASTree], subexp: Parser, operators: Operators): Parser = { elements addOne new Expr(c, subexp, operators); this }
  def reset(): Parser = { elements = ArrayBuffer(); this }
  def reset(c: Class[_ <: ASTree]): Parser = {
    factory = Factory getForASTList c;
    reset()
  }
  def insertChoice(p: Parser): Parser = {
    val e: Parser#Element = elements(0)
    e match {
      case tree: OrTree => tree insert p
      case _ => {
        val otherwise: Parser = new Parser(this)
        reset()
        or(p, otherwise)
      }
    }
    this
  }
  def matches(lexer: Lexer): Boolean = {
    (elements isEmpty) || (elements(0) matches lexer)
  }


  abstract class Element {
    def parse(lexer: Lexer, res: ArrayBuffer[ASTree])
    def matches(lexer: Lexer): Boolean
  }

  class Tree(parser: Parser) extends Element{
    override def parse(lexer: Lexer, res: ArrayBuffer[ASTree]): Unit = {
      res addOne(parser parse lexer)
    }

    override def matches(lexer: Lexer): Boolean = {
      parser matches lexer
    }
  }

  class OrTree(var parsers: Array[Parser]) extends Element{

    override def parse(lexer: Lexer, res: ArrayBuffer[ASTree]): Unit = {
      val p: Option[Parser] = choose(lexer)
      if(p isEmpty)
        throw new ParseException(lexer peek 0)
      else
        res addOne(p.get parse lexer)
    }

    override def matches(lexer: Lexer): Boolean = {
      choose(lexer) nonEmpty
    }

    def choose(lexer: Lexer): Option[Parser] = {
      parsers find { p => p.matches(lexer) }
    }

    def insert(p: Parser): Unit ={
      parsers = parsers.+:(p)
    }
  }

  class Repeat(p: Parser, onlyOnce: Boolean) extends Element{
    override def parse(lexer: Lexer, res: ArrayBuffer[ASTree]): Unit = {
      while(p matches lexer){
        val n: ASTree = p parse lexer
        if(n.getClass != classOf[ASTList] || n.numChildren > 0)
          res addOne n
        if(onlyOnce)
          return
      }
    }

    override def matches(lexer: Lexer): Boolean = p matches lexer
  }

  abstract class AToken(c: Class[_ <: ASTLeaf]) extends Element {
    private[this] val factory: Factory = {
      var klass: Class[_ <: ASTLeaf] = c
      if(klass == null)
        klass = classOf[ASTLeaf]

      Factory get(klass, classOf[Token])
    }

    override def parse(lexer: Lexer, res: ArrayBuffer[ASTree]): Unit = {
      val t: Token = lexer.read()
      if(test(t))
        res addOne factory.make(t)
      else
        throw ParseException.e(t)
    }

    override def matches(lexer: Lexer): Boolean = test(lexer peek 0)

    def test(t: Token): Boolean
  }

  class IdToken(c: Class[_ <: ASTLeaf]) extends AToken(c){
    var reserved: mutable.Set[String] = mutable.Set()
    def this(r: mutable.Set[String], c: Class[_ <: ASTLeaf]) = {
      this(c)
      if(r != null)
        reserved = r
    }

    override def test(t: Token): Boolean = (t isIdentifier) && !(reserved contains t.getText)
  }

  class NumToken(c: Class[_ <: ASTLeaf]) extends AToken(c){
    override def test(t: Token): Boolean = t isNumber
  }

  class StrToken(c: Class[_ <: ASTLeaf]) extends AToken(c){
    override def test(t: Token): Boolean = t isString
  }

  class Leaf(tokens: Array[String]) extends Element{
    override def parse(lexer: Lexer, res: ArrayBuffer[ASTree]): Unit = {
      val t: Token = lexer.read()
      if(t isIdentifier){
        var find: Boolean = false
        tokens find(s => {
          find = s equals t.getText
          if(find) this.find(res, t)
          find
        })
        if(find) return
      }
      if(tokens nonEmpty){
        throw ParseException.e(tokens(0) + " expected.", t)
      } else {
        throw ParseException.e(t)
      }
    }

    def find(res: ArrayBuffer[ASTree], t: Token): Unit ={
      res addOne new ASTLeaf(t)
    }

    override def matches(lexer: Lexer): Boolean = {
      val t: Token = lexer peek 0
      (t isIdentifier) && (tokens exists (s => s equals t.getText))
    }
  }

  class Skip(tokens: Array[String]) extends Leaf(tokens){
    override def find(res: ArrayBuffer[ASTree], t: Token): Unit = {}
  }

  //leftAssoc: left associative
  class Precedence(val value: Int, val leftAssoc: Boolean)

  class Operators extends mutable.HashMap[String, Precedence]{
    val operators: Set[String] = Set()
    def add(name: String, prec: Int, leftAssoc: Boolean): Unit ={
      put(name, new Precedence(prec, leftAssoc))
    }
  }
  object Operators {
    val LEFT: Boolean = true
    val RIGHT: Boolean = false
  }

  class Expr(factor: Parser, ops: Operators) extends Element {
    var factory: Factory = _
    def this(c: Class[_ <: ASTree], exp: Parser, op: Operators) = {
      this(exp, op)
      factory = Factory getForASTList c
    }

    override def parse(lexer: Lexer, res: ArrayBuffer[ASTree]): Unit = {
      var right = factor parse lexer
      var prec: Precedence = nextOperator(lexer)
      while(prec != null){
        right = doShift(lexer, right, prec.value)
        prec = nextOperator(lexer)
      }
      res addOne right
    }

    def doShift(lexer: Lexer, left: ASTree, prec: Int): ASTree = {
      val list: ArrayBuffer[ASTree] = ArrayBuffer(left, new ASTLeaf(lexer.read()))
      var right: ASTree = factor parse lexer
      var next: Precedence = nextOperator(lexer)
      while(next != null && rightIsExpr(prec, next)){
        right = doShift(lexer, right, next.value)
        next = nextOperator(lexer)
      }
      list addOne right
      factory make list
    }

    private def nextOperator(lexer: Lexer): Precedence = {
      val t: Token = lexer peek 0
      //t isIdentifier -> value
      ops getOrElse(t.getText, null)
    }

    private def rightIsExpr(prec: Int, nextPrec: Precedence): Boolean = {
      if(nextPrec.leftAssoc)
        prec < nextPrec.value
      else
        prec <= nextPrec.value
    }

    override def matches(lexer: Lexer): Boolean = factor matches lexer
  }

  val factoryName: String = "create"
  abstract class Factory{
    def make0(arg: Object): ASTree
    def make(arg: Object): ASTree = {
      Try(make0(arg)) getOrElse(throw new RuntimeException)
    }
  }
  object Factory{
    def getForASTList(c: Class[_ <: ASTree]): Factory = {
      var f: Factory = get(c, classOf[ArrayBuffer[_]])
      if (f == null) {
        f = (arg: Object) => {
          val res: ArrayBuffer[ASTree] = arg.asInstanceOf[ArrayBuffer[ASTree]]
          if (res.size == 1)
            res (0)
          else
            new ASTList (res)
        }
      }
      f
    }

    def get(c: Class[_ <: ASTree], argType: Class[_]): Factory = {
      if(c == null)
        return null
      var factory: Factory = null
      Try({
        val m: Method = c getMethod(factoryName, argType)
        factory = (arg: Object) => m.invoke(null, arg).asInstanceOf[ASTree]
      }).recover({
        case _: NoSuchMethodException => {
          val cons = c getConstructor argType
          factory = (arg: Object) => cons newInstance arg
        }
        case e: Exception => throw e
      })
      factory
    }
  }
}

object Parser{


  def rule(): Parser = rule(null)
  def rule(c: Class[_ <: ASTree]): Parser = new Parser(c)

}
