package stone.interpreter

import stone.lex.Lexer
import stone.lex.token.Token
import stone.parse.Parser
import stone.parse.ast._

import scala.collection.mutable

/**
 * 基本控制流：
 * {}: 至少重复0次，[]: 至多出现1次
 * primary    : "(" expr ")" | NUMBER | IDENTIFIER | STRING
 * factor     : "-" primary | primary
 * expr       : factor { OP factor }
 * block      : "{" [ statement ] {( ";" | EOL ) [ statement ]} "}"
 * simple     : expr
 * statement  : "if" expr block ["else" block]
 *              | "while" expr block
 *              | simple
 * program    : [ statement ] (";" | EOL)
 */
class BasicParser {

  var reserved: mutable.Set[String] = mutable.Set(
    ";", "}", ")", Token.EOL
  )



  val expr0: Parser = rule()
  val operators: expr0.Operators = {
    val tmp: expr0.Operators = new expr0.Operators()
    tmp add("=", 1, expr0.Operators.RIGHT)
    tmp add("==", 2, expr0.Operators.LEFT)
    tmp add(">", 2, expr0.Operators.LEFT)
    tmp add("<", 2, expr0.Operators.LEFT)
    tmp add("+", 3, expr0.Operators.LEFT)
    tmp add("-", 3, expr0.Operators.LEFT)
    tmp add("*", 4, expr0.Operators.LEFT)
    tmp add("/", 4, expr0.Operators.LEFT)
    tmp add("%", 4, expr0.Operators.LEFT)
    tmp
  }
  val primary: Parser = rule(classOf[PrimaryExpr])
                          .or(rule().sep("(").ast(expr0).sep(")"),
                              rule().number(classOf[NumberLiteral]),
                              rule().identifier(classOf[Name], reserved),
                              rule().string(classOf[StringLiteral]))
  val factor: Parser = rule().or(
                                rule(classOf[NegativeExpr]).sep("-").ast(primary),
                                primary)
  val expr: Parser = expr0.expression(classOf[BinaryExpr], factor, operators)
  val statement0: Parser = rule()
  val block: Parser = rule(classOf[BlockStmt])
                        .sep("{")
                        .option(statement0)
                        .repeat(rule().sep(";", Token.EOL).option(statement0))
                        .sep("}")
  val simple: Parser = rule(classOf[PrimaryExpr]).ast(expr)
  val statement: Parser = statement0.or(rule(classOf[IfStmt])
                                          .sep("if")
                                          .ast(expr)
                                          .ast(block)
                                          .option(rule().sep("else").ast(block)),
                                        rule(classOf[WhileStmt])
                                            .sep("while")
                                            .ast(expr)
                                            .ast(block),
                                        simple)
  val program: Parser = rule().or(statement, rule(classOf[NullStmt]))
                              .sep(";", Token.EOL)





  def parse(lexer: Lexer): ASTree = {
    program parse lexer
  }

  def rule(): Parser = Parser.rule()
  def rule(c: Class[_ <: ASTree]): Parser = Parser.rule(c)
}
