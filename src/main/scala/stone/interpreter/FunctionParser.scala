package stone.interpreter

import stone.parse.Parser
import stone.parse.ast.{Arguments, DefStmt, ParameterList}

/**
 * 函数语法：
 * param      : IDENTIFIER
 * params     : param { "," param }
 * param_list : "(" [ params ] ")"
 * func        : "def" IDENTIFIER param_list block
 * args       : expr { "," expr }
 * postfix    : "(" [ args ] ")"
 * primary    : ( "(" expr ")" | NUMBER | IDENTIFIER | STRING ) { postfix }
 * simple     : expr [ args ]
 * program    : [ def | statement ] (";" | EOL)
 *
 */
class FunctionParser extends BasicParser {



  val param: Parser = rule().identifier(reserved)
  val params: Parser = rule(classOf[ParameterList]).ast(param).repeat(rule().sep(",").ast(param))
  val paramList: Parser = rule().sep("(").maybe(params).sep(")")
  val func: Parser = rule(classOf[DefStmt]).sep("def").identifier(reserved).ast(paramList).ast(block)
  val args: Parser = rule(classOf[Arguments]).ast(expr).repeat(rule().sep(",").ast(expr))
  val postfix: Parser = rule().sep("(").maybe(args).sep(")")


  primary.repeat(postfix)
  simple.option(args)
  program.insertChoice(func)

}
