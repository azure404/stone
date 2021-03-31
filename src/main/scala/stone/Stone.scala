package stone

import stone.lex.Lexer
import stone.lex.reader.FileReader
import stone.lex.token.Token
import stone.parse.ast.{ASTree, NullStmt}
import stone.interpreter.Evaluator._
import stone.interpreter.{ArrayParser, BasicParser, ClassParser, ClosureParser, FunctionParser, Natives}
import stone.scope.{Environment, NestedEnvironment, ResizableArrayEnvironment}
import stone.scope.ext.EnvironmentExt._


object Stone extends App {
//  run(new BasicParser(), new BasicEnvironment())
//  run(new FunctionParser(), new NestedEnvironment())
//  run(new ClosureParser(), new NestedEnvironment())
//  run(new ClosureParser(), new Natives().environment(new NestedEnvironment()))
//  run(new ClassParser(), new Natives().environment(new NestedEnvironment()))
//    run(new ClosureParser(), new Natives().environment(new ResizableArrayEnvironment()))
  run(new ClassParser(), new Natives().environment(new ResizableArrayEnvironment()))
  def run(p: BasicParser, env: Environment): Unit = {
    val lexer: Lexer = new Lexer(new FileReader("src/main/resource/Code.txt"))
    while(lexer.peek(0) != Token.EOF) {
      val ast: ASTree = p parse lexer
      ast match {
        case n: NullStmt =>
        case t: Any =>
          t lookup env.symbols
          println("=> " + ast)
          println(ast eval env)
      }

    }
  }


}
