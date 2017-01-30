import Lexer._

object TestSimpleParser {
    def main(args: Array[String]) = {
        parse(tokenize, "contract { transaction f() { hello = 1; world = 2;} } ") match {
            case Success(matched,_) => println(matched)
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }
  }
}