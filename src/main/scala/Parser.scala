import scala.util.parsing.combinator._
import scala.util.parsing.input._

object Parser extends Parsers {

  override type Elem = Token

  def expression: Parser[Term] = application | expression0

  def expression0: Parser[Term] = abstraction | identifier | LPAREN ~> expression <~ RPAREN

  def abstraction: Parser[Term] = LAMBDA ~> arguments ~ DOT ~ expression ^^ {
    case args ~ DOT ~ exp => args.foldRight(exp) { Abs }
  }

  def application: Parser[Term] = expression0 ~ rep1(expression0) ^^ {
    case exp ~ exps => exps.foldLeft(exp) { (app, e) => App(app, e) }
  }

  def arguments: Parser[List[Var]] = rep1(identifier)

  def identifier: Parser[Var] = accept("variable", { case ID(id) => Var(id) })

  def apply(input: List[Token]): Option[Term] = {
    expression(TokenReader(input)) match {
      case NoSuccess(_, _) => None
      case Success(term, _) => Some(term)
    }
  }

}

case class TokenReader(input: List[Token]) extends Reader[Token] {

  override def first: Token = input.head

  override def atEnd: Boolean = input.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[Token] = TokenReader(input.tail)

}

case class ParseError(msg: String)