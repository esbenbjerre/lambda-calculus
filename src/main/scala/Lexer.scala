import scala.language.implicitConversions

sealed trait Token
case object LPAREN extends Token
case object RPAREN extends Token
case object LAMBDA extends Token
case object DOT extends Token
case class ID(id: String) extends Token

object Lexer {

  private val lambda = """[\\|λ]""".r
  private val id = "[a-zA-Z](_[0-9]+)?".r

  def apply(input: List[Char]): List[Token] = input.foldRight(List.empty[Token])((x, acc) => x match {
    case '(' => LPAREN :: acc
    case ')' => RPAREN :: acc
    case '.' => DOT :: acc
    case `x` if lambda.matches(x) => LAMBDA :: acc
    case `x` if id.matches(x) => ID(x) :: acc
    case _ => acc
  })

  def apply(input: String): List[Token] = apply(input.toList)

  implicit def fromChar(c: Char): String = c.toString

}
