import cats.Monad
import cats.effect._

object REPL extends scala.App {

  val repl: IO[Unit] = for {
    _ <- IO(print("λ> "))
    line <- IO(scala.io.StdIn.readLine())
  } yield {
    Parser(Lexer(line)) match {
      case None => println("Parse error")
      case Some(term) => println(term.cbv)
    }
  }

  val program = Monad[IO].foreverM {
    repl.as(ExitCode.Success)
  }
  program.unsafeRunSync()

}