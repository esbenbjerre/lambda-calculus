case class Var(x: String) extends Term

case class Abs(x: Var, e: Term) extends Term

case class App(e1: Term, e2: Term) extends Term

sealed trait Term { term =>

  def freeVars: Set[Var] = term match {
    case Var(id) => Set(Var(id))
    case Abs(arg, body) => body.freeVars - arg
    case App(fun, arg) => fun.freeVars ++ arg.freeVars
  }

  def alpha: Term = alpha0(Map.empty)

  private def alpha0(vars: Map[String, String]): Term = term match {
    case Var(id) => vars.get(id) match {
      case None => Var(id)
      case Some(y) => Var(y)
    }
    case Abs(arg, body) =>
      val y = arg.x + "'" * (vars.size + 1)
      Abs(Var(y), body.alpha0(vars.updated(arg.x, y)))
    case App(fun, arg) => App(fun.alpha0(vars), arg.alpha0(vars))
  }

  def substitute(x: Var, s: Term): Term = term.alpha.substitute0(x, s)

  private def substitute0(x: Var, s: Term): Term = term match {
    case Var(x1) if Var(x1) == x => s
    case Abs(y, e) if y != x && !s.freeVars.contains(y) => Abs(y, e.substitute0(x, s))
    case App(e1, e2) => App(e1.substitute0(x, s), e2.substitute0(x, s))
    case _ => term
  }

  def cbn: Term = term match {
    case Var(x) => Var(x)
    case Abs(x, e) => Abs(x, e)
    case App(e1, e2) => e1.cbn match {
      case Abs(x, e) => e.substitute(x, e2).cbn
      case e10 => App(e10, e2)
    }
  }

  def cbv: Term = term match {
    case Var(x) => Var(x)
    case Abs(x, e) => Abs(x, e)
    case App(e1, e2) => e1.cbv match {
      case Abs(x, e) => e.substitute(x, e2.cbv).cbv
      case e10 => App(e10, e2.cbv).cbv
    }
  }

  override def toString: String = term match {
    case Var(id) => id
    case Abs(arg, body) => s"(λ$arg.$body)"
    case App(fun, arg) => s"($fun $arg)"
  }

}