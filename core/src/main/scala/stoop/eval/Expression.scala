package stoop.eval

import stoop.parse.Expression.*

object Expression extends Interpreter[Expr, Boolean | Int](parser){

  def evalAtom(atom: Atom): Boolean | Int =
    atom match
      case Bool(value) => value
      case Integer(value) => value
      case Parens(expr) => eval(expr)

  private def evalPre1(term: Pre1): Boolean | Int =
    term match
      case Mul(l, r) =>
        (evalPre1(l), evalAtom(r)) match
          case (l: Int, r: Int) => l * r
      case Div(l, r) =>
        (evalPre1(l), evalAtom(r)) match
          case (l: Int, r: Int) => l / r
      case _ => eval(term)

  private def evalPre2(term: Pre2): Boolean | Int =
    term match
      case Add(l, r) =>
        (evalPre2(l), evalPre1(r)) match
          case (l: Int, r: Int) => l + r
      case Sub(l, r) =>
        (evalPre2(l), evalPre1(r)) match
          case (l: Int, r: Int) => l - r
      case _ => eval(term)

  private def evalPre3(term: Pre3): Boolean | Int =
    term match
      case GtEq(l, r) =>
        (evalPre3(l), evalPre2(r)) match
          case (l: Int, r: Int) => l >= r
      case LtEq(l, r) =>
        (evalPre3(l), evalPre2(r)) match
          case (l: Int, r: Int) => l <= r
      case Eq(l, r) =>
        (evalPre3(l), evalPre2(r)) match
          case (l: Int, r: Int) => l == r
          case (l: Boolean, r: Boolean) => l == r
          case _ => false
      case _ => eval(term)

  private def evalPre4(term: Pre4): Boolean | Int =
    term match
      case Gt(l, r) =>
        (evalPre4(l), evalPre3(r)) match
          case (l: Int, r: Int) => l > r
      case Lt(l, r) =>
        (evalPre4(l), evalPre3(r)) match
          case (l: Int, r: Int) => l < r
      case _ => eval(term)

  override def eval(program: Expr): Boolean | Int =
    program match
      case And(l, r) =>
        (eval(l), evalPre4(r)) match
          case (l: Boolean, r: Boolean) => l && r
      case Or(l, r) =>
        (eval(l), evalPre4(r)) match
          case (l: Boolean, r: Boolean) => l || r
      case pred: Atom => evalAtom(pred)
      case pred: Pre1 => evalPre1(pred)
      case pred: Pre2 => evalPre2(pred)
      case pred: Pre3 => evalPre3(pred)
      case pred: Pre4 => evalPre4(pred)
}

