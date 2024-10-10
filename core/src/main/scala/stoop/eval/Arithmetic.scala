package stoop.eval

import stoop.parse.Arithmetic.*

object Arithmetic extends Interpreter[Expr, Int](parser) {

    def evalAtom(atom: Atom): Int =
        atom match
            case Integer(value) => value
            case Parens(expr) => eval(expr)

    def evalTerm(term: Term): Int =
        term match
            case Mul(l, r) => evalTerm(l) * evalAtom(r)
            case Div(l, r) => evalTerm(l) / evalAtom(r)
            case _ => eval(term)
        
    override def eval(program: Expr): Int =
        program match
            case atom: Atom => evalAtom(atom)
            case term: Term => evalTerm(term)
            case Add(l, r) => eval(l) + evalTerm(r)
            case Sub(l, r) => eval(l) - evalTerm(r)
}
