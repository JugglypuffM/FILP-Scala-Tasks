package exercises04.calculator

sealed trait Expr[A]
case class Mul[A](left: Expr[A], right: Expr[A])                                  extends Expr[A]
case class Div[A](left: Expr[A], right: Expr[A])                                  extends Expr[A]
case class Plus[A](left: Expr[A], right: Expr[A])                                 extends Expr[A]
case class Minus[A](left: Expr[A], right: Expr[A])                                extends Expr[A]
case class Val[A](v: A)                                                           extends Expr[A]
case class If[A](iff: A => Boolean, cond: Expr[A], left: Expr[A], right: Expr[A]) extends Expr[A]
