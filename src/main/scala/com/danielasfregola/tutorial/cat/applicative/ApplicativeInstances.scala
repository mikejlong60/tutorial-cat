package com.danielasfregola.tutorial.cat.applicative

import com.danielasfregola.tutorial.cat._

object ApplicativeInstances {

  implicit val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {

    override def pure[A](a: A): Maybe[A] = Just(a)

    override def ap[A, B](boxF: Maybe[A => B])(boxA: Maybe[A]): Maybe[B] = (boxF, boxA) match {
      case (Just(f), Just(a)) => pure(f(a))
      case _ => Empty
    }
  }

  implicit val zeroOrMoreApplicative: Applicative[ZeroOrMore] = new Applicative[ZeroOrMore] {

    override def pure[A](a: A): ZeroOrMore[A] = OneOrMore(a, Zero)

    override def ap[A, B](boxF: ZeroOrMore[(A) => B])(boxA: ZeroOrMore[A]): ZeroOrMore[B] = (boxF, boxA) match {
      case (OneOrMore(hF, _), OneOrMore(h, tail)) => OneOrMore(hF(h), ap(boxF)(tail))
      case _ => Zero
    }
  }

}