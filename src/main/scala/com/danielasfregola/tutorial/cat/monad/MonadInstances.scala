package com.danielasfregola.tutorial.cat.monad

import com.danielasfregola.tutorial.cat._
import com.danielasfregola.tutorial.cat.applicative.ApplicativeInstances

object MonadInstances {

  implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {

    override def flatMap[A, B](boxA: Maybe[A])(f: (A) => Maybe[B]): Maybe[B] = boxA match {
      case Just(a) => f(a)
      case Empty => Empty
    }

    override def pure[A](a: A): Maybe[A] = ApplicativeInstances.maybeApplicative.pure(a)
  }

  implicit val zeroOrMoreMonad: Monad[ZeroOrMore] = new Monad[ZeroOrMore] {

    override def flatMap[A, B](boxA: ZeroOrMore[A])(f: (A) => ZeroOrMore[B]): ZeroOrMore[B] = boxA match {
      case OneOrMore(h, tail) => f(h).append(flatMap(tail)(f))
      case Zero => Zero
    }

    override def pure[A](a: A): ZeroOrMore[A] = ApplicativeInstances.zeroOrMoreApplicative.pure(a)
  }

}