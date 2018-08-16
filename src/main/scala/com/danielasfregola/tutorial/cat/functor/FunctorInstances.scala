package com.danielasfregola.tutorial.cat.functor

import com.danielasfregola.tutorial.cat._

object FunctorInstances {

  implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    override def map[A, B](boxA: Maybe[A])(f: (A) => B): Maybe[B] = boxA match {
      case Just(a) => Just(f(a))
      case Empty => Empty
    }
  }

  implicit val zeroOrMoreFunctor: Functor[ZeroOrMore] = new Functor[ZeroOrMore] {
    override def map[A, B](boxA: ZeroOrMore[A])(f: (A) => B): ZeroOrMore[B] = boxA match {
      case OneOrMore(a, tail) => OneOrMore(f(a), map(tail)(f))
      case Zero => Zero
    }
  }

}