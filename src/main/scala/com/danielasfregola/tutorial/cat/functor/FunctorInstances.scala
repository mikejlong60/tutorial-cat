package com.danielasfregola.tutorial.cat.functor

import com.danielasfregola.tutorial.cat.{Predicate, PredicateDisjunction, AbstractFilter, Filter}
import com.danielasfregola.tutorial.cat.{Just, Empty, Maybe, Zero, ZeroOrMore, OneOrMore}

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

  val filterFunctor: Functor[AbstractFilter] = new Functor[AbstractFilter] {
    override def map[A, B](boxA: AbstractFilter[A])(f: (A) => B): AbstractFilter[B] = boxA match {
      case Predicate(p) => Predicate(f(p))
      case PredicateDisjunction(predicates) => {
        val result = predicates.map(predicate => map(predicate)(f))
        PredicateDisjunction(result)
      }
      case Filter(conjunctions) => {
        val result = conjunctions.map(subjectFilter => subjectFilter._1 -> map(subjectFilter._2)(f))
        Filter(result)
      }
    }
  }
}



//case class Predicate[A](predicate: String, argument: String) extends AbstractFilter[A]

//case class PredicateDisjunction[A](predicates: List[A]) extends AbstractFilter[A]

//case class Filter[A](predicateConjunctions: Map[String, A]) extends AbstractFilter[A]
