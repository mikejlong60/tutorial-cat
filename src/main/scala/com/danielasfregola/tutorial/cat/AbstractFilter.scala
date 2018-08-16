package com.danielasfregola.tutorial.cat

trait AbstractFilter[+A]

case class Predicate[A](predicate: A) extends AbstractFilter[A]

case class PredicateDisjunction[A](predicates: List[AbstractFilter[A]]) extends AbstractFilter[A]

case class Filter[A](predicateConjunctions: Map[String, AbstractFilter[A]]) extends AbstractFilter[A]

