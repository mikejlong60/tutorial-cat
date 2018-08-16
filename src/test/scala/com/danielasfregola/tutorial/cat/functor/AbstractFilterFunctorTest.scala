package com.danielasfregola.tutorial.cat.functor

import com.danielasfregola.tutorial.cat.AbstractFilter
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import com.danielasfregola.tutorial.cat.Filter
import FunctorInstances.filterFunctor

class AbstractFilterFunctorTest extends Properties("AbstractFilter Functor Laws test") {

  val f: Tuple2[String, String] => Boolean = pred => pred._1 == pred._2
  val g: Boolean => String = pred => pred.toString
  val h: String => Int = _.length
  val fG = f andThen g
  val mapFG: AbstractFilter[(String, String)] => AbstractFilter[String] = filterFunctor.map(_)(fG)
  val mapF: AbstractFilter[(String, String)] => AbstractFilter[Boolean] = filterFunctor.map(_)(f)
  val mapG: AbstractFilter[Boolean] => AbstractFilter[String] = filterFunctor.map(_)(g)

  property("identity") = forAll(AbstractFilterGenerator.genFilter) { predicateConjunctions =>
    val expected = Filter(predicateConjunctions)
    filterFunctor.map(expected)(identity) == expected
  }

  property("composition") = forAll(AbstractFilterGenerator.genFilter) { predicateConjunctions =>
    val expected = Filter(predicateConjunctions)
    mapFG(expected) == (mapF andThen mapG)(expected)
  }

  property("associativity") = forAll(AbstractFilterGenerator.genFilter) { predicateConjunctions =>
    val expected = Filter(predicateConjunctions)
    val gH = g andThen h
    val mapGH: AbstractFilter[Boolean] => AbstractFilter[Int] = filterFunctor.map(_)(gH)
    val mapH: AbstractFilter[String] => AbstractFilter[Int] = filterFunctor.map(_)(h)
    (mapF andThen mapGH)(expected) == (mapFG andThen mapH)(expected)
  }

}

