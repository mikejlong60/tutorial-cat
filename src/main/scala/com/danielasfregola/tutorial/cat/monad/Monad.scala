package com.danielasfregola.tutorial.cat.monad

import com.danielasfregola.tutorial.cat.applicative.Applicative

trait Monad[Box[_]] extends Applicative[Box] {

  def flatMap[A, B](boxA: Box[A])(f: A => Box[B]): Box[B]

  def flatten[A](boxBoxA: Box[Box[A]]): Box[A] =
    flatMap(boxBoxA)(identity)

  override def ap[A, B](boxF: Box[A => B])(boxA: Box[A]): Box[B] =
    flatMap(boxF)(f => map(boxA)(f))

  override def map[A, B](boxA: Box[A])(f: A => B): Box[B] =
    flatMap(boxA)(a => pure(f(a)))

}