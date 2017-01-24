package com.scratbai.scalafp

/**
  * Created by baifuyou on 17-1-18.
  */
case class State[S, +A](run: S => (A, S)) {

//  def unit[A](a: A): State[S, A] = s => (a, s)
//
//  def flatMap[A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = s => {
//    val (state, rng1) = f(s)
//    g(state)(rng1)
//  }
}
