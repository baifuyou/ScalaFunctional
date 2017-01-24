package com.scratbai.scalafp

/**
  * Created by baifuyou on 16/9/3.
  */
object App {
  def main(args: Array[String]) = {
    val rng = new SimpleRNG(System.currentTimeMillis())
    println(RNG.randIntDouble2(rng))
  }
}
