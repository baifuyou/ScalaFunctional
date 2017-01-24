package com.scratbai.scalafp

/**
  * Created by baifuyou on 16-9-12.
  */
trait RNG {
  def nextInt: (Int, RNG);
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  //70页 练习6.1
  def nonNegativeInt(r: RNG): (Int, RNG) = {
    val (rand, rng) = r.nextInt
    if (rand < 0 && rand > Int.MinValue) {
      (-rand, rng)
    } else if (rand == Int.MinValue) {
      (0, rng)
    } else {
      (rand, rng)
    }
  }

  //70页 练习6.2
  def double(r: RNG): (Double, RNG) = {
    val (rand, rng) = nonNegativeInt(r)
    if (rand == Int.MaxValue) {
      ((rand - 1)/Int.MaxValue.toDouble, rng)
    } else {
      (rand/Int.MaxValue.toDouble, rng)
    }
  }

  //70页 练习6.3
  def intDouble(r: RNG): ((Int, Double), RNG) = {
    val (rand1, rng1) = r.nextInt
    val (rand2, rng2) = double(rng1)
    ((rand1, rand2), rng2)
  }

  //70页 练习6.3
  def doubleInt(r: RNG): ((Double, Int), RNG) = {
    val (rand1, rng1) = double(r)
    val (rand2, rng2) = rng1.nextInt
    ((rand1, rand2), rng2)
  }

  //70页 练习6.3
  def double3(r: RNG): ((Double, Double, Double), RNG) = {
    val (rand1, rng1) = double(r)
    val (rand2, rng2) = double(rng1)
    val (rand3, rng3) = double(rng2)
    ((rand1, rand2, rand3), rng3)
  }

  //70页 练习6.4
  def ints(count: Int)(r: RNG): (List[Int], RNG) = {
    if (count <= 1) {
      val (rand, rng) = r.nextInt
      (List(rand), rng)
    } else {
      val (rand, rng1) = r.nextInt
      val (rands, rng2) = ints(count - 1)(rng1)
      (rand::rands, rng2)
    }
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  //71页 练习6.5
  def doubleByMap = map(r => r.nextInt)(a => {
    if (a == Int.MaxValue) {
      a - 1
    } else {
      a/Int.MaxValue.toDouble
    }
  })

  //72页 练习6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (rand1, rng1) = ra(rng)
      val (rand2, rng2) = rb(rng1)
      (f(rand1, rand2), rng2)
    }
  }

  //72页 练习6.6
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  //72页 练习6.6
  val randIntDouble2: Rand[(Int, Double)] = both(int, double)

  //72页 练习6.6
  val randDoubleInt2: Rand[(Double, Int)] = both(double, int)

  //72页 练习6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      var rng2 = rng;
      var rs = List[A]()
      for (f <- fs) {
        val rand = f(rng2)
        rng2 = rand._2
        rs = rand._1::rs
      }
      (rs, rng2)
    }
  }

  //72页 练习6.7
  def ints2(n: Int) = sequence(List.fill(n)(int))

  //73页 练习6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (rand, rng1) = f(rng)
    g(rand)(rng1)
  }

  //73页 练习6.8
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
    rng => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng)
      else nonNegativeLessThan(n)(rng)
    }
  })

  //73页 练习6.9
  def mapByFlatMap[A, B](rand: Rand[A])(f: A => B): Rand[B] = flatMap(rand)(a => {
    rng => (f(a), rng)
  })

  //73页 练习6.9
  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => {
    rng => {
      val (rnumb, rngb) = rb(rng)
      (f(a, rnumb), rngb)
    }
  })
}
