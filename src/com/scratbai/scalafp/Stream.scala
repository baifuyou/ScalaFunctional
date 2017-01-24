package com.scratbai.scalafp

/**
  * Created by baifuyou on 16/9/3.
  */
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  //59页 练习5.1
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  //59页 练习5.2
  def take(n: Int): Stream[A] = {
    if (n < 1)
      Empty
    else
      this match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => t().take(n - 1))
      }
  }

  //59页 练习5.2
  def drop(n: Int): Stream[A] = {
    if (n < 1)
      this
    else
      this match {
        case Empty => Empty
        case Cons(h, t) => t().drop(n - 1)
      }
  }

  //59页 练习5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (p(h()))
        Cons(h, () => t().takeWhile(p))
      else
        Empty
  }

  //60页 练习5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  //60页 练习5.5
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, b) => {
    if (p(a)) {
      Cons(() => a, () => b)
    } else {
      Empty
    }
  })

  def headOption2: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  //61页 练习5.7
  def map[B](f: A => B) = foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))

  //61页 练习5.7
  def filter[B](f: A => Boolean) = foldRight[Stream[A]](Empty)((a, b) => {
    if (f(a)) {
      Cons(() => a, () => b)
    } else {
      b
    }
  })

  //64页 练习5.13
  def map2[B](f: A => B) = Stream.unfold(this)(_ match {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  })

  //64页 练习5.13
  def take2(n: Int) = Stream.unfold((this, n))(_ match {
    case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i - 1)))
    case _ => None
  })

  //64页 练习5.13
  def takeWhile3(p: A => Boolean) = Stream.unfold(this)(_ match {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  })

  //64页 练习5.13
  def zipWith[B, U >: A, C](o: Stream[C])(f: (U, C) => B) = Stream.unfold((this, o))(_ match {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  })

  //64页 练习5.13
  def zipAll[B](o: Stream[B]) = Stream.unfold((this, o))(_ match {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), _) => Some(((Some(h1()), None), (t1(), Empty)))
    case (_, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case _ => None
  })

  //64页 练习5.13
  def hasSubsequence[U >: A](subseq: Stream[U]) = Stream.unfold(this)(_ match {
    case s@Cons(h, t) => Some((s.startsWith(subseq), t()))
    case _ => None
  }).exists(b => b)

  //64页 练习5.15
  def hasSubsequence2[U >: A](subseq: Stream[U]) = this.tails.exists(s => s.startsWith(subseq))

  //64页 练习5.15
  def tails: Stream[Stream[A]] = Stream.unfold(this)(_ match {
    case s@Cons(h, t) => Some((s, t()))
    case Empty => Some((Empty, null))
    case _ => None
  })

  //64页 练习5.14
  def startsWith[U >: A](subseq: Stream[U]) = zipWith(subseq)((a, c) => a == c).forAll((b) => b)

  //65页 练习5.16
  def scanRight[R](i: => R)(f: (A, => R) => R) = tails.map(_.foldRight(i)(f))

  //65页 练习5.16
  def scanRight2[R](i: => R)(f: (A, => R) => R) = foldRight(Stream(i))((a, b) => {
    lazy val bVal = b
    bVal match {
      case s@Cons(h, t) => Stream.cons[R](f(a, h()), s)
      case _ => Empty
    }
  })

  def append[U >: A](a: => Stream[U]): Stream[U] = foldRight(a)((e, b) => Cons(() => e, () => b))

  def flatMap[B](f: A => Stream[B]) = foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  //63页 练习5.8
  def constant[A](a: A): Stream[A] = Cons(() => a, () => constant(a))

  //63页 练习5.9
  def from(a: Int): Stream[Int] = Cons(() => a, () => from(a + 1))

  def tmp(a: Int, b: Int): Stream[Int] = Cons(() => a + b, () => tmp(b, a + b))

  //63页 练习5.10
  def fibs: Stream[Int] = Stream(0, 1).append(tmp(0, 1))

  //63页 练习5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((v, s)) => Cons(() => v, () => unfold[A, S](s)(f))
      case _ => Empty
    }
  }

  //64页 练习5.12
  def fibs2 = unfold((0, 1))((s) => Some((s._1, (s._2, s._1 + s._2))))

  //64页 练习5.12
  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  //64页 练习5.12
  def from2(a: Int) = unfold(a)(s => Some(s, s + 1))

  //64页 练习5.12
  def ones = unfold(1)(s => Some(s, s))
}
