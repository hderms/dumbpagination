package com.hderms.dumbpagination

trait Paginatable[A] {
  import scalaz._
  import Scalaz._
  def next(original: A): Option[A]
  def toList(original: A)(implicit pager: Paginatable[A]): List[A] = {
    unfold(original) { (x: A) =>
    {
      val nx = pager.next(x)
      nx match {
        case None => none
        case Some(nextPage) => (x, nextPage).some
      }
    }
    }.toList

  }
}


object Page {
  implicit object Foo extends Paginatable[Int] {
    def next(original: Int): Option[Int] = { if (original <= 10) Some(original + 1) else None}
  }
  def test = {
    Foo.toList(0)
  }
}
