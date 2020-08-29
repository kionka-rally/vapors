package com.rallyhealth.collections

package object ops {

  implicit class IterableOps[+A](private val it: Iterable[A]) extends AnyVal {

    def foldWith[B](zero: B): FoldWith[A, B] = new FoldWith(it, zero)
  }

  final class FoldWith[+A, B] private[ops] (
    it: Iterable[A],
    zero: B
  ) {

    @inline def breakAfterState(p: B => Boolean)(f: (B, A) => B): B = breakWhen((b, _) => p(b))(f)

    @inline def breakBeforeNext(p: A => Boolean)(f: (B, A) => B): B = breakWhen((_, a) => p(a))(f)

    def breakWhen(p: (B, A) => Boolean)(f: (B, A) => B): B = {
      val ii = it.iterator
      var b = zero
      while (ii.hasNext) {
        val x = ii.next
        if (p(b, x)) return b
        b = f(b, x)
      }
      b
    }

  }

}
