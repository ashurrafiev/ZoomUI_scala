package com.xrbpowered.scala.utils

object IteratorUtils {
	implicit class IteratorWithUtils[A](i: Iterator[A]) {
		def optNext: Option[A] =
			if(i.hasNext) Some(i.next) else None
			
		def mappedFind[B](predicate: A => Option[B]): Option[B] =
			if(i.hasNext)
				predicate(i.next).orElse( mappedFind(predicate) )
			else None
			
		def foldLeftUntil[B](x: B, end: A)(op: (B, A) => B): B =
			optNext match {
				case Some(n) if (n!=end) => foldLeftUntil(op(x, n), end)(op)
				case _ => x
			}
	}
}