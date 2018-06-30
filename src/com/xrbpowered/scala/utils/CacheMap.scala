package com.xrbpowered.scala.utils

import scala.collection.mutable

class CacheMap[A, B] extends mutable.HashMap[A, B]() {
	def getOrCache(key: A)(makeValue: => B): B =
		get(key) match {
			case Some(value) => value
			case None => val value = makeValue; put(key, value); value
		}
}
