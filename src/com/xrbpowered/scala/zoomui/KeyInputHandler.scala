package com.xrbpowered.scala.zoomui

trait KeyInputHandler {
	import UIElement._

	var onKeyPressed: (this.type, Char, Int, Set[Modifier]) => Boolean = { (_, _, _, _) => false }
	var onFocusGained: this.type => Unit = { _ => () }
	var onFocusLost: this.type => Unit = { _ => () }

	def keyPressed(ch: Char, code: Int, mods: Set[Modifier]): Boolean = onKeyPressed(this, ch, code, mods)
	def focusGained(): Unit = onFocusGained(this)
	def focusLost(): Unit = onFocusLost(this)
}
