package com.xrbpowered.scala.zoomui

import UIElement._

class DragWindowActor(val element: UIElement, val triggerButton: Button, val triggerMods: Set[Modifier]) extends DragActor {
	def this(element: UIElement) = this(element, Left, Set.empty[Modifier])

	def isTrigger(button: Button, mods: Set[Modifier]): Boolean =
		button==triggerButton && mods==triggerMods
	
	def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean = isTrigger(button, mods)
	def notifyMouseMove(dx: Float, dy: Float): Boolean = { element.base.window.move(dx.toInt, dy.toInt); true }
	def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], target: Option[UIElement]): Boolean = true
}