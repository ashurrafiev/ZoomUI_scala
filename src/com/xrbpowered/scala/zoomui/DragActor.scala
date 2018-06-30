package com.xrbpowered.scala.zoomui

trait DragActor {
	import UIElement._

	def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean
	def notifyMouseMove(dx: Float, dy: Float): Boolean
	def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], target: Option[UIElement]): Boolean
}
