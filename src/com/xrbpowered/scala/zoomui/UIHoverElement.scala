package com.xrbpowered.scala.zoomui

abstract class UIHoverElement(parent: UIContainer) extends UIElement(parent) {
	var hover = false

	override def mouseIn(): Unit = repaint { hover = true; super.mouseIn() }
	override def mouseOut(): Unit = repaint { hover = false; super.mouseOut() }

}
