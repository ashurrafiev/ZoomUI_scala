package com.xrbpowered.scala.zoomui.std

import com.xrbpowered.scala.zoomui.{UIContainer, UIElement, UIHoverElement}

abstract class UIButtonBase(parent: UIContainer) extends UIHoverElement(parent) {
	import UIElement._

	var down = false

	private var _disabled = false
	def disabled: Boolean = _disabled
	def enabled: Boolean = !_disabled
	def setEnabled(enabled: Boolean): Unit = { _disabled = !enabled }
	def enable: this.type = { _disabled = false; this }
	def disable: this.type = {
		_disabled = true
		hover = false
		down = false
		this
	}

	var onAction: this.type => Unit = { _ => () }

	def action(): Unit = onAction(this)

	override def mouseIn(): Unit = if(enabled) super.mouseIn()
	override def mouseReleased(): Unit = repaint { down = false; super.mouseReleased() }

	override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
		super.mouseDown(pos, button, mods) || (
			if(button == Left) {
				if(!disabled) repaint { down = true }
				true
			}
			else false )

	override def mouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], initiator: Option[UIElement]): Boolean =
		super.mouseUp(pos, button, mods, initiator) || (
			if(initiator.contains(this) && button==Left) repaint {
				down = false
				if(!disabled) action()
				true
			}
			else false )

}
