package com.xrbpowered.scala.zoomui

import java.awt.RenderingHints

class BaseContainer(_window: UIWindow, private var _baseScale: Float) extends UIContainer(None) with KeyInputHandler {
	import UIElement._

	override protected def getBase: BaseContainer = this

	def window: UIWindow = _window

	private var uiUnderMouse: Option[UIElement] = None
	private var uiFocused: Option[KeyInputHandler] = None

	private var drag: Option[DragActor] = None
	private var uiInitiator: Option[UIElement] = None
	private var initiatorButtonMods: (Button, Set[Modifier]) = (Left, Set.empty)
	private var prevMousePoint: (Float, Float) = (0, 0)

	private var invalidLayout: Boolean = true
	override def invalidateLayout(): Unit = { invalidLayout = true }

	private def dragMode: Boolean = drag.isDefined

	override def focusLost(): Unit = { super.focusLost(); resetFocus() }

	override def keyPressed(ch: Char, code: Int, mods: Set[Modifier]): Boolean =
		super.keyPressed(ch, code, mods) || uiFocused.fold(false) { _.keyPressed(ch, code, mods)	}

	override def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[UIElement] = {
		if (!dragMode) {
			prevMousePoint = pos
			initiatorButtonMods = (button, mods)
			val ui = super.notifyMouseDown(pos, button, mods)
			if (ui != uiInitiator) uiInitiator.foreach { _.mouseReleased() }
			uiInitiator = ui
			if (uiFocused.isDefined && uiFocused != uiInitiator) resetFocus()
		}
		Some(this)
	}
	override def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], initiator: Option[UIElement]): Option[UIElement] = {
		if (dragMode) {
			val ui = elementAt(pos)
			if (drag.get.notifyMouseUp(pos, button, mods, ui)) drag = None
		}
		else {
			if (super.notifyMouseUp(pos, button, mods, uiInitiator) != uiInitiator)
				uiInitiator.foreach { _.mouseReleased() } // FIXME release for multi-button scenarios
		}
		Some(this)
	}

	override def mouseOut(): Unit = {
		if (!dragMode) {
			uiUnderMouse.foreach { e => if(e!=this) e.mouseOut() }
			uiUnderMouse = None
		}
		super.mouseOut()
	}

	private def updateMouseMove(pos: (Float, Float)): Unit = {
		val ui = elementAt(pos)
		if (ui != uiUnderMouse) {
			uiUnderMouse.foreach { e => if(e!=this) e.mouseOut() }
			uiUnderMouse = ui
			uiUnderMouse.foreach { e => if(e!=this) e.mouseIn() }
		}
	}
	override def mouseMoved(pos: (Float, Float), mods: Set[Modifier]): Unit = {
		if (!dragMode) {
			updateMouseMove(pos)
			uiUnderMouse.foreach { e => if(e!=this) e.mouseMoved(e.baseToLocal(pos), mods) }
		}
	}
	def mouseDragged(pos: (Float, Float)): Unit = {
		if (drag.isEmpty)
			drag = uiInitiator.fold(None: Option[DragActor]) {
				_.acceptDrag(prevMousePoint, initiatorButtonMods._1, initiatorButtonMods._2)
			}
		if (dragMode) {
			drag = if (drag.get.notifyMouseMove(pos._1 - prevMousePoint._1, pos._2 - prevMousePoint._2)) drag else None
			prevMousePoint = pos
		}
		updateMouseMove(pos)
	}

	def resetFocus(): Unit = {
		uiFocused.foreach {_.focusLost() }
		uiFocused = children.find(_.isInstanceOf[KeyInputHandler]).asInstanceOf[Option[KeyInputHandler]]
	}
	def focus: Option[KeyInputHandler] = uiFocused
	def focus_= (handler: Option[KeyInputHandler]): Unit = {
		if(uiFocused.isDefined && uiFocused != handler)
			resetFocus()
		uiFocused = handler
		uiFocused.foreach { _.focusGained() }
	}

	override def addChild(c: UIElement): Unit = { super.addChild(c); resetFocus() }

	def baseScale: Float = _baseScale
	def baseScale_= (scale: Float): Unit = {
		this._baseScale = if(scale>0f) scale else UIWindow.getSystemScale
		invalidateLayout()
	}

	override def pixelScale: Float = 1f / _baseScale

	override def parentToLocal(pos: (Float, Float)): (Float, Float) =
		(pos._1 / _baseScale, pos._2 / _baseScale)

	override def layout(): Unit = {
		for(c <- children) {
			c.location = (0, 0)
			c.size = size
			c.layout()
		}
		invalidLayout = false
	}

	override def x: Float = 0
	override def x_= (x: Float): Unit = { }
	override def y: Float = 0
	override def y_= (y: Float): Unit = { }
	override def width: Float = window.clientWidth / _baseScale
	override def width_= (width: Float): Unit = { }
	override def height: Float = window.clientHeight / _baseScale
	override def height_= (height: Float): Unit = { }

	override def isInside(pos: (Float, Float)): Boolean = true

	override def paint(g: GraphAssist): Unit = {
		if (invalidLayout) layout()
		g.graph.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HRGB)
		g.graph.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
		super.paint(g)
	}

	override def paintChildren(g: GraphAssist): Unit = {
		g.pushTx()
		g.scale(_baseScale)
		super.paintChildren(g)
		g.popTx()
	}
}
object BaseContainer {

	class ModalBaseContainer[A](_window: UIModalWindow[A], scale: Float) extends BaseContainer(_window, scale) {
		override def window: UIModalWindow[A] = _window
	}

}