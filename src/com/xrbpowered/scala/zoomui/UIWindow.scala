package com.xrbpowered.scala.zoomui

import java.awt.{Cursor, Toolkit}

import com.xrbpowered.scala.zoomui.std.UIMessageBox
import com.xrbpowered.scala.zoomui.std.UIMessageBox.{Cancel, Ok, iconQuestion, show}

abstract class UIWindow(val factory: UIWindowFactory) {
	protected def createContainer = new BaseContainer(this, factory.baseScale)
	protected val _container: BaseContainer = createContainer
	def container: BaseContainer = _container

	protected var _exitOnClose = false
	def exitOnClose(exit: Boolean): UIWindow = { _exitOnClose = exit; this }
	
	def clientWidth: Int
	def clientHeight: Int
	def clientSize: (Int, Int) = (clientWidth, clientHeight)
	def clientSize_= (size: (Int, Int)): Unit

	def x: Int
	def y: Int
	def moveTo(x: Int, y: Int): Unit
	def center(): Unit
	def move(dx: Int, dy: Int): Unit =
		moveTo(x+dx, y+dy)

	def notifyResized(): Unit = {
		container.invalidateLayout()
		repaint()
	}

	def show(): Unit
	def repaint(): Unit

	def baseToScreen(pos: (Float, Float)): (Int, Int)
	def screenToBase(pos: (Int, Int)): (Float, Float)
	
	def setCursor(cursor: Cursor)

	var onClosing: this.type => Boolean = { _ => true }
	var onClose: this.type => Unit = { _ => if(_exitOnClose) System.exit(0) }

	def closing: Boolean = onClosing(this)
	def requestClosing(): Boolean =
		if(closing) { close(); true }
		else false
	def close(): Unit = onClose(this)
}
object UIWindow {
	var confirmCosing: UIWindow => Boolean = frame => {
		import UIMessageBox._
		show(frame.factory, "Exit", "Do you want to close the application?",
			Some(iconQuestion), Array(Ok, Cancel)) { res => {
				if(res==Ok) frame.close()
			} }
		false
	}
}