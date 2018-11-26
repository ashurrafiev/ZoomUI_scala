package com.xrbpowered.scala.zoomui

import java.awt.{Cursor, Toolkit}

import com.xrbpowered.scala.zoomui.UIWindow.baseScale
import com.xrbpowered.scala.zoomui.std.UIMessageBox
import com.xrbpowered.scala.zoomui.std.UIMessageBox.{Cancel, Ok, iconQuestion, show}

abstract class UIWindow {
	protected def createContainer = new BaseContainer(this, UIWindow.baseScale)
	protected val _container: BaseContainer = createContainer
	def container: BaseContainer = _container

	def clientWidth: Int
	def clientHeight: Int
	def clientSize: (Int, Int) = (clientWidth, clientHeight)
	def clientSize_= (size: (Int, Int)): Unit

	def center(): Unit

	def notifyResized(): Unit = {
		container.invalidateLayout()
		repaint()
	}

	def show(): Unit
	def repaint(): Unit

	def setCursor(cursor: Cursor)

	var onClosing: this.type => Boolean = { _ => true }
	var onClose: this.type => Unit = { _ => () }

	def closing: Boolean = onClosing(this)
	def requestClosing(): Boolean =
		if(closing) { close(); true }
		else false
	def close(): Unit = onClose(this)
}
object UIWindow {
	private var _baseScale: Float = getSystemScale
	def baseScale: Float = _baseScale
	def baseScale_= (scale: Float): Unit =
		_baseScale = if(scale>0f) scale else getSystemScale

	def getSystemScale: Float = Toolkit.getDefaultToolkit.getScreenResolution / 96f

	var confirmCosing: UIWindow => Boolean = frame => {
		import UIMessageBox._
		show("Exit", "Do you want to close the application?",
			Some(iconQuestion), Array(Ok, Cancel)) { (_, res) => {
			if(res==Ok) frame.close()
		} }
		false
	}
}