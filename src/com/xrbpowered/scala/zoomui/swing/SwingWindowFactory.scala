package com.xrbpowered.scala.zoomui.swing

import com.xrbpowered.scala.zoomui.UIWindowFactory
import com.xrbpowered.scala.zoomui.UIWindow
import com.xrbpowered.scala.zoomui.UIModalWindow

class SwingWindowFactory extends UIWindowFactory {
	override def create(title: String, w: Int, h: Int, canResize: Boolean): UIWindow =
		new SwingFrame(this, title, w, h, canResize, false).exitOnClose(false)
	override def createModal[A](title: String, w: Int, h: Int, canResize: Boolean,
			onResult: A => Unit, onCancel: () => Unit): UIModalWindow[A] = {
		val dlg = new SwingModalDialog[A](this, title, w, h, canResize)
		dlg.onResult = onResult
		dlg.onCancel = onCancel
		dlg
	}
	override def createUndecorated(w: Int, h: Int): UIWindow =
		new SwingFrame(this, null, w, h, false, true).exitOnClose(false)
		
	def createFrame(title: String, w: Int, h: Int, canResize: Boolean): SwingFrame =
		new SwingFrame(this, title, w, h, canResize, false)

	def createFrame(title: String, w: Int, h: Int): SwingFrame =
		new SwingFrame(this, title, w, h, true, false)

	def createFullscreen: SwingFrame =
		return new SwingFrame(this, null, 1024, 600, false, true).maximize()
}
object SwingWindowFactory {
	def use(): SwingWindowFactory =
		UIWindowFactory.instance match {
			case swing: SwingWindowFactory => swing
			case _ =>
				val factory = new SwingWindowFactory
				UIWindowFactory.instance = factory
				factory
		}

	def use(baseScale: Float): SwingWindowFactory = {
		val factory = use()
		factory.baseScale = baseScale
		factory
	}
	
}