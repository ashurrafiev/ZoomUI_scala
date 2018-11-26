package com.xrbpowered.scala.zoomui.swing

import com.xrbpowered.scala.zoomui.UIWindowFactory
import com.xrbpowered.scala.zoomui.UIWindow
import com.xrbpowered.scala.zoomui.UIModalWindow

class SwingWindowFactory extends UIWindowFactory {
	override def create(title: String, w: Int, h: Int, canResize: Boolean): UIWindow =
		new SwingFrame(this, title, w, h, canResize, false)
	override def createModal[A](title: String, w: Int, h: Int, canResize: Boolean,
			onResult: Option[A => Unit], onCancel: Option[() => Unit]): UIModalWindow[A] = {
		val dlg = new SwingModalDialog[A](this, title, w, h, canResize)
		onResult.foreach { dlg.onResult = _ }
		onCancel.foreach { dlg.onCancel = _ }
		dlg
	}
	override def createUndecorated(w: Int, h: Int): UIWindow =
		new SwingFrame(this, null, w, h, false, true)
}