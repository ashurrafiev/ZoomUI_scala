package com.xrbpowered.scala.zoomui

import java.awt.Toolkit
import com.xrbpowered.scala.zoomui.swing.SwingWindowFactory

abstract class UIWindowFactory {
	import UIWindowFactory._
	private var _baseScale: Float = getSystemScale
	def baseScale: Float = _baseScale
	def baseScale_= (scale: Float): Unit =
		_baseScale = if(scale>0f) scale else getSystemScale

	def create(title: String, w: Int, h: Int, canResize: Boolean): UIWindow
	def createModal[A](title: String, w: Int, h: Int, canResize: Boolean,
			onResult: A => Unit, onCancel: () => Unit): UIModalWindow[A]
	def createUndecorated(w: Int, h: Int): UIWindow
}
object UIWindowFactory {
	var instance: UIWindowFactory = new SwingWindowFactory
	
	def getSystemScale: Float = Toolkit.getDefaultToolkit.getScreenResolution / 96f
}