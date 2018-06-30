package com.xrbpowered.scala.zoomui

import com.xrbpowered.scala.zoomui.BaseContainer.ModalBaseContainer

abstract class UIModalWindow[A](defaultResult: A) extends UIWindow {
	override protected def createContainer: BaseContainer = new ModalBaseContainer[A](this, UIWindow.baseScale)
	override def container: ModalBaseContainer[A] = _container.asInstanceOf[ModalBaseContainer[A]]

	var onResult: (this.type, A) => Unit = { (_, _) => () }

	override def close(): Unit = closeWithResult(defaultResult)
	def closeWithResult(result: A): Unit = { onClose(this); onResult(this, result) }
}
