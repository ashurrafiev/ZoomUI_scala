package com.xrbpowered.scala.zoomui

import com.xrbpowered.scala.zoomui.BaseContainer.ModalBaseContainer

abstract class UIModalWindow[A](factory: UIWindowFactory) extends UIWindow(factory) {
	override protected def createContainer: BaseContainer = new ModalBaseContainer[A](this, factory.baseScale)
	override def container: ModalBaseContainer[A] = _container.asInstanceOf[ModalBaseContainer[A]]

	var onResult: A => Unit = { _ => () }
	var onCancel: () => Unit = { () => () }

	override def close(): Unit = { onClose(this); onCancel() }
	def closeWithResult(result: A): Unit = { onClose(this); onResult(result) }
	
	val resultHandler: A => Unit = { res => closeWithResult(res) }
	val cancelHandler: () => Unit = { () => close() }
}
object UIModalWindow {
	def cancelWithDefault[A](onResult: A => Unit, defaultResult: A): () => Unit =
		() => { onResult(defaultResult) }
}