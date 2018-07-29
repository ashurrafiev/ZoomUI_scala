package com.xrbpowered.scala.zoomui

import scala.collection.mutable.ArrayBuffer
import com.xrbpowered.scala.utils.IteratorUtils._

abstract class UIContainer(parent: Option[UIContainer]) extends UIElement(parent) {
	import UIElement._

	def this(parent: UIContainer) = this(Some(parent))

	val children: ArrayBuffer[UIElement] = new ArrayBuffer[UIElement]()

	def addChild(c: UIElement): Unit = { children += c; invalidateLayout() }
	def removeChild(c: UIElement): Unit = { children -= c; invalidateLayout() }
	def removeAllChildren(): Unit = { children.clear(); invalidateLayout() }

	override def layout(): Unit = children.foreach { _.layout() }

	def paintSelf(g: GraphAssist): Unit = {}

	def paintChildren(g: GraphAssist): Unit = {
		val clip = g.graph.getClipBounds
		for (c <- children if c.visible(clip)) {
			g.pushTx()
			g.translate(c.x, c.y)
			c.paint(g)
			g.popTx()
		}
	}

	override def paint(g: GraphAssist): Unit = {
		paintSelf(g)
		paintChildren(g)
	}

	def findChild(predicate: UIElement => Option[UIElement]): Option[UIElement] =
		children.reverseIterator.withFilter(_.visible).mappedFind(predicate)

	override def elementAt(pos: (Float, Float)): Option[UIElement] = findChild(
			_.elementAt(parentToLocal(pos))
		).orElse(super.elementAt(pos))

	override def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[UIElement] = findChild(
			_.notifyMouseDown(parentToLocal(pos), button, mods)
		).orElse(super.notifyMouseDown(pos, button, mods))

	override def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], initiator: Option[UIElement]): Option[UIElement] = findChild(
			_.notifyMouseUp(parentToLocal(pos), button, mods, initiator)
		).orElse(super.notifyMouseUp(pos, button, mods, initiator))

	override def notifyMouseScroll(pos: (Float, Float), delta: Float, mods: Set[Modifier]): Option[UIElement] = findChild(
			_.notifyMouseScroll(parentToLocal(pos), delta, mods)
		).orElse(super.notifyMouseScroll(pos, delta, mods))

}
