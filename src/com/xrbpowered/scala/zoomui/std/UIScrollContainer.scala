package com.xrbpowered.scala.zoomui.std

import java.awt.Color

import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer, UIPanView, UIElement}

abstract class UIScrollContainer(parent: UIContainer) extends UIContainer(parent) {
	import UIElement._
	import UIScrollContainer._

	val view = new UIPanView(this)

	override def layout(): Unit = {
		view.location = (0, 0)
		view.size = (width, height)
		val pan = (layoutView() - height).toInt max 0
		view.panRange = (0, pan)
		super.layout()
	}

	def layoutView(): Float

	override def paintChildren(g: GraphAssist): Unit = {
		super.paintChildren(g)

		if(view.panRange._2>0) {
			g.fillRect(width - 4f, 0, 4, height, new Color(0xdddddd))

			val s = height / (view.panRange._2.toFloat + height)
			val top = view.getPan._2 * s
			val h = height * s
			g.fillRect(width - 5f, top, 5, h, new Color(0x777777))
		}
	}

	override def mouseScroll(pos: (Float, Float), delta: Float, mods: Set[Modifier]): Boolean =
		super.mouseScroll(pos, delta, mods) ||	(
			if (mods.none) repaint {
				view.pan(0, -delta * scrollStep)
				true
			}
			else false )

}
object UIScrollContainer {
	val scrollStep: Float = 3f * GraphAssist.ptToPixels(9f)
}