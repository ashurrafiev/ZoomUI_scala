package com.xrbpowered.scala.zoomui.std

import java.awt.Color

import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer, UIPanView, UIElement}
import com.xrbpowered.scala.zoomui.std.text.UITextBox

abstract class UIScrollContainer(parent: UIContainer) extends UIContainer(parent) {
	import UIElement._
	import UIScrollContainer._

	val view = new UIPanView(this)
	val scroll = new UIScrollBar(this, true) {
		override def paintSelf(g: GraphAssist): Unit = {
			if(view.panRange._2>0) {
				setThumbSpan(height.round)
				setRange(0, view.panRange._2, scrollStep)
				value = view.getPan._2.round
			}
			else
				setRange(0, 0, 0)
			super.paintSelf(g)
		}
	}
	scroll.onChanged = s => view.setPan(0, s.value)

	override def layout(): Unit = {
		scroll.setLength(height)
		scroll.location = (width - scroll.width, 0)
		
		view.location = (0, 0)
		view.size = (width - scroll.width, height)
		view.setPanRangeForClient(0, layoutView());
		super.layout()
	}

	def layoutView(): Float
	
	override def paintChildren(g: GraphAssist): Unit = {
		super.paintChildren(g)
		paintBorder(g)
	}
	def paintBorder(g: GraphAssist): Unit = g.border(this, colorBorder)

	override def mouseScroll(pos: (Float, Float), delta: Float, mods: Set[Modifier]): Boolean =
		super.mouseScroll(pos, delta, mods) ||	(
			if (mods.none) repaint {
				view.pan(0, -delta * wheelStep)
				true
			}
			else false )

}
object UIScrollContainer {
	val scrollStep: Int = GraphAssist.ptToPixels(9f)
	val wheelStep: Int = 3*scrollStep
	
	var colorBorder: Color = UITextBox.colorBorder
}