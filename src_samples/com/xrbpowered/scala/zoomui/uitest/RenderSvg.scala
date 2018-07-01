package com.xrbpowered.scala.zoomui.uitest

import java.awt.Color

import com.xrbpowered.scala.zoomui.GraphAssist
import com.xrbpowered.scala.zoomui.UIContainer
import com.xrbpowered.scala.zoomui.UIElement
import com.xrbpowered.scala.zoomui.UIZoomView
import com.xrbpowered.scala.zoomui.swing.SwingFrame
import com.xrbpowered.scala.zoomui.icons.SvgFile
import java.awt.RenderingHints
import java.awt.Rectangle
import com.xrbpowered.scala.zoomui.UIWindow

class RenderSvg(parent: UIContainer) extends UIZoomView(parent) {

	val svg = new SvgFile("drawing.svg")
	
	val view: UIElement = new UIElement(this) {
		override def visible(clip: Rectangle): Boolean = visible
		
		override def paint(g: GraphAssist): Unit = {
			g.graph.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
			svg.render(g.graph, 1)
		}
	}
	
	scaleRange = (1, 100)
	scale = 10
	
	override def paintSelf(g: GraphAssist): Unit =
		g.fill(this, Color.WHITE)

}
object RenderSvg {
	def main(args: Array[String]): Unit = {
		UIWindow.baseScale = 1
		val frame = new SwingFrame("ZoomViewTest", 800, 600)
		new RenderSvg(frame.container)
		frame.show
	}
}