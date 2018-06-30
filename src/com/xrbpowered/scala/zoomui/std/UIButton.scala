package com.xrbpowered.scala.zoomui.std

import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer}
import java.awt.{Color, Font, GradientPaint}

class UIButton(parent: UIContainer, var label: String) extends UIButtonBase(parent) {
	import  UIButton._

	size = (defaultWidth, defaultHeight)

	override def paint(g: GraphAssist): Unit = {
		import GraphAssist._
		g.setPaint(
			if (down) colorDown
			else new GradientPaint(0, 0, colorGradTop, 0, height, colorGradBottom) )

		g.fill(this)
		g.border(this, if (hover) colorText else colorBorder)

		g.setColor(colorText)
		g.setFont(font)
		g.drawString(label, width/2, height/2, Center)
	}
}
object UIButton {
	var font = new Font("Tahoma", Font.PLAIN, GraphAssist.ptToPixels(9f))

	var colorDown = new Color(0xd4d4d4)
	var colorBorder = new Color(0x888888)
	var colorText: Color = Color.BLACK
	var colorGradTop = new Color(0xeeeeee)
	var colorGradBottom = new Color(0xcccccc)

	var defaultWidth = 88
	var defaultHeight = 20
}
