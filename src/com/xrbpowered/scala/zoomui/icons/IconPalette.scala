package com.xrbpowered.scala.zoomui.icons

import java.awt.{Color, GradientPaint, Paint}

class IconPalette(val colors: Array[(Color, Color, Color, Color)]) {
	def getBgPaint(style: Int, y1: Float, y2: Float): Paint =
		new GradientPaint(0, y1, colors(style)._1, 0, y2, colors(style)._2)
	def getFgPaint(style: Int, y1: Float, y2: Float): Paint =
		new GradientPaint(0, y1, colors(style)._3, 0, y2, colors(style)._4)
}
