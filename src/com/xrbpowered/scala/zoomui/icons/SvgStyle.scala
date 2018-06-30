package com.xrbpowered.scala.zoomui.icons

import java.awt.{BasicStroke, Color, Graphics2D, Paint}

import org.w3c.dom.Element

case class SvgStyle(fill: Option[Paint], strokeColor: Option[Color], strokeWidth: Double) {
	def hasFill: Boolean = fill.isDefined
	def setFillStyle(g2: Graphics2D): Unit = fill.foreach { f => g2.setPaint(f) }

	def hasStroke: Boolean = strokeColor.isDefined
	def setStrokeStyle(g2: Graphics2D, scale: Double): Unit = {
		g2.setStroke(new BasicStroke((strokeWidth*scale).toFloat))
		strokeColor.foreach { c => g2.setColor(c) }
	}
}
object SvgStyle {
	val Default = SvgStyle(Some(Color.BLACK), None, 1.0)

	def parseFill(v: String, defs: SvgDefs): Option[Paint] =
		if (v.startsWith("url")) {
			val id = v.substring(4, v.length - 1)
			defs.defs.get(id) match {
				case Some(p: Paint) => Some(p)
				case _ => None
			}
		}
		else parseColor(v)

	def parseColor(v: String): Option[Color] =
		if (v.startsWith("#")) {
			try {
				val c = Integer.parseInt(v.substring(1), 16)
				Some(new Color(c))
			} catch {
				case e: NumberFormatException => None
			}
		}
		else None

	def forElement(parent: SvgStyle, defs: SvgDefs, e: Element): SvgStyle = {
		val attr = e.getAttribute("style")
		if (!attr.isEmpty) {
			var fill = parent.fill
			var strokeColor = parent.strokeColor
			var strokeWidth = parent.strokeWidth
			val vals = attr.split(";")
			for (v <- vals) {
				val kv = v.split(":", 2)
				kv(0) = kv(0).trim
				kv(1) = kv(1).trim
				if (kv(0) == "fill") fill = parseFill(kv(1), defs)
				else if (kv(0) == "stroke") strokeColor = parseColor(kv(1))
				else if (kv(0) == "stroke-width") {
					if (kv(1).endsWith("px"))
						kv(1) = kv(1).substring(0, kv(1).length - 2)
					strokeWidth = kv(1).toDouble
				}
			}
			SvgStyle(fill, strokeColor, strokeWidth)
		}
		else parent
	}
}
