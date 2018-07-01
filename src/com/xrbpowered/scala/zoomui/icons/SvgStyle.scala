package com.xrbpowered.scala.zoomui.icons

import java.awt.{BasicStroke, Color, Graphics2D, Paint}

import org.w3c.dom.Element
import scala.xml.Elem

case class SvgStyle(fill: Option[Paint], strokeColor: Option[Color], strokeWidth: Double) {
	def hasFill: Boolean = fill.isDefined
	def setFillStyle(g2: Graphics2D): Unit = fill.foreach { f => g2.setPaint(f) }

	def hasStroke: Boolean = strokeColor.isDefined
	def setStrokeStyle(g2: Graphics2D, scale: Double): Unit = {
		g2.setStroke(new BasicStroke((strokeWidth*scale).toFloat))
		strokeColor.foreach { c => g2.setColor(c) }
	}
	
	def withFill(fill: Option[Paint]) = SvgStyle(fill, strokeColor, strokeWidth);
	def withStrokeColor(strokeColor: Option[Color]) = SvgStyle(fill, strokeColor, strokeWidth);
	def withStrokeWidth(strokeWidth: Double) = SvgStyle(fill, strokeColor, strokeWidth);
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

	def forElement(parent: SvgStyle, defs: SvgDefs, e: Elem): SvgStyle =
		(e \ "@style").text.split("""\s*;\s*""").foldLeft(parent) { (s, v) => {
			val kv = v.split("""\s*:\s*""", 2)
			kv(0) match {
				case "fill" =>
					s.withFill(parseFill(kv(1), defs))
				case "stroke" =>
					s.withStrokeColor(parseColor(kv(1)))
				case "stroke-width" =>
					s.withStrokeWidth(
						if (kv(1).endsWith("px"))
							kv(1).substring(0, kv(1).length - 2).toDouble
						else
							kv(1).toDouble
					)
				case _ => s
			}
		} }
}
