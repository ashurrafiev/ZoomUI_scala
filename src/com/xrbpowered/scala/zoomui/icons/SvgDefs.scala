package com.xrbpowered.scala.zoomui.icons

import java.awt.{Color, LinearGradientPaint, RadialGradientPaint}
import java.awt.MultipleGradientPaint.CycleMethod
import java.awt.geom.Point2D

import org.w3c.dom.{Element, Node}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class SvgDefs {
	import SvgDefs._

	val defs = new mutable.HashMap[String, Object]()

	private def readStops(e: Element): ArrayBuffer[(Float, Color)] = {
		val stops = new ArrayBuffer[(Float, Color)]
		var cn = e.getFirstChild
		while (cn != null) {
			if (cn.getNodeType == Node.ELEMENT_NODE) {
				val ce = cn.asInstanceOf[Element]
				if (ce.getNodeName.equals("stop")) {
					val offs = getAttrValue(ce, "offset", 1.0, 0.0).toFloat
					val attr = ce.getAttribute("style")
					if (!attr.isEmpty) {
						var color = Option.empty[Color]
						var opacity = 1.0
						val vals = attr.split(";")
						for (v <- vals) {
							val kv = v.split(":", 2)
							kv(0) = kv(0).trim
							kv(1) = kv(1).trim
							if (kv(0) == "stop-color") color = SvgStyle.parseColor(kv(1))
							else if (kv(0) == "stop-opacity") opacity = kv(1).toDouble
						}
						if (color.isDefined) {
							stops += ((offs, new Color(color.get.getRed, color.get.getGreen, color.get.getBlue, (opacity * 255.0).round.toInt)))
						}
					}
				}
			}
			cn = cn.getNextSibling
		}
		stops
	}

	def addDefs(g: Element, scale: Double): Unit = {
		var n = g.getFirstChild
		while (n != null) {
			if (n.getNodeType == Node.ELEMENT_NODE) {
				val e = n.asInstanceOf[Element]
				val id = e.getAttribute("id")
				var obj: Object = null
				if (e.getNodeName.equals("linearGradient")) {
					var p1: Point2D = new Point2D.Double(0, 0)
					var p2: Point2D = new Point2D.Double(0, 0)
					var spread = CycleMethod.NO_CYCLE

					var fractions = Array[Float]()
					var colors = Array[Color]()

					defs.get(e.getAttribute("xlink:href")) match {
						case Some(grad: LinearGradientPaint) =>
							p1 = grad.getStartPoint.clone.asInstanceOf[Point2D]
							p2 = grad.getEndPoint.clone.asInstanceOf[Point2D]
							spread = grad.getCycleMethod
							fractions = grad.getFractions
							colors = grad.getColors
						case _ => ()
					}

					p1.setLocation(getAttrValue(e, "x1", scale, p1.getX), getAttrValue(e, "y1", scale, p1.getY))
					p2.setLocation(getAttrValue(e, "x2", scale, p2.getX), getAttrValue(e, "y2", scale, p2.getY))
					spread = getAttrValue(e, "spreadMethod", Array[String]("pad", "reflect", "repeat"), CycleMethod.values, spread)

					val stops = readStops(e)
					if (stops.nonEmpty) {
						val num = stops.size
						fractions = new Array[Float](num)
						colors = new Array[Color](num)
						for(i <- stops.indices) {
							val stop = stops(i)
							fractions(i) = stop._1
							colors(i) = stop._2
						}
					}

					val tx = SvgFile.getTransform(e.getAttribute("gradientTransform"), scale)
					tx.transform(p1, p1)
					tx.transform(p2, p2)
					if (p1.equals(p2)) p2.setLocation(p2.getX, p2.getY + 0.001)
					obj = new LinearGradientPaint(p1.getX.toFloat, p1.getY.toFloat, p2.getX.toFloat, p2.getY.toFloat, fractions, colors, spread)
				}
				else if (e.getNodeName.equals("radialGradient")) {
					var pc: Point2D = new Point2D.Double(0, 0)
					var radius = 0.0
					var spread = CycleMethod.NO_CYCLE

					var fractions = Array[Float]()
					var colors = Array[Color]()

					defs.get(e.getAttribute("xlink:href")) match {
						case Some(grad: LinearGradientPaint) =>
							spread = grad.getCycleMethod
							fractions = grad.getFractions
							colors = grad.getColors
						case Some(grad: RadialGradientPaint) =>
							pc = grad.getCenterPoint.clone.asInstanceOf[Point2D]
							radius = grad.getRadius
							spread = grad.getCycleMethod
							fractions = grad.getFractions
							colors = grad.getColors
						case _ => ()
					}

					pc.setLocation(getAttrValue(e, "cx", scale, pc.getX), getAttrValue(e, "cy", scale, pc.getY))
					radius = getAttrValue(e, "r", scale, radius)
					spread = getAttrValue(e, "spreadMethod", Array[String]("pad", "reflect", "repeat"), CycleMethod.values, spread)

					val stops = readStops(e)
					if (stops.nonEmpty) {
						val num = stops.size
						fractions = new Array[Float](num)
						colors = new Array[Color](num)
						for(i <- stops.indices) {
							val stop = stops(i)
							fractions(i) = stop._1
							colors(i) = stop._2
						}
					}

					val tx = SvgFile.getTransform(e.getAttribute("gradientTransform"), scale)
					tx.transform(pc, pc)
					radius *= tx.getScaleX
					obj = new RadialGradientPaint(pc.getX.toFloat, pc.getY.toFloat, radius.toFloat, fractions, colors, spread)
				}

				if (obj != null && !id.isEmpty)
					defs.put("#" + id, obj)
			}
			n = n.getNextSibling
		}
	}
}
object SvgDefs {
	def getAttrValue(e: Element, name: String, scale: Double, d: Double): Double = {
		val s = e.getAttribute(name)
		if (s.isEmpty) d else s.toDouble * scale
	}

	def getAttrValue(e: Element, name: String, d: String): String = {
		val s = e.getAttribute(name)
		if (s.isEmpty) d else s
	}

	def getAttrValue[T](e: Element, name: String, keys: Array[String], vals: Array[T], d: T): T = {
		val s = e.getAttribute(name)
		if (s.isEmpty) d
		else {
			for (i <- keys.indices) if (s == keys(i)) return vals(i)
			d
		}
	}
}