package com.xrbpowered.scala.zoomui.icons

import java.awt.{Color, LinearGradientPaint, RadialGradientPaint}
import java.awt.MultipleGradientPaint.CycleMethod
import java.awt.geom.Point2D

import org.w3c.dom.{Element, Node}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.PrefixedAttribute

class SvgDefs {
	import SvgDefs._

	val defs = new mutable.HashMap[String, Object]()

	private def readStops(e: Elem): Seq[(Float, Color)] =
		(e \ "stop").map( ce => (
			(ce \ "@offset").text.toFloat,
			(ce \ "@style").text.split("""\s*;\s*""").foldLeft(Color.WHITE) { (c, v) => {
				def col(color: Color, a: Int): Color = new Color((color.getRGB & 0xffffff) | (a << 24), true)
				val kv = v.split("""\s*:\s*""", 2)
				kv(0) match {
					case "stop-color" =>
						col(SvgStyle.parseColor(kv(1)).get, c.getAlpha)
					case "stop-opacity" =>
						col(c, (kv(1).toDouble * 255.0).round.toInt)
					case _ => c
				}
			} }
		) )

	def addDefs(g: Elem, scale: Double): Unit = {
		(g \ "_").foreach { _ match { case e: Elem =>
			val obj: Option[Object] = e.label match {
				case "linearGradient" =>
					var p1: Point2D = new Point2D.Double(0, 0)
					var p2: Point2D = new Point2D.Double(0, 0)
					var spread = CycleMethod.NO_CYCLE
	
					var fractions = Array[Float]()
					var colors = Array[Color]()
					
					defs.get((e \ s"@{$xlink}href").text) match {
						case Some(grad: LinearGradientPaint) =>
							p1 = grad.getStartPoint.clone.asInstanceOf[Point2D]
							p2 = grad.getEndPoint.clone.asInstanceOf[Point2D]
							spread = grad.getCycleMethod
							fractions = grad.getFractions
							colors = grad.getColors
						case _ => ()
					}
	
					p1.setLocation(getAttrValue(e \ "@x1", scale, p1.getX), getAttrValue(e \ "@y1", scale, p1.getY))
					p2.setLocation(getAttrValue(e \ "@x2", scale, p2.getX), getAttrValue(e \ "@y2", scale, p2.getY))
					spread = getAttrValue(e \ "@spreadMethod", Array[String]("pad", "reflect", "repeat"), CycleMethod.values, spread)
	
					val stops = readStops(e)
					if (stops.nonEmpty) {
						val (f, c) = stops.unzip
						fractions = f.toArray
						colors = c.toArray
					}
	
					val tx = SvgFile.getTransform((e \ "@gradientTransform").text, scale)
					tx.transform(p1, p1)
					tx.transform(p2, p2)
					if (p1.equals(p2)) p2.setLocation(p2.getX, p2.getY + 0.001)
					Some(new LinearGradientPaint(p1.getX.toFloat, p1.getY.toFloat, p2.getX.toFloat, p2.getY.toFloat, fractions, colors, spread))
					
				case "radialGradient" =>
					var pc: Point2D = new Point2D.Double(0, 0)
					var radius = 0.0
					var spread = CycleMethod.NO_CYCLE
	
					var fractions = Array[Float]()
					var colors = Array[Color]()
	
					defs.get((e \ s"@{$xlink}href").text) match {
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
	
					pc.setLocation(getAttrValue(e \ "@cx", scale, pc.getX), getAttrValue(e \ "@cy", scale, pc.getY))
					radius = getAttrValue(e \ "@r", scale, radius)
					spread = getAttrValue(e \ "@spreadMethod", Array[String]("pad", "reflect", "repeat"), CycleMethod.values, spread)
	
					val stops = readStops(e)
					if (stops.nonEmpty) {
						val (f, c) = stops.unzip
						fractions = f.toArray
						colors = c.toArray
					}
	
					val tx = SvgFile.getTransform((e \ "@gradientTransform").text, scale)
					tx.transform(pc, pc)
					radius *= tx.getScaleX
					Some(new RadialGradientPaint(pc.getX.toFloat, pc.getY.toFloat, radius.toFloat, fractions, colors, spread))
					
				case _ => None
			}
			(e \ "@id").text match {
				case id if !id.isEmpty => obj.foreach(defs.put("#" + id, _))
				case _ => ()
			}
		} }
	}
}
object SvgDefs {
	val xlink = "http://www.w3.org/1999/xlink"

	def getAttrValue(attr: NodeSeq, scale: Double, d: Double): Double =
		attr.foldLeft(d)((_, s) => s.text.toDouble * scale)

	def getAttrValue[T](attr: NodeSeq, keys: Array[String], vals: Array[T], d: T): T =
		attr.text match {
			case s if s.isEmpty => d
			case s => keys.zipWithIndex.find(_._1 == s) match {
				case Some((_, i)) => vals(i)
				case None => d
			}
		}
}