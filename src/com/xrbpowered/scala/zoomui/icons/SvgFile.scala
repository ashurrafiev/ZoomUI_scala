package com.xrbpowered.scala.zoomui.icons

import java.awt.Graphics2D
import java.awt.geom.{AffineTransform, Arc2D, Path2D, Point2D}
import java.util.regex.Pattern

import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Element, Node}
import java.io.InputStream
import java.io.FileInputStream
import java.io.File
import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq

class SvgFile(uri: String) {
	import SvgFile._

	val root: Option[Elem] = try {
		val in: InputStream = ClassLoader.getSystemResourceAsStream(uri) match {
			case null => new FileInputStream(new File(uri))
			case in => in
		}
		val xml = XML.load(in)
		in.close()
		Some(xml)
	}
	catch {
		case e: Exception => e.printStackTrace(); None
	}

	// From org.apache.batik.ext.awt.geom.ExtendedGeneralPath.computeArc().
	private def arcTo(path: Path2D.Double, rx: Double, ry: Double, theta: Double,
				largeArc: Boolean, sweep: Boolean, x: Double, y: Double): Unit =
		// Ensure radii are valid
		if (rx == 0 || ry == 0) {
			path.lineTo(x, y)
		}
		else {
			// Get the current (x, y) coordinates of the path// Get the current (x, y) coordinates of the path
			val p2d = path.getCurrentPoint
			val x0 = p2d.getX
			val y0 = p2d.getY
			// Compute the half distance between the current and the final point
			val dx2 = (x0 - x) / 2.0
			val dy2 = (y0 - y) / 2.0
			// Convert theta from degrees to radians
			val th = Math.toRadians(theta % 360.0)

			// Step 1 : Compute (x1, y1)
			val x1 = Math.cos(th) * dx2 + Math.sin(th) * dy2
			val y1 = -Math.sin(th) * dx2 + Math.cos(th) * dy2
			val Px1 = x1 * x1
			val Py1 = y1 * y1

			// Ensure radii are large enough
			val (rx1, ry1) = {
				val rx2 = Math.abs(rx)
				val ry2 = Math.abs(ry)
				val d: Double = Px1 / (rx2 * rx2) + Py1 / (ry2 * ry2)
				if (d > 1)
					(Math.abs(Math.sqrt(d) * rx2), Math.abs(Math.sqrt(d) * ry2))
				else
					(rx2, ry2)
			}
			val Prx = rx1 * rx1
			val Pry = ry1 * ry1

			// Step 2 : Compute (cx1, cy1)
			val sign = if (largeArc == sweep) -1 else 1
			val coef = sign * Math.sqrt(((Prx * Pry) - (Prx * Py1) - (Pry * Px1)) / ((Prx * Py1) + (Pry * Px1)))
			val cx1 = coef * ((rx1 * y1) / ry1)
			val cy1 = coef * -((ry1 * x1) / rx1)

			// Step 3 : Compute (cx, cy) from (cx1, cy1)
			val sx2 = (x0 + x) / 2.0
			val sy2 = (y0 + y) / 2.0
			val cx = sx2 + (Math.cos(th) * cx1 - Math.sin(th) * cy1)
			val cy = sy2 + (Math.sin(th) * cx1 + Math.cos(th) * cy1)

			// Step 4 : Compute the angleStart (theta1) and the angleExtent (dtheta)
			val ux = (x1 - cx1) / rx1
			val uy = (y1 - cy1) / ry1
			val vx = (-x1 - cx1) / rx1
			val vy = (-y1 - cy1) / ry1
			// Compute the angle start
			val angleStart = {
				val n = Math.sqrt((ux * ux) + (uy * uy))
				val p = ux
				val sign = if (uy < 0) -1 else 1
				Math.toDegrees(sign * Math.acos(p / n))
			} % 360.0
			// Compute the angle extent
			val angleExtent = {
				val n = Math.sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy))
				val p = ux * vx + uy * vy
				val sign = if (ux * vy - uy * vx < 0) -1 else 1
				val a = Math.toDegrees(sign * Math.acos(p / n))
				if (!sweep && a > 0) a - 360.0
					else if (sweep && a < 0) a + 360.0
					else a
			} % 360.0

			val arc = new Arc2D.Double
			arc.x = cx - rx1
			arc.y = cy - ry1
			arc.width = rx1 * 2.0
			arc.height = ry1 * 2.0
			arc.start = -angleStart
			arc.extent = -angleExtent
			path.append(arc, true)
		}

	private def createPath(d: String, scale: Double): Path2D = {
		val path = new Path2D.Double
		val s: Array[String] = d.split("[\\,\\s]\\s*")
		var cmd = '\u0000'
		var i = 0
		while (i<s.length) {
			val c = s(i).charAt(0)
			if (c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z') {
				cmd = c
				i += 1
			}
			val cur = path.getCurrentPoint
			val (dx, dy): (Double, Double) = if (cur != null) (cur.getX, cur.getY) else (0, 0)
			cmd match {
				case 'M' =>
					path.moveTo(s(i).toDouble * scale, s(i + 1).toDouble * scale)
					i += 2
					cmd = 'L'
				case 'm' =>
					path.moveTo(s(i).toDouble * scale + dx, s(i + 1).toDouble * scale + dy)
					i += 2
					cmd = 'l'
				case 'L' =>
					path.lineTo(s(i).toDouble * scale, s(i + 1).toDouble * scale)
					i += 2
				case 'l' =>
					path.lineTo(s(i).toDouble * scale + dx, s(i + 1).toDouble * scale + dy)
					i += 2
				case 'V' =>
					path.lineTo(dx, s(i).toDouble * scale)
					i += 1
				case 'v' =>
					path.lineTo(dx, s(i).toDouble * scale + dy)
					i += 1
				case 'H' =>
					path.lineTo(s(i).toDouble * scale, dy)
					i += 1
				case 'h' =>
					path.lineTo(s(i).toDouble * scale + dx, dy)
					i += 1
				case 'C' =>
					path.curveTo(
						s(i).toDouble * scale, s(i + 1).toDouble * scale,
						s(i + 2).toDouble * scale, s(i + 3).toDouble * scale,
						s(i + 4).toDouble * scale, s(i + 5).toDouble * scale)
					i += 6
				case 'c' =>
					path.curveTo(
						s(i).toDouble * scale + dx, s(i + 1).toDouble * scale + dy,
						s(i + 2).toDouble * scale + dx, s(i + 3).toDouble * scale + dy,
						s(i + 4).toDouble * scale + dx, s(i + 5).toDouble * scale + dy)
					i += 6
				case 'Q' =>
					path.quadTo(
						s(i).toDouble * scale, s(i + 1).toDouble * scale,
						s(i + 2).toDouble * scale, s(i + 3).toDouble * scale)
					i += 4
				case 'q' =>
					path.quadTo(
						s(i).toDouble * scale + dx, s(i + 1).toDouble * scale + dy,
						s(i + 2).toDouble * scale + dx, s(i + 3).toDouble * scale + dy)
					i += 4
				case 'A' =>
					arcTo(path,
						s(i).toDouble * scale, s(i + 1).toDouble * scale, s(i + 2).toDouble,
						s(i + 3).toInt != 0, s(i + 4).toInt != 0,
						s(i + 5).toDouble * scale, s(i + 6).toDouble * scale)
					i += 7
				case 'a' =>
					arcTo(path,
						s(i).toDouble * scale, s(i + 1).toDouble * scale, s(i + 2).toDouble,
						s(i + 3).toInt != 0, s(i + 4).toInt != 0,
						s(i + 5).toDouble * scale + dx, s(i + 6).toDouble * scale + dy)
					i += 7
				case 'Z' | 'z' =>
					path.closePath()
				case _ =>
					if (cmd != '\u0000')
						System.err.println(s"Unknown path command: $cmd")
					cmd = '\u0000'
					i += 1
			}
		}
		path
	}

	private def render(g2: Graphics2D, g: Elem, defs: SvgDefs, parentStyle: SvgStyle, scale: Double, d: Int): Unit =
		(g \ "_").foreach { _ match { case e: Elem =>
			val style = SvgStyle.forElement(parentStyle, defs, e)

			val t = g2.getTransform
			g2.transform(getTransform((e \ "@transform").text, scale))

			e.label match {
				case "g" => render(g2, e, defs, style, scale, d+1)
				case "defs" => defs.addDefs(e, scale)
				case "rect" =>
					val x = getAttrValue(e \ "@x", scale)
					val y = getAttrValue(e \ "@y", scale)
					val width = getAttrValue(e \ "@width", scale)
					val height = getAttrValue(e \ "@height", scale)
					val ry = getAttrValue(e \ "@ry", scale * 2.0)
					val rx = getAttrValue(e \ "@rx", scale * 2.0) match {
						case x if x <= 0 => ry
						case x => x
					}
					if (style.hasFill) {
						style.setFillStyle(g2)
						if (ry <= 0) g2.fillRect(x, y, width, height)
						else g2.fillRoundRect(x, y, width, height, rx, ry)
					}
					if (style.hasStroke) {
						style.setStrokeStyle(g2, scale)
						if (ry <= 0) g2.drawRect(x, y, width, height)
						else g2.drawRoundRect(x, y, width, height, rx, ry)
					}
				case "circle" =>
					val cx = (e \ "@cx").text.toDouble * scale
					val cy = (e \ "@cy").text.toDouble * scale
					val r = (e \ "@r").text.toDouble * scale
					val x = (cx - r).toInt
					val y = (cy - r).toInt
					val width = (r * 2.0).toInt
					if (style.hasFill) {
						style.setFillStyle(g2)
						g2.fillOval(x, y, width, width)
					}
					if (style.hasStroke) {
						style.setStrokeStyle(g2, scale)
						g2.drawOval(x, y, width, width)
					}
				case "path" =>
					val path = createPath((e \ "@d").text, scale)
					if (style.hasFill) {
						style.setFillStyle(g2)
						g2.fill(path)
					}
					if (style.hasStroke) {
						style.setStrokeStyle(g2, scale)
						g2.draw(path)
					}
				case _ => ()
			}
			g2.setTransform(t)
		} }

	def render(g2: Graphics2D, scale: Double): Unit =
		root.foreach { render(g2, _, new SvgDefs, SvgStyle.Default, scale, 0) }

	private def getPath(pathId: String, transform: AffineTransform, g: Elem, scale: Double): Option[Path2D] = {
		(g \ "_").foreach { _ match { case e: Elem =>
				val t = new AffineTransform(transform)
				t.concatenate(getTransform((e \ "@transform").text, scale))

				e.label match {
					case "g" =>
						return getPath(pathId, t, e, scale)
					case "rect" =>
						// not supported, convert everything to paths
					case "circle" =>
						// not supported, convert everything to paths
					case "path" if (e \ "@id").text==pathId =>
						val path = createPath((e \ "@d").text, scale)
						path.transform(t)
						return Some(path)
					case _ => ()
				}
		} }
		None
	}

	def getPath(pathId: String, scale: Double): Option[Path2D] =
		root.fold(None: Option[Path2D]) { getPath(pathId, new AffineTransform, _, scale) }
}
object SvgFile {
	def getTransform(tr: String, scale: Double): AffineTransform = {
		val tx = new AffineTransform
		if (tr == null || tr.isEmpty) return tx
		val m = Pattern.compile("([a-z]+)\\((.*?)\\)").matcher(tr)
		var offs = 0
		while (m.find(offs)) {
			val t = m.group(1)
			val s = m.group(2).split("[\\,\\s]\\s*")
			if (t == "translate") {
				val x = s(0).toDouble * scale
				val y = if (s.length < 2) 0.0
				else s(1).toDouble * scale
				tx.translate(x, y)
			}
			else if (t == "scale") {
				val x = s(0).toDouble
				val y = if (s.length < 2) x
				else s(1).toDouble
				tx.scale(x, y)
			}
			else if (t == "matrix") {
				val tm = new AffineTransform(Array[Double](
					s(0).toDouble, s(1).toDouble, s(2).toDouble, s(3).toDouble,
					s(4).toDouble * scale, s(5).toDouble * scale))
				tx.concatenate(tm)
			}
			offs = m.end
		}
		tx
	}

	private def getAttrValue(attr: NodeSeq, scale: Double): Int =
		attr.foldLeft(0)((_, s) => (s.text.toDouble * scale).toInt)
}
