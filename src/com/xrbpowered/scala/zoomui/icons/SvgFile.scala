package com.xrbpowered.scala.zoomui.icons

import java.awt.Graphics2D
import java.awt.geom.{AffineTransform, Arc2D, Path2D, Point2D}
import java.util.regex.Pattern

import javax.xml.parsers.DocumentBuilderFactory
import java.io.InputStream
import java.io.FileInputStream
import java.io.File
import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import java.awt.geom.Rectangle2D
import java.awt.geom.RoundRectangle2D
import java.awt.geom.Ellipse2D

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
		
		def nextCmd(i: Int, cmd: Char, s: Seq[String]): Unit =
			if (i<s.length) s(i)(0) match {
				case c if (c>='A' && c<='Z' || c>='a' && c<='z') => next(i+1, c, s)
				case _ => next(i, cmd, s)
			}

		def next(i: Int, cmd: Char, s: Seq[String]): Unit = {
			val cur = path.getCurrentPoint
			val (dx, dy): (Double, Double) = if (cur != null) (cur.getX, cur.getY) else (0, 0)
			cmd match {
				case 'M' =>
					path.moveTo(s(i).toDouble * scale, s(i + 1).toDouble * scale)
					nextCmd(i + 2, 'L', s)
				case 'm' =>
					path.moveTo(s(i).toDouble * scale + dx, s(i + 1).toDouble * scale + dy)
					nextCmd(i + 2, 'l', s)
				case 'L' =>
					path.lineTo(s(i).toDouble * scale, s(i + 1).toDouble * scale)
					nextCmd(i + 2, cmd, s)
				case 'l' =>
					path.lineTo(s(i).toDouble * scale + dx, s(i + 1).toDouble * scale + dy)
					nextCmd(i + 2, cmd, s)
				case 'V' =>
					path.lineTo(dx, s(i).toDouble * scale)
					nextCmd(i + 1, cmd, s)
				case 'v' =>
					path.lineTo(dx, s(i).toDouble * scale + dy)
					nextCmd(i + 1, cmd, s)
				case 'H' =>
					path.lineTo(s(i).toDouble * scale, dy)
					nextCmd(i + 1, cmd, s)
				case 'h' =>
					path.lineTo(s(i).toDouble * scale + dx, dy)
					nextCmd(i + 1, cmd, s)
				case 'C' =>
					path.curveTo(
						s(i).toDouble * scale, s(i + 1).toDouble * scale,
						s(i + 2).toDouble * scale, s(i + 3).toDouble * scale,
						s(i + 4).toDouble * scale, s(i + 5).toDouble * scale)
					nextCmd(i + 6, cmd, s)
				case 'c' =>
					path.curveTo(
						s(i).toDouble * scale + dx, s(i + 1).toDouble * scale + dy,
						s(i + 2).toDouble * scale + dx, s(i + 3).toDouble * scale + dy,
						s(i + 4).toDouble * scale + dx, s(i + 5).toDouble * scale + dy)
					nextCmd(i + 6, cmd, s)
				case 'Q' =>
					path.quadTo(
						s(i).toDouble * scale, s(i + 1).toDouble * scale,
						s(i + 2).toDouble * scale, s(i + 3).toDouble * scale)
					nextCmd(i + 4, cmd, s)
				case 'q' =>
					path.quadTo(
						s(i).toDouble * scale + dx, s(i + 1).toDouble * scale + dy,
						s(i + 2).toDouble * scale + dx, s(i + 3).toDouble * scale + dy)
					nextCmd(i + 4, cmd, s)
				case 'A' =>
					arcTo(path,
						s(i).toDouble * scale, s(i + 1).toDouble * scale, s(i + 2).toDouble,
						s(i + 3).toInt != 0, s(i + 4).toInt != 0,
						s(i + 5).toDouble * scale, s(i + 6).toDouble * scale)
					nextCmd(i + 7, cmd, s)
				case 'a' =>
					arcTo(path,
						s(i).toDouble * scale, s(i + 1).toDouble * scale, s(i + 2).toDouble,
						s(i + 3).toInt != 0, s(i + 4).toInt != 0,
						s(i + 5).toDouble * scale + dx, s(i + 6).toDouble * scale + dy)
					nextCmd(i + 7, cmd, s)
				case 'Z' | 'z' =>
					path.closePath()
					nextCmd(i, 0, s)
				case _ =>
					if (cmd != 0)
						System.err.println(s"Unknown path command: $cmd")
					nextCmd(i + 1, 0, s)
			}
		}
		
		nextCmd(0, 0, d.split("""[\,\s]\s*"""));
		path
	}
	
	private def createRect(e: Elem, scale: Double): Path2D = {
		val x = getAttrValue(e \ "@x", scale)
		val y = getAttrValue(e \ "@y", scale)
		val width = getAttrValue(e \ "@width", scale)
		val height = getAttrValue(e \ "@height", scale)
		val ry = getAttrValue(e \ "@ry", scale * 2.0)
		val rx = getAttrValue(e \ "@rx", scale * 2.0) match {
			case x if x <= 0 => ry
			case x => x
		}
		
		val path = new Path2D.Double
		if(ry<=0)
			path.append(new Rectangle2D.Double(x, y, width, height), false);
		else
			path.append(new RoundRectangle2D.Double(x, y, width, height, rx, ry), false);
		path
	}
	
	private def createCircle(e: Elem, scale: Double): Path2D = {
		val cx = (e \ "@cx").text.toDouble * scale
		val cy = (e \ "@cy").text.toDouble * scale
		val r = (e \ "@r").text.toDouble * scale
		val x = (cx - r).toInt
		val y = (cy - r).toInt
		val width = (r * 2.0).toInt
		
		val path = new Path2D.Double
		path.append(new Ellipse2D.Double(x, y, width, width), false);
		path
	}
	
	private def render(g2: Graphics2D, style: SvgStyle, scale: Double, path: Path2D): Unit = {
		if (style.hasFill) {
			style.setFillStyle(g2)
			g2.fill(path)
		}
		if (style.hasStroke) {
			style.setStrokeStyle(g2, scale)
			g2.draw(path)
		}
	}
	
	private def transformed(t: AffineTransform, path: Path2D): Path2D = { path.transform(t); path }

	private def render(g2: Graphics2D, g: Elem, defs: SvgDefs, parentStyle: SvgStyle, scale: Double): Unit =
		(g \ "_").foreach { _ match { case e: Elem =>
			val style = SvgStyle.forElement(parentStyle, defs, e)

			val t = g2.getTransform
			g2.transform(getTransform((e \ "@transform").text, scale))

			e.label match {
				case "g" => render(g2, e, defs, style, scale)
				case "defs" => defs.addDefs(e, scale)
				case "rect" =>
					render(g2, style, scale, createRect(e, scale))
				case "circle" =>
					render(g2, style, scale, createCircle(e, scale))
				case "path" =>
					render(g2, style, scale, createPath((e \ "@d").text, scale))
				case _ => ()
			}
			g2.setTransform(t)
		} }

	def render(g2: Graphics2D, scale: Double): Unit =
		root.foreach { render(g2, _, new SvgDefs, SvgStyle.Default, scale) }

	private def getPath(pathId: String, transform: AffineTransform, g: Elem, scale: Double): Option[Path2D] = {
		def findChild(i: Iterator[Node]): Option[Path2D] = if(!i.hasNext) None else i.next match {
			case e: Elem =>
				val t = new AffineTransform(transform)
				t.concatenate(getTransform((e \ "@transform").text, scale))

				e.label match {
					case "g" =>
						getPath(pathId, t, e, scale)
					case "rect" if (e \ "@id").text==pathId =>
						Some(transformed(t, createRect(e, scale)))
					case "circle" if (e \ "@id").text==pathId =>
						Some(transformed(t, createCircle(e, scale)))
					case "path" if (e \ "@id").text==pathId =>
						Some(transformed(t, createPath((e \ "@d").text, scale)))
					case _ => 
						findChild(i)
				}
			case _ => findChild(i)
		}
		findChild((g \ "_").iterator)
	}

	def getPath(pathId: String, scale: Double): Option[Path2D] =
		root.fold(None: Option[Path2D]) { getPath(pathId, new AffineTransform, _, scale) }
}
object SvgFile {
	def getTransform(tr: String, scale: Double): AffineTransform = {
		val tx = new AffineTransform
		for (m <- """([a-z]+)\((.*?)\)""".r.findAllMatchIn(tr)) {
			val s = m.group(2).split("""[\,\s]\s*""")
			m.group(1) match {
				case "translate" =>
					val x = s(0).toDouble * scale
					val y = if (s.length < 2) 0.0 else s(1).toDouble * scale
					tx.translate(x, y)
				case "scale" =>
					val x = s(0).toDouble
					val y = if (s.length < 2) x else s(1).toDouble
					tx.scale(x, y)
				case "matrix" =>
					val tm = new AffineTransform(Array[Double](
						s(0).toDouble, s(1).toDouble, s(2).toDouble, s(3).toDouble,
						s(4).toDouble * scale, s(5).toDouble * scale))
					tx.concatenate(tm)
				case _ => ()
			}
		}
		tx
	}

	private def getAttrValue(attr: NodeSeq, scale: Double): Double =
		attr.foldLeft(0.0)((_, s) => s.text.toDouble * scale)
}
