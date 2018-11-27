package com.xrbpowered.scala.zoomui

import java.awt._
import java.awt.geom.AffineTransform
import java.util

import scala.language.implicitConversions

class GraphAssist(val graph: Graphics2D) {
	import GraphAssist._

	private val _tx: util.LinkedList[AffineTransform] = new util.LinkedList[AffineTransform]()
	def tx: AffineTransform = _tx.getFirst
	def pushTx(): Unit = _tx.addFirst(graph.getTransform)
	def popTx(): Unit = graph.setTransform(_tx.removeFirst())

	def clearTransform(): Unit = graph.setTransform(new AffineTransform())
	def translate(tx: Double, ty: Double): Unit = graph.translate(tx, ty)
	def scale(scale: Double): Unit = graph.scale(scale, scale)

	private val _clip: util.LinkedList[Rectangle] = new util.LinkedList[Rectangle]()
	def pushClip(x: Float, y: Float, w: Float, h: Float): Boolean = {
		val clip = graph.getClipBounds
		val r = new Rectangle(x.toInt, y.toInt, w.toInt, h.toInt)
		if(r.intersects(clip)) {
			_clip.addFirst(clip)
			graph.setClip(r.intersection(clip))
			true
		}
		else false
	}
	def popClip(): Unit = graph.setClip(_clip.removeFirst())
	
	private val _aa: util.LinkedList[Boolean] = new util.LinkedList[Boolean]()
	def antialiasing: Boolean =
		graph.getRenderingHint(RenderingHints.KEY_ANTIALIASING)==RenderingHints.VALUE_ANTIALIAS_ON
	def pushAntialiasing(aa: Boolean): Unit = {
		_aa.addFirst(antialiasing)
		graph.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				if(aa) RenderingHints.VALUE_ANTIALIAS_ON else RenderingHints.VALUE_ANTIALIAS_OFF)
	}
	def popAntialiasing(): Unit = {
		val aa = _aa.removeFirst()
		graph.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				if(aa) RenderingHints.VALUE_ANTIALIAS_ON else RenderingHints.VALUE_ANTIALIAS_OFF)
	}

	private val _pureStroke: util.LinkedList[Boolean] = new util.LinkedList[Boolean]()
	def pureStroke: Boolean =
		graph.getRenderingHint(RenderingHints.KEY_STROKE_CONTROL)==RenderingHints.VALUE_STROKE_PURE
	def pushPureStroke(pure: Boolean): Unit = {
		_pureStroke.addFirst(antialiasing)
		graph.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL,
				if(pure) RenderingHints.VALUE_STROKE_PURE else RenderingHints.VALUE_STROKE_NORMALIZE)
	}
	def popPureStroke(): Unit = {
		val pure = _pureStroke.removeFirst()
		graph.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL,
				if(pure) RenderingHints.VALUE_STROKE_PURE else RenderingHints.VALUE_STROKE_NORMALIZE)
	}

	def setColor(c: Color): Unit = graph.setColor(c)
	def setFont(f: Font): Unit = graph.setFont(f)
	def getFontMetrics: FontMetrics = graph.getFontMetrics
	def setPaint(p: Paint): Unit = graph.setPaint(p)
	def setStroke(width: Float): Unit = graph.setStroke(new BasicStroke(width))
	def resetStroke(): Unit = graph.setStroke(defaultStroke)

	def fillRect(x: Float, y: Float, w: Float, h: Float): Unit = graph.fillRect(x.toInt, y.toInt, w.toInt, h.toInt)
	def fillRect(x: Float, y: Float, w: Float, h: Float, c: Color): Unit = { setColor(c); fillRect(x, y, w, h) }
	def fill(e: UIElement): Unit = fillRect(0, 0, e.width, e.height)
	def fill(e: UIElement, c: Color): Unit = fillRect(0, 0, e.width, e.height, c)

	def drawRect(x: Float, y: Float, w: Float, h: Float): Unit = graph.drawRect(x.toInt, y.toInt, w.toInt, h.toInt)
	def drawRect(x: Float, y: Float, w: Float, h: Float, c: Color): Unit = { setColor(c); drawRect(x, y, w, h) }
	def border(e: UIElement): Unit = drawRect(0, 0, e.width, e.height)
	def border(e: UIElement, c: Color): Unit = drawRect(0, 0, e.width, e.height, c)

	def line(x1: Float, y1: Float, x2: Float, y2: Float): Unit = graph.drawLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
	def line(x1: Float, y1: Float, x2: Float, y2: Float, c: Color): Unit = { setColor(c); line(x1, y1, x2, y2) }
	def border(e: UIElement, h: HAlign): Unit = line(h.x(e.width), 0, h.x(e.width), e.height)
	def border(e: UIElement, h: HAlign, c: Color): Unit = { setColor(c); border(e, h) }
	def border(e: UIElement, v: VAlign): Unit = line(0, v.y(e.height), e.width, v.y(e.height))
	def border(e: UIElement, v: VAlign, c: Color): Unit = { setColor(c); border(e, v) }

	def startPixelMode(e: UIElement, antialias: Boolean): Float = {
		pushAntialiasing(antialias)
		pushTx()
		clearTransform()
		translate(tx.getTranslateX, tx.getTranslateY)
		e.pixelScale
	}
	def startPixelMode(e: UIElement): Float = startPixelMode(e, false)
	def finishPixelMode(): Unit = {
		popTx()
		popAntialiasing()
	}
	
	def pixelBorder(e: UIElement, thickness: Int, fill: Option[Color], stroke: Option[Color]): Unit = {
		val pixelScale = startPixelMode(e)
		val w = Math.ceil(e.width / pixelScale).toInt
		val h = Math.ceil(e.height / pixelScale).toInt
		fill.foreach(c => {
			setColor(c)
			graph.fillRect(0, 0, w, h)
		})
		stroke.foreach(c => {
			resetStroke()
			setColor(c)
			for (i <- 0 until thickness)
				graph.drawRect(i, i, w-i*2-1, h-i*2-1)
		})
		finishPixelMode()
	}
	
	def drawString(str: String, x: Float, y: Float): Unit = graph.drawString(str, x, y)

	def drawString(str: String, x: Float, y: Float, align: Align): Float = {
		val fm = graph.getFontMetrics
		val w = fm.stringWidth(str)
		val h = fm.getAscent - fm.getDescent
		val tx = x - align.h.x(w)
		val ty = y + h - align.v.y(h)
		drawString(str, tx, ty)
		y + fm.getHeight
	}

}
object GraphAssist {
	def ptToPixels(pt: Float): Int = (96f * pt / 72f).round

	sealed case class HAlign(d: Float) {
		def + (v: VAlign): Align = Align(v, this)
		def x(width: Float): Float = width * d / 2f
	}
	sealed case class VAlign(d: Float) {
		def + (h: HAlign): Align = Align(this, h)
		def y(height: Float): Float = height * d / 2f
	}
	sealed case class Align(v: VAlign, h: HAlign)

	implicit def toAlign(h: HAlign): Align = Align(VAlign(1), h)
	implicit def toAlign(v: VAlign): Align = Align(v, HAlign(1))

	object Left extends HAlign(0)
	object Right extends HAlign(2)
	object Top extends VAlign(0)
	object Bottom extends VAlign(2)
	object Center extends Align(VAlign(1), HAlign(1))

	private val defaultStroke = new BasicStroke(1f)
}
