package com.xrbpowered.scala.zoomui.icons

import java.awt.geom.{AffineTransform, Path2D, Rectangle2D}
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.image.BufferedImage

import com.xrbpowered.scala.utils.CacheMap

class SvgIcon(val uri: String, val baseSize: Int, val palette: IconPalette) {

	private var fgPath: Option[Path2D] = None
	private var bgPath: Option[Path2D] = None
	private var bounds: Option[Rectangle2D] = None

	val cache = new CacheMap[Int, BufferedImage]()

	def load: SvgIcon = {
		val svg = new SvgFile(uri)
		fgPath = svg.getPath("fg", 1)
		bgPath = svg.getPath("bg", 1)
		bounds = Some(fgPath.get.getBounds2D)
		if(bgPath.isDefined)
			bounds.get.add(bgPath.get.getBounds2D)
		this
	}

	private def getScale(size: Float, pixelScale: Float): Double =
		size / baseSize.toDouble / pixelScale

	def createImage(style: Int, scale: Double): BufferedImage = {
		if (this.bounds.isEmpty) load
		val bounds = this.bounds.get
		val img = new BufferedImage(Math.ceil(bounds.getWidth * scale + 2).toInt, Math.ceil(bounds.getHeight * scale + 2).toInt, BufferedImage.TYPE_INT_ARGB)

		val ig2 = img.getGraphics.asInstanceOf[Graphics2D]
		ig2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
		ig2.scale(scale, scale)
		ig2.translate(-bounds.getX + 1 / scale, -bounds.getY + 1 / scale)
		if (bgPath.isDefined) {
			ig2.setPaint(palette.getBgPaint(style, bounds.getY.toFloat, (bounds.getY + baseSize).toFloat))
			ig2.fill(bgPath.get)
		}
		ig2.setPaint(palette.getFgPaint(style, bounds.getY.toFloat, (bounds.getY + baseSize).toFloat))
		ig2.fill(fgPath.get)
		img
	}

	def createImage(style: Int, size: Float, pixelScale: Float): BufferedImage =
		createImage(style, getScale(size, pixelScale))

	def paint(g2: Graphics2D, style: Int, x: Float, y: Float, size: Float, pixelScale: Float, useCache: Boolean): Unit = {
		if (this.bounds.isEmpty) load
		val bounds = this.bounds.get
		if (useCache) {
			val imgSize = (size / pixelScale).toInt
			val scale = getScale(size, pixelScale)

			val key = imgSize * palette.colors.length + style
			var img = cache.getOrCache(key)(createImage(style, scale))
			val tx = g2.getTransform
			g2.setTransform(new AffineTransform)
			g2.translate(tx.getTranslateX + bounds.getX * scale + x / pixelScale, tx.getTranslateY + bounds.getY * scale + y / pixelScale)
			// g2.translate(tx.getTranslateX + bounds.getX * scale + x / pixelScale, tx.getTranslateY + (bounds.getY + baseSize) * scale + (y - size / 8f) / pixelScale) // FIXME wtf are these coordinates?
			g2.drawImage(img, -1, -1, null)
			g2.setTransform(tx)
		}
		else {
			val tx = g2.getTransform
			g2.translate(x, y)
			//g2.translate(x, y + size - 4) // FIXME wtf are these coordinates?
			val scale = size / baseSize.toDouble
			g2.scale(scale, scale)
			if (bgPath.isDefined) {
				g2.setPaint(palette.getBgPaint(style, -baseSize, 0))
				g2.fill(bgPath.get)
			}
			g2.setPaint(palette.getFgPaint(style, -baseSize, 0))
			g2.fill(fgPath.get)
			g2.setTransform(tx)
		}
	}
}
