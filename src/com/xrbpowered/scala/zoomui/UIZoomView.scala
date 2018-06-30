package com.xrbpowered.scala.zoomui

class UIZoomView(parent: UIContainer) extends UIPanView(parent) {
	import UIElement._

	var scale = 1f
	private var _scaleRange: (Float, Float) = (0.1f, 3.0f)
	def scaleRange: (Float, Float) = _scaleRange
	def scaleRange_= (range: (Float, Float)): Unit = {
		_scaleRange = (range._1 min 1f, range._2 max 1f)
		checkScaleRange()
	}

	private def checkScaleRange(): Unit = {
		scale = scale max _scaleRange._1 min _scaleRange._2
	}
	def resetScale(): Unit = { scale = 1f }

	override def pan(dx: Float, dy: Float): Unit = super.pan(dx * scale, dy * scale)

	override def pixelScale: Float = super.pixelScale / scale

	override def parentToLocal(pos: (Float, Float)): (Float, Float) = {
		val p = super.parentToLocal(pos)
		(p._1 / scale, p._2 / scale)
	}

	override protected def applyTransform(g: GraphAssist): Unit = {
		super.applyTransform(g)
		g.scale(scale)
	}

	override def mouseScroll(pos: (Float, Float), delta: Float, mods: Set[Modifier]): Boolean =
		super.mouseScroll(pos, delta, mods) || (
			if (mods == Ctrl) repaint {
				scale *= 1.0f + delta * 0.2f
				checkScaleRange()
				true
			}
			else false )

}
