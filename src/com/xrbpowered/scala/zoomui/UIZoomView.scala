package com.xrbpowered.scala.zoomui

class UIZoomView(parent: UIContainer) extends UIPanView(parent) {
	import UIElement._

	protected var _scale = 1f
	private var _scaleRange: (Float, Float) = (0.1f, 3.0f)
	def scaleRange: (Float, Float) = _scaleRange
	def scaleRange_= (range: (Float, Float)): Unit = {
		_scaleRange = (range._1 min 1f, range._2 max 1f)
		checkScaleRange()
	}

	private def checkScaleRange(): Unit = {
		_scale = _scale max _scaleRange._1 min _scaleRange._2
	}
	def resetScale(): Unit = {
		_scale = 1f
		checkScaleRange()
	}
	def setScale(s: Float): Unit = {
		_scale = s
		checkScaleRange()
	}
	def scale(ds: Float): Unit = {
		_scale *= ds
		checkScaleRange()
	}

	override def setPan(x: Float, y: Float): Unit = super.setPan(x * _scale, y * _scale)
	override def pan(dx: Float, dy: Float): Unit = super.pan(dx * _scale, dy * _scale)

	override def pixelScale: Float = super.pixelScale / _scale

	override def parentToLocal(pos: (Float, Float)): (Float, Float) = {
		val p = super.parentToLocal(pos)
		(p._1 / _scale, p._2 / _scale)
	}

	override protected def applyTransform(g: GraphAssist): Unit = {
		super.applyTransform(g)
		g.scale(_scale)
	}

	override def mouseScroll(pos: (Float, Float), delta: Float, mods: Set[Modifier]): Boolean =
		super.mouseScroll(pos, delta, mods) || (
			if (mods == Ctrl) repaint {
				scale(1.0f + delta * 0.2f)
				true
			}
			else false )

}
