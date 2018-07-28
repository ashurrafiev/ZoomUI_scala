package com.xrbpowered.scala.zoomui

class UIPanView(parent: UIContainer) extends UIContainer(parent) {
	import UIElement._

	private val panActor: DragActor = new DragActor {
		override def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
			button==Right
		override def notifyMouseMove(dx: Float, dy: Float): Boolean = repaint {
			val pix = pixelScale
			pan(dx * pix, dy * pix)
			true
		}
		override def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], target: Option[UIElement]): Boolean = true
	}

	protected var _pan: (Float, Float) = (0, 0)
	def getPan: (Float, Float) = _pan

	var _panRange: (Int, Int) = (UIPanView.UNLIMITED, UIPanView.UNLIMITED)
	def panRange: (Int, Int) = _panRange
	def panRange_= (range: (Int, Int)): Unit = { _panRange = range; checkPanRange() }

	def setPanRangeForClient(w: Float, h: Float): Unit =
		panRange = ((w - width).toInt max 0, (h - height).toInt max 0);
	
	private def checkPanRange(): Unit = { _pan = (
		if (_panRange._1 != UIPanView.DISABLED)
			if (_panRange._1 > 0) {
				if (_pan._1 < 0) 0f
				else if (_pan._1 > _panRange._1) _panRange._1
				else _pan._1
			}
			else _pan._1
		else 0,
		if (_panRange._2 != UIPanView.DISABLED)
			if (_panRange._2 > 0) {
				if (_pan._2 < 0) 0f
				else if (_pan._2 > _panRange._2) _panRange._2
				else _pan._2
			}
			else _pan._2
		else 0
	) }

	def setPan(x: Float, y:Float): Unit = {
		_pan = (x, y)
		checkPanRange()
	}
	def pan(dx: Float, dy: Float): Unit = {
		_pan = (_pan._1 - dx, _pan._2 - dy)
		checkPanRange()
	}
	def resetPan(): Unit = { 
		_pan = (0, 0)
		checkPanRange();
	}

	override def parentToLocal(pos: (Float, Float)): (Float, Float) = {
		val p = super.parentToLocal(pos)
		(p._1 + _pan._1, p._2 + _pan._2)
	}

	protected def applyTransform(g: GraphAssist): Unit =
		g.translate(-_pan._1, -_pan._2)

	override def paintChildren(g: GraphAssist): Unit =
		if(g.pushClip(0, 0, width, height)) {
			g.pushTx()
			applyTransform(g)
			super.paintChildren(g)
			g.popTx()
			g.popClip()
		}

	override def elementAt(pos: (Float, Float)): Option[UIElement] =
		restrictInside(pos)(super.elementAt(pos))

	override def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[UIElement] =
		restrictInside(pos)(super.notifyMouseDown(pos, button, mods))

	override def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], initiator: Option[UIElement]): Option[UIElement] =
		restrictInside(pos)(super.notifyMouseUp(pos, button, mods, initiator))

	override def notifyMouseScroll(pos: (Float, Float), delta: Float, mods: Set[Modifier]): Option[UIElement] =
		restrictInside(pos)(super.notifyMouseScroll(pos, delta, mods))

	override def acceptDrag(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[DragActor] =
		if(panActor.notifyMouseDown(pos, button, mods)) Some(panActor) else None

	override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
		super.mouseDown(pos, button, mods) || button == Right

}
object UIPanView {
	val UNLIMITED: Int = -1
	val DISABLED: Int = 0
}