package com.xrbpowered.scala.zoomui

import java.awt.Rectangle

import com.xrbpowered.scala.zoomui.UIElement.Modifier

abstract class UIElement(val parent: Option[UIContainer]) {
	import UIElement._

	def this(parent: UIContainer) = this(Some(parent))

	parent.foreach(_.addChild(this))

	protected def getBase: BaseContainer = parent.get.getBase
	val base: BaseContainer = getBase

	private var _visible: Boolean = true
	def visible: Boolean = _visible
	def visible_= (v: Boolean): Unit =
		_visible = v
	def visible(clip: Rectangle): Boolean =
		visible && !(clip.x-_x>width || clip.x-_x+clip.width<0 ||
				clip.y-_y>height || clip.y-_y+clip.height<0)

	private var _x: Float = 0
	private var _y: Float = 0
	private var _width: Float = 0
	private var _height: Float = 0

	def x: Float = _x
	def x_= (x: Float): Unit = { _x = x }
	def y: Float = _y
	def y_= (y: Float): Unit = { _y = y }
	def width: Float = _width
	def width_= (width: Float): Unit = { _width = width }
	def height: Float = _height
	def height_= (height: Float): Unit = { _height = height }

	def location: (Float, Float) = (x, y)
	def location_= (pos: (Float, Float)): Unit = { x = pos._1; y = pos._2 }
	def size: (Float, Float) = (width, height)
	def size_= (size: (Float, Float)): Unit = { width = size._1; height = size._2}

	def parentToLocal(pos: (Float, Float)): (Float, Float) =
		(pos._1 - this.x, pos._2 - this.y)

	def baseToLocal(pos: (Float, Float)): (Float, Float) =
		parentToLocal( parent match {
			case Some(p) => p.baseToLocal(pos)
			case None => pos
		} )

	def pixelScale: Float = parent.fold(1f)(_.pixelScale)

	def repaint[A](fwd: A): A = { base.window.repaint(); fwd }

	def invalidateLayout(): Unit = base.invalidateLayout()

	def layout(): Unit = {}

	def isInside(pos: (Float, Float)): Boolean =
		visible && pos._1>=x && pos._1<=x+width && pos._2>=y && pos._2<=y+height

	def paint(g: GraphAssist): Unit

	def restrictInside(pos: (Float, Float))(action: => Option[UIElement]): Option[UIElement] =
		if(isInside(pos)) action else None

	def acceptDrag(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[DragActor] = None

	def elementAt(pos: (Float, Float)): Option[UIElement] =
		restrictInside(pos)(Some(this))

	var onMouseIn: this.type => Unit = { _ => () }
	var onMouseOut: this.type => Unit = { _ => () }
	var onMouseReleased: this.type => Unit = { _ => () }
	var onMouseMoved: (this.type, (Float, Float), Set[Modifier]) => Unit = { (_, _, _) => () }
	var onMouseDown: (this.type, (Float, Float), Button, Set[Modifier]) => Boolean = { (_, _, _, _) => false }
	var onMouseUp: (this.type, (Float, Float), Button, Set[Modifier], Option[UIElement]) => Boolean = { (_, _, _, _, _) => false }
	var onMouseScroll: (this.type, (Float, Float), Float, Set[Modifier]) => Boolean = { (_, _, _, _) => false }

	def mouseIn(): Unit = onMouseIn(this)
	def mouseOut(): Unit = onMouseOut(this)
	def mouseReleased(): Unit = onMouseReleased(this)
	def mouseMoved(pos: (Float, Float), mods: Set[Modifier]): Unit = onMouseMoved(this, pos, mods)
	def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
		onMouseDown(this, pos, button, mods)
	def mouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], initiator: Option[UIElement]): Boolean =
		onMouseUp(this, pos, button, mods, initiator)
	def mouseScroll(pos: (Float, Float), delta: Float, mods: Set[Modifier]): Boolean =
		onMouseScroll(this, pos, delta, mods)

	def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[UIElement] = restrictInside(pos) {
		Option(this).filter(_ => mouseDown(pos, button, mods))
	}

	def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], initiator: Option[UIElement]): Option[UIElement] = restrictInside(pos) {
		Option(this).filter(_ => mouseUp(pos, button, mods, initiator))
	}

	def notifyMouseScroll(pos: (Float, Float), delta: Float, mods: Set[Modifier]): Option[UIElement] = restrictInside(pos) {
		Option(this).filter(_ => mouseScroll(pos, delta, mods))
	}
}
object UIElement {
	sealed trait Modifier
	private object _Ctrl extends Modifier
	private object _Alt extends Modifier
	private object _Shift extends Modifier
	val Ctrl: Set[Modifier] = Set(_Ctrl)
	val Alt: Set[Modifier] = Set(_Alt)
	val Shift: Set[Modifier] = Set(_Shift)

	implicit class ModSet(s: Set[Modifier]) {
		def + (m: Set[Modifier]): Set[Modifier] = s ++ m
		def contains(m: Set[Modifier]): Boolean = m subsetOf s
		def none: Boolean = s.isEmpty
	}

	def modifiers(ctrl: Boolean, alt: Boolean, shift: Boolean): Set[Modifier] =
		(if(ctrl) Ctrl else Set.empty[Modifier]) ++
		(if(alt) Alt else Set.empty[Modifier]) ++
		(if(shift) Shift else Set.empty[Modifier])

	sealed trait Button
	object Left extends Button
	object Right extends Button
	object Middle extends Button
	object Unknown extends Button

}
