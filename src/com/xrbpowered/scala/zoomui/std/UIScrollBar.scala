package com.xrbpowered.scala.zoomui.std

import com.xrbpowered.scala.zoomui.UIContainer
import java.awt.Color
import com.xrbpowered.scala.zoomui.UIHoverElement
import com.xrbpowered.scala.zoomui.GraphAssist
import java.awt.GradientPaint
import com.xrbpowered.scala.zoomui.DragActor
import com.xrbpowered.scala.zoomui.UIElement

class UIScrollBar(parent: UIContainer, val vertical: Boolean) extends UIContainer(parent) {
	import UIElement._
	import UIScrollBar._
	
	private class Thumb extends UIHoverElement(UIScrollBar.this) {
		var span = 0
		var top, bottom = 0f
		var down = false
		
		def updateLocation() {
			top = if(vertical) decButton.height else decButton.width
			bottom = if(vertical) incButton.y else incButton.x
			if(enabled) {
				val s = if(vertical) parent.get.width else parent.get.height
				val h = span*(bottom-top)/(max-min+span) match {
					case x if x<s => span = (s*(max-min)/(bottom-top-s)).ceil.toInt; s
					case x => x
				};
				val pos = _value*(bottom-top)/(max-min+span)+top
				
				if(vertical) {
					size = (s, h)
					location = (0, pos)
				}
				else {
					size = (h, s)
					location = (pos, 0)
				}
			}
		}
		
		override def acceptDrag(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[DragActor] =
			if(dragThumbActor.notifyMouseDown(pos, button, mods)) Some(dragThumbActor) else None
		override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
			if(button==Left) repaint { down = true; true } else false
		override def mouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], initiator: Option[UIElement]): Boolean =
			if(button==Left) repaint { down = false; true } else false
			
		override def paint(g: GraphAssist): Unit = {
			g.setPaint(
				if(down) UIButton.colorDown
				else if(vertical)
					new GradientPaint(0, 0, UIButton.colorGradTop, width, 0, UIButton.colorGradBottom)
				else
					new GradientPaint(0, 0, UIButton.colorGradTop, 0, height, UIButton.colorGradBottom)
			)
			g.fill(this)
			g.border(this, if(hover) UIButton.colorText else UIButton.colorBorder)
		}
	}
	
	private val dragThumbActor: DragActor = new DragActor() {
		var pos = 0f;
		
		override def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
			if(button==Left) {
				this.pos = if(vertical) thumb.y else thumb.x
				thumb.down = true
				true
			}
			else false
			
		override def notifyMouseMove(dx: Float, dy: Float): Boolean = repaint {
			pos += (if(vertical) dy else dx) * pixelScale;
			val s = (pos-thumb.top) / (thumb.bottom-thumb.top);
			if(value = (min + s*(max-min+thumb.span)).round) changed()
			true
		}
		
		override def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], target: Option[UIElement]): Boolean = repaint {
			thumb.down = false
			true
		}
	}
	
	var onChanged: this.type => Unit = { _ => () }
	
	def changed(): Unit = onChanged(this)
	
	private var min = 0
	private var max = 100
	private var step = 1
	def setRange(min: Int, max: Int, step: Int) = {
		this.min = min
		this.max = max
		this.step = step
		checkRange()
	}
	
	private def checkRange(): Unit = {
		if(max<min)
			max = min
		if(_value<min)
			_value = min
		if(_value>max)
			_value = max
		decButton.setEnabled(enabled);
		incButton.setEnabled(enabled);
		thumb.visible = enabled;
	}
	
	private var _value = 0
	def value = _value
	def value_= (v: Int): Boolean = {
		val old = _value
		_value = v
		checkRange()
		old!=_value
	}
	
	val decButton = new UIButtonBase(this) {
		override def action(): Unit =
			if(value = _value-step) changed()
		override def paint(g: GraphAssist): Unit = {
			if(down || hover)
				g.fill(this, if(down) UIButton.colorDown else colorBorder)
			g.setColor(if(enabled) colorArrow else colorArrowDisabled)
			if(vertical)
				drawUpArrow(g, (width/2f).toInt, (height/2f).toInt);
			else
				drawLeftArrow(g, (width/2f).toInt, (height/2f).toInt);
		}
	}
	
	val incButton = new UIButtonBase(this) {
		override def action(): Unit =
			if(value = _value+step) changed()
		override def paint(g: GraphAssist): Unit = {
			if(down || hover)
				g.fill(this, if(down) UIButton.colorDown else colorBorder)
			g.setColor(if(enabled) colorArrow else colorArrowDisabled)
			if(vertical)
				drawDownArrow(g, (width/2f).toInt, (height/2f).toInt);
			else
				drawRightArrow(g, (width/2f).toInt, (height/2f).toInt);
		}
	}
	
	private val thumb = new Thumb();
	
	def enabled: Boolean = max>min && step>0
	def setThumbSpan(v: Int) = thumb.span = v
	
	def setLength(length: Float) =
		size = if(vertical) (defaultWidth, length) else (length, defaultWidth) 
	
	override def layout(): Unit = {
		val s = if(vertical) width else height
		decButton.location = (0, 0)
		decButton.size = (s, s)
		incButton.location = if(vertical) (0, height-s) else (width-s, 0)
		incButton.size = (s, s)
	}
	
	override def paintSelf(g: GraphAssist): Unit = {
		g.fill(this, colorBg)
		g.border(this, colorBorder)
	}
	
	override def paintChildren(g: GraphAssist): Unit = {
		thumb.updateLocation()
		super.paintChildren(g)
	}
}
object UIScrollBar {
	var defaultWidth: Int = 16
	
	var colorBg: Color = new Color(0xf2f2f2)
	var colorBorder: Color = new Color(0xcccccc)
	var colorArrow: Color = UIButton.colorText
	var colorArrowDisabled: Color = UIButton.colorBorder
	
	def drawUpArrow(g: GraphAssist, x: Int, y: Int): Unit =
		g.graph.fillPolygon(Array[Int](x-4, x, x+4), Array[Int](y+2, y-2, y+2), 3)
	def drawDownArrow(g: GraphAssist, x: Int, y: Int): Unit =
		g.graph.fillPolygon(Array[Int](x-4, x, x+4), Array[Int](y-2, y+2, y-2), 3)
	def drawLeftArrow(g: GraphAssist, x: Int, y: Int): Unit =
		g.graph.fillPolygon(Array[Int](x+2, x-2, x+2), Array[Int](y-4, y, y+4), 3)
	def drawRightArrow(g: GraphAssist, x: Int, y: Int): Unit =
		g.graph.fillPolygon(Array[Int](x-2, x+2, x-2), Array[Int](y-4, y, y+4), 3)
}