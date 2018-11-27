package com.xrbpowered.scala.zoomui.swing

import java.awt.event._
import java.awt.{Dimension, Graphics, Graphics2D}

import com.xrbpowered.scala.zoomui._
import javax.swing.{JPanel, SwingUtilities}
import java.awt.Point

class BasePanel(val window: UIWindow) extends JPanel {
	import BasePanel._

	setFocusable(true)
	setFocusTraversalKeysEnabled(false)

	addComponentListener(new ComponentAdapter {
		override def componentResized(e: ComponentEvent):Unit = {
			window.notifyResized()
		}
	})

	addFocusListener(new FocusListener {
		override def focusGained(e: FocusEvent): Unit = { }
		override def focusLost(e: FocusEvent): Unit =
			window.container.resetFocus()
	})

	addKeyListener(new KeyAdapter {
		override def keyPressed(e: KeyEvent): Unit =
			if(window.container.keyPressed(e.getKeyChar, e.getKeyCode, getModifiers(e)))
				e.consume()
	})

	addMouseWheelListener(e => {
		val pos: (Float, Float) = (e.getX, e.getY)
		window.container.notifyMouseScroll(pos, e.getPreciseWheelRotation.toFloat, getModifiers(e))
	})

	addMouseListener(new MouseAdapter() {
		override def mousePressed(e: MouseEvent): Unit = {
			val pos: (Float, Float) = (e.getX, e.getY)
			val bm = getButtonMods(e)
			window.container.notifyMouseDown(pos, bm._1, bm._2)
		}
		override def mouseReleased(e: MouseEvent): Unit = {
			val pos: (Float, Float) = (e.getX, e.getY)
			val bm = getButtonMods(e)
			window.container.notifyMouseUp(pos, bm._1, bm._2, None)
		}
		override def mouseEntered(e: MouseEvent): Unit =
			window.container.mouseIn()
		override def mouseExited(e: MouseEvent): Unit =
			window.container.mouseOut()
	})

	addMouseMotionListener(new MouseAdapter() {
		override def mouseDragged(e: MouseEvent): Unit = {
			val pos: (Float, Float) = (e.getX, e.getY)
			window.container.mouseDragged(pos)
		}

		override def mouseMoved(e: MouseEvent): Unit = {
			val pos: (Float, Float) = (e.getX, e.getY)
			window.container.mouseMoved(pos, getModifiers(e))
		}
	})

	override def paintComponent(g: Graphics): Unit = {
		window.container.paint(new GraphAssist(g.asInstanceOf[Graphics2D]))
	}

	def resize(size: (Int, Int)): Unit = {
		val scale = window.container.baseScale
		setPreferredSize(new Dimension((size._1 * scale).toInt, (size._2 * scale).toInt))
		SwingUtilities.getWindowAncestor(this).pack()
		window.notifyResized()
	}

	private val _pt = new Point 
	def baseToScreen(pos: (Float, Float)): (Int, Int) = {
		_pt.setLocation(pos._1, pos._2)
		SwingUtilities.convertPointToScreen(_pt, this)
		return (_pt.x, _pt.y);
	}
	def screenToBase(pos: (Int, Int)): (Float, Float) = {
		_pt.setLocation(pos._1, pos._2)
		SwingUtilities.convertPointFromScreen(_pt, this)
		return (_pt.x, _pt.y);
	}
	
}
object BasePanel {
	import UIElement._

	private def getModifiers(e: InputEvent): Set[Modifier] =
		UIElement.modifiers(e.isControlDown, e.isAltDown, e.isShiftDown)

	private def getButtonMods(e: MouseEvent): (Button, Set[Modifier]) = (
		e.getButton match {
			case MouseEvent.BUTTON1 => Left
			case MouseEvent.BUTTON2 => Middle
			case MouseEvent.BUTTON3 => Right
			case _ => Unknown
		},
		getModifiers(e) )
}