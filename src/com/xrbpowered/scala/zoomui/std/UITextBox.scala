package com.xrbpowered.scala.zoomui.std

import java.awt._
import java.awt.event.KeyEvent

import com.xrbpowered.scala.zoomui._

class UITextBox(parent: UIContainer) extends UIHoverElement(parent) with KeyInputHandler {
	import UIElement._
	import UITextBox._

	size = (defaultWidth, defaultHeight)

	var text = ""
	private var _cursor = 0
	private var _sel: Option[(Int, Int)] = None
	private var _selRange: Option[(Int, Int)] = None
	private var cursorX = 0f
	private var updateCursor = false
	private var dragSelecting = false

	private val dragSelectActor: DragActor = new DragActor {
		private var x = 0f

		override def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
			if(button == Left) {
				dragSelecting = true
				this.x = baseToLocal((pos._1, 0))._1
				cursorX = this.x
				updateCursor = true
				startSelection()
				true
			}
			else false

		override def notifyMouseMove(dx: Float, dy: Float): Boolean = repaint {
			x += dx * pixelScale
			cursorX = x
			updateCursor = true
			true
		}

		override def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], target: Option[UIElement]): Boolean = {
			dragSelecting = false
			true
		}
	}

	override def paint(g: GraphAssist): Unit = {
		g.fill(this, colorBackground)

		val focused = this.focused

		if (g.pushClip(0, 0, width, height)) {
			g.pushTx()
			g.clearTransform()
			g.translate(g.tx.getTranslateX, g.tx.getTranslateY)
			val pix = pixelScale

			g.setFont(font.deriveFont(font.getSize / pix))
			val fm = g.getFontMetrics
			if (updateCursor) {
				_cursor = searchCol(fm, (cursorX - 4f) / pix)
				updateCursor = false
				if (dragSelecting)
					modifySelection(true)
			}

			val h = fm.getAscent - fm.getDescent
			val x0 = 4 / pix
			val y = height / pix / 2f + h / 2f
			val lh = fm.getHeight
			val descent = fm.getDescent

			if (_sel.isEmpty) {
				g.setColor(colorText)
				g.drawString(text, x0, y)
			}
			else {
				var x = x0
				val sel = _selRange.get
				if (sel._1 > 0) {
					g.setColor(colorText)
					val s = text.substring(0, sel._1)
					g.drawString(s, x, y)
					x += fm.getStringBounds(s, g.graph).getWidth.toFloat
				}
				val s = text.substring(sel._1, sel._2)
				val w = fm.getStringBounds(s, g.graph).getWidth.asInstanceOf[Float]
				g.fillRect(x, y - lh + descent, (x + w).toInt - x.toInt, lh, colorSelection)
				g.setColor(colorSelectedText)
				g.drawString(s, x, y)
				x += w
				if (sel._2 < text.length) {
					g.setColor(colorText)
					val s = text.substring(sel._2)
					g.drawString(s, x, y)
				}
			}

			if (focused) {
				val cx = fm.stringWidth(text.substring(0, _cursor))
				g.graph.setXORMode(Color.BLACK)
				g.fillRect(x0 + cx, y - lh + descent, 2f / pix, lh, Color.WHITE)
				g.graph.setPaintMode()
			}

			g.popTx()
			g.popClip()
		}

		g.border(this,
			if (focused) colorSelection
			else if (hover) colorText
			else colorBorder)
	}

	def focused: Boolean = base.focus.contains(this)

	def deselect(): Unit = {
		_sel = None
		updateSelRange()
	}

	def selectAll(): Unit = {
		_sel = Some(0, text.length)
		_cursor = text.length
		updateSelRange()
	}

	private def startSelection(): Unit = if(_sel.isEmpty) {
		_sel = Some(_cursor, _cursor)
		updateSelRange()
	}

	private def modifySelection(keepStart: Boolean): Unit = if(_sel.isDefined) {
		_sel = Some(_sel.get._1, _cursor)
		if(_sel.get._1 == _sel.get._2 && !keepStart)
			deselect()
		else
			updateSelRange()
	}

	private def updateSelRange(): Unit =
		_selRange = _sel match {
			case None => None
			case Some((a, b)) => Some(a min b, a max b)
		}

	def deleteSelection(): Boolean = if (_sel.isDefined) {
			_cursor = _selRange.get._1
			modify(_selRange.get._1, None, _selRange.get._2)
			deselect()
			true
		}
		else false

	def modify(before: Int, add: Option[Char], after: Int): Unit =
		text = text.substring(0, before) + add.getOrElse("") + text.substring(after)

	private def searchCol(fm: FontMetrics, tx: Float): Int =
		bsearchCol(fm, tx, (0, text.length), (0, fm.stringWidth(text)))

	private def bsearchCol(fm: FontMetrics, tx: Float, c: (Int, Int), w: (Int, Int)): Int =
		if (tx <= w._1) c._1
		else if (tx >= w._2) c._2
		else if (c._2 - c._1 == 1) {
			if ((tx - w._1) * 3f < (w._2 - tx)) c._1 else c._2
		}
		else {
			val s = (tx - w._1) / (w._2 - w._1).toFloat
			var cc = c._1 + (s * (c._2 - c._1)).toInt
			if (cc == c._1) cc = c._1 + 1
			if (cc == c._2) cc = c._2 - 1
			val wc = fm.stringWidth(text.substring(0, cc))
			if (tx <= wc)
				bsearchCol(fm, tx, (c._1, cc), (w._1, wc))
			else
				bsearchCol(fm, tx, (cc, c._2), (wc, w._2))
		}

	override def mouseIn(): Unit = repaint {
		base.window.setCursor(Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR))
		super.mouseIn()
	}
	override def mouseOut(): Unit = repaint {
		base.window.setCursor(Cursor.getDefaultCursor)
		super.mouseOut()
	}

	override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
		super.mouseDown(pos, button, mods) || (
			if(button == Left) {
				if (focused) repaint {
					cursorX = parentToLocal(pos)._1
					updateCursor = true
					deselect()
				}
				else
					base.focus = Some(this)
				true
			}
			else false	)

	override def acceptDrag(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[DragActor] =
		Some(dragSelectActor).filter(_.notifyMouseDown(pos, button, mods))

	override def keyPressed(ch: Char, code: Int, mods: Set[Modifier]): Boolean =
		super.keyPressed(ch, code, mods) || repaint {
			import KeyEvent._

			def withSel[A](action: => A): A = {
				if (mods == Shift) startSelection() else deselect()
				val res = action
				if (mods == Shift)
					modifySelection(false)
				res
			}

			code match {
				case VK_LEFT => withSel { _cursor = (_cursor - 1) max 0 }
				case VK_RIGHT => withSel { _cursor = (_cursor + 1) min text.length }
				case VK_HOME => withSel { _cursor = 0 }
				case VK_END => withSel { _cursor = text.length }
				case VK_BACK_SPACE =>
					if(!deleteSelection() && _cursor > 0) {
						modify(_cursor - 1, None, _cursor)
						_cursor -= 1
					}
				case VK_DELETE =>
					if(!deleteSelection() && _cursor < text.length) {
						modify(_cursor, None, _cursor + 1)
					}
				case VK_ENTER =>
					base.resetFocus()
				case _ =>
					if (!Character.isISOControl(ch) && ch != KeyEvent.CHAR_UNDEFINED) {
						deleteSelection()
						modify(_cursor, Some(ch), _cursor)
						_cursor += 1
					}
			}
			true
		}

	override def focusGained(): Unit = repaint {
		selectAll()
		_cursor = text.length
		super.focusGained()
	}

	override def focusLost(): Unit = repaint {
		deselect()
		super.focusLost()
	}
}
object UITextBox {
	var font: Font = UIButton.font

	var colorBackground: Color = Color.WHITE
	var colorText: Color = Color.BLACK
	var colorSelection = new Color(0x0077dd)
	var colorSelectedText: Color = Color.WHITE
	var colorBorder = new Color(0x888888)

	var defaultWidth = 120
	var defaultHeight = 20
}
