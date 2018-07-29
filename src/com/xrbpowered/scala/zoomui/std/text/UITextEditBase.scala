package com.xrbpowered.scala.zoomui.std.text

import com.xrbpowered.scala.zoomui.KeyInputHandler
import com.xrbpowered.scala.zoomui.UIElement
import com.xrbpowered.scala.zoomui.UIHoverElement
import com.xrbpowered.scala.zoomui.UIContainer
import com.xrbpowered.scala.zoomui.UIPanView
import com.xrbpowered.scala.zoomui.GraphAssist
import com.xrbpowered.scala.zoomui.std.UIButton
import com.xrbpowered.scala.zoomui.std.UIListItem
import scala.collection.mutable.ArrayBuffer
import com.xrbpowered.scala.zoomui.DragActor
import com.xrbpowered.scala.zoomui.std.History
import java.awt.Rectangle
import java.awt.FontMetrics
import java.awt.RenderingHints
import java.awt.Cursor
import java.awt.event.KeyEvent
import com.xrbpowered.scala.utils.IteratorUtils._
import java.awt.Color
import java.awt.datatransfer.UnsupportedFlavorException
import java.awt.datatransfer.StringSelection
import java.awt.datatransfer.Clipboard
import java.awt.datatransfer.DataFlavor
import java.io.IOException
import java.awt.Toolkit

class UITextEditBase(parent: UIPanView, val singleLine: Boolean) extends UIHoverElement(parent) with KeyInputHandler {
	import UIElement._
	import UITextEditBase._
	
	var autoIndent = true
	
	var font = UIButton.font
	var colorBackground = UITextBox.colorBackground
	var colorHighlight = UIListItem.colorHighlight
	var colorText = UITextBox.colorText
	var colorSelection = UITextBox.colorSelection
	var colorSelectedText = UITextBox.colorSelectedText
	
	class Line {
		var offs: Int = 0
		var length: Int = 0
		var width: Option[Int] = None
		
		def calcStart: Int =
			lines.iterator.foldLeftUntil(0, this)( (pos, line) => pos+line.offs+line.length ) + offs
	}
	
	private val dragSelectActor: DragActor = new DragActor() {
		private var x, y = 0f
		
		override def notifyMouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
			if(button==Left) {
				checkPushHistory(Unspecified)
				baseToLocal(pos) match { case p => x = p._1; y = p._2 }
				cursorToMouse(x, y)
				startSelection()
				true
			}
			else false
			
		override def notifyMouseMove(dx: Float, dy: Float): Boolean = repaint {
			x += dx * pixelScale
			y += dy * pixelScale
			cursorToMouse(x, y)
			scrollToCursor()
			modifySelection(true)
			true
		}
		
		override def notifyMouseUp(pos: (Float, Float), button: Button, mods: Set[Modifier], target: Option[UIElement]): Boolean = true
	}
	
	class HistoryState(val text: String, val cursor: Position, val sel: Option[(Position, Position)]) {
		def this() = this(
				UITextEditBase.this._text,
				new Position(UITextEditBase.this.cursor),
				_sel.flatMap { p => Some((new Position(p._1), new Position(p._2))) }
			)
		
		def restore(): Unit = {
			setText(text, false)
			UITextEditBase.this.cursor.set(cursor)
			_sel = sel.flatMap { p => Some((new Position(p._1), new Position(p._2))) }
			updateSelRange()
			scrollToCursor()
		}
	}
	
	private var historyAction: HistoryAction = Unspecified
	val history = new History[HistoryState](32) {
		override protected def apply(item: HistoryState) =
			item.restore()
		override def push(): Unit =
			push(new HistoryState())
	}
	
	private var _text: String = ""
	def text: String = _text
	def text_= (t: String) = setText(t, true)
	
	private val lines = new ArrayBuffer[Line]()
	
	private val cursor: Position = new Position(0, 0)
	private var _sel: Option[(Position, Position)] = None
	private var _selRange: Option[(Position, Position)] = None
	
	private var cursorX: Option[Float] = None
	private class CursorLineCache(val lineIndex: Int, val line: Line, val lineStart: Int) {
		def this(lineIndex: Int, line: Line) = this(lineIndex, line, line.calcStart)
		def this(lineIndex: Int) = this(lineIndex, lines(lineIndex))
		def this() = this(cursor.line)
	}
	private var cursorLineCache: Option[CursorLineCache] = None
	private def checkCursorLineCache(): CursorLineCache = {
		val cc = cursorLineCache match {
			case Some(c) if(c.lineIndex==cursor.line) => c
			case _ => new CursorLineCache() 
		}
		cursorLineCache = Some(cc)
		cc
	}

	private var _displayLine = 0
	private var _pixelScale = 0f
	private var _lineHeight = 0
	private var _descent = 0
	private var _page = 0
	private var _tabWidth = 0
	private var _x0, _y0 = 0
	private val clipBounds = new Rectangle()
	private var _minx, _maxx, _maxy = 0
	private var _updateSize = false
	def updateSize(): Unit = _updateSize = true
	
	private var _fm: FontMetrics = null // will get value on paint, not supposed to be used before that
	
	text = ""

	def scrollToCursor(): Unit = {
		val (_panx, _pany) = parent.getPan
		
		val cc = checkCursorLineCache()
		val cx = _x0 + stringWidth(cc.lineStart, cc.lineStart+cursor.col)
		val panx =
			if(cx-_x0 < _minx)
				(cx-_x0)*_pixelScale;
			else if(cx+_x0 > _maxx)
				_panx + (cx+_x0-_maxx)*_pixelScale; // FIXME error in this branch
			else
				_panx
		
		val pany =
			if(singleLine) 0
			else if(_displayLine > cursor.line)
				cursor.line*_lineHeight*_pixelScale
			else if(_displayLine+_page<=cursor.line)
				_lineHeight*(cursor.line+1)*_pixelScale - parent.height
			else
				_pany
		
		parent.setPan(panx, pany);
	}
	
	private def setText(text: String, resetHistory: Boolean): Unit = {
		lines.clear()
		if(singleLine) {
			_text = newlineRegex.replaceAllIn(text, "").replaceAll("""\t""", "")
			val line = new Line()
			lines += line
			line.offs = 0
			line.length = _text.length
		}
		else {
			_text = text;
			def startLine(offs: Int): Line = {
				val line = new Line()
				lines += line
				line.offs = offs
				line
			}
			val (pos, line) = newlineRegex.findAllMatchIn(text).foldLeft( (0, startLine(0)) )(
				(x, m) => x match {
					case(pos, line) =>
						line.length = m.start - pos
						(m.end, startLine(m.end - m.start))
				} )
			line.length = text.length - pos;
		}
		cursorLineCache = None
		
		if(resetHistory) {
			history.clear()
			history.push()
		}
	}
	
	override def visible(clip: Rectangle): Boolean = visible
	
	private def updateMetrics(g: GraphAssist) = {
		_fm = g.getFontMetrics
		_lineHeight = _fm.getHeight
		
		_descent = _fm.getDescent
		_tabWidth = _fm.stringWidth("    ")
		_y0 = _lineHeight*(1+_displayLine)-_descent
		_x0 = (4/_pixelScale).toInt
		g.graph.getClipBounds(clipBounds)
		_minx = (clipBounds.getMinX()).floor.toInt
		_maxx = (clipBounds.getMaxX()).ceil.toInt
		_maxy = (clipBounds.getMaxY()).ceil.toInt
		_page = (_maxy - _y0) / _lineHeight
	}
	
	override def paint(g: GraphAssist): Unit = repaint {
		val focused = this.focused
		if(_lineHeight>0 && !singleLine)
			_displayLine = (parent.getPan._2 / _pixelScale / _lineHeight).toInt

		val aa = g.graph.getRenderingHint(RenderingHints.KEY_ANTIALIASING);
		g.graph.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);

		g.pushTx()
		g.clearTransform()
		g.translate(g.tx.getTranslateX, g.tx.getTranslateY)
		_pixelScale = pixelScale
		
		g.setFont(font.deriveFont(font.getSize/_pixelScale))
		updateMetrics(g)
		
		if(singleLine)
			g.fillRect(_minx, _y0-_lineHeight, _maxx-_minx, _maxy-_y0+_lineHeight, colorBackground)
		else {
			g.fillRect(_minx, _y0-_lineHeight, _maxx, _descent, colorBackground)
			if(_x0>_minx)
				g.fillRect(_minx, _y0-_lineHeight, _x0-_minx, _maxy-_y0+_lineHeight, colorBackground)
		}

		var y = if(singleLine) (parent.height/_pixelScale/2f+(_fm.getAscent-_fm.getDescent)/2f).toInt else _y0
		var pos = 0
		var w = 0f
		var lineIndex = 0
		lines.foreach( line => {
			val lineStart = pos+line.offs
			val lineEnd = lineStart+line.length
			if(line.width.isEmpty)
				line.width = Some(stringWidth(lineStart, lineEnd))
			w = w max line.width.get
			
			if(singleLine || lineIndex>=_displayLine && y-_lineHeight<_maxy) {
				val bg = if(lineIndex==cursor.line && focused && !singleLine) colorHighlight else colorBackground
				drawLine(g, lineIndex, lineStart, lineEnd, y, bg, focused)
				y += _lineHeight
			}
			
			pos = lineEnd
			lineIndex += 1
		} )
		if(!singleLine && y-_lineHeight<_maxy)
			g.fillRect(_minx, y-_lineHeight+_descent, _maxx, _maxy-y+_lineHeight-_descent, colorBackground)
		
		w = (w+_x0*2)*_pixelScale
		val h = if(singleLine) 0 else _lineHeight*lines.size*_pixelScale
		if(_updateSize || width!=w || height!=h) {
			parent.setPanRangeForClient(w, h)
			size = (w max parent.width, h max parent.height)
			_updateSize = false
		}
		
		g.popTx()
		g.graph.setRenderingHint(RenderingHints.KEY_ANTIALIASING, aa)
	}
	
	protected def drawLine(g: GraphAssist, lineIndex: Int, lineStart: Int, lineEnd: Int, y: Int, bg: Color, drawCursor: Boolean): Unit = {
		var x = _x0
		_selRange match {
			case Some((selMin, selMax)) if(lineIndex>=selMin.line && lineIndex<=selMax.line) =>
				var col = lineStart
				if(lineIndex==selMin.line && selMin.col>0) {
					col = (lineStart+selMin.col) min lineEnd
					x = drawText(g, x, y, lineStart, col, bg, colorText)
				}
				if(lineIndex==selMax.line && selMax.col<lineEnd-lineStart) {
					val cmax = lineStart+selMax.col
					if(col<cmax)
						x = drawText(g, x, y, col, cmax, colorSelection, colorSelectedText)
					x = drawText(g, x, y, cmax, lineEnd, bg, colorText)
					drawRemainder(g, x, y, bg)
				}
				else {
					x = drawText(g, x, y, col, lineEnd, colorSelection, colorSelectedText)
					drawRemainder(g, x, y, if(lineIndex<selMax.line) colorSelection else bg)
				}
			case _ => 
				x = drawText(g, x, y, lineStart, lineEnd, bg, colorText)
				drawRemainder(g, x, y, bg)
		}
		
		if(drawCursor && cursor.line==lineIndex) {
			val cx = stringWidth(lineStart, lineStart+cursor.col)
			g.graph.setXORMode(Color.BLACK)
			g.fillRect(_x0+cx, y-_lineHeight+_descent, 2f/_pixelScale, _lineHeight, Color.WHITE)
			g.graph.setPaintMode()
			if(cursorX.isEmpty)
				cursorX = Some((_x0+cx)*_pixelScale)
		}
	}
		
	protected def drawRemainder(g: GraphAssist, x: Int, y: Int, bg: Color): Unit =
		if(x<_maxx) g.fillRect(x, y-_lineHeight+_descent, _maxx-x, _lineHeight, bg)
	
	protected def drawText(g: GraphAssist, x: Int, y: Int, c0: Int, c1: Int, bg: Color, fg: Color): Int = {
		def drawString(s: String, x: Int): Int = {
			val w = _fm.stringWidth(s)
			if(x<_maxx && x+w>_minx) {
				g.fillRect(x, y-_lineHeight+_descent, w, _lineHeight, bg)
				g.setColor(fg)
				g.drawString(s, x, y)
			}
			x + w
		}
		def rec(c0: Int, c1: Int, x: Int, col: Int): Int = {
			val t = _text.indexOf('\t', col)
			if(t<0 || t>=c1) {
				if(col<c1) {
					val s = _text.substring(col, c1)
					drawString(s, x)
				}
				else x
			}
			else {
				val x2 =
					if(t>col) {
						val s = _text.substring(col, t)
						drawString(s, x)
					}
					else x
				val w = ((x2-_x0)+_tabWidth)/_tabWidth*_tabWidth-(x2-_x0)
				g.fillRect(x2, y-_lineHeight+_descent, w, _lineHeight, bg)
				rec(c0, c1, x2+w, t+1)
			}
		}
		rec(c0, c1, x, c0)
	}
	
	protected def stringWidth(c0: Int, c1: Int): Int = {
		def rec(c0: Int, c1: Int, x: Int, col: Int): Int = {
			val t = _text.indexOf('\t', col)
			if(t<0 || t>=c1) {
				if(col<c1) {
					val s = _text.substring(col, c1)
					x+_fm.stringWidth(s)
				}
				else x
			}
			else {
				val x2 =
					if(t>col) {
						val s = _text.substring(col, t)
						x + _fm.stringWidth(s)
					}
					else x
				val w = (x2+_tabWidth)/_tabWidth*_tabWidth-x2
				rec(c0, c1, x2+w, t+1)
			}
		}
		rec(c0, c1 min _text.length, 0, c0)
	}
	
	protected def searchCol(tx: Float): Int = {
		def bsearchCol(cstart: Int, c0: Int, x0: Int, c1: Int, x1: Int): Int =
			if(tx<=x0) c0
			else if(tx>=x1) c1
			else if(c1-c0==1) {
				if((tx-x0)*3f < (x1-tx)) c0 else c1
			}
			else {
				val s = (tx-x0) / (x1-x0).toFloat
				val c = c0+(s*(c1-c0)).toInt match {
					case x =>
						if(x==c0) c0+1
						else if(x==c1) c1-1
						else x
				}
				val w = stringWidth(cstart, c)
				if(tx<=w)
					return bsearchCol(cstart, c0, x0, c, w)
				else
					return bsearchCol(cstart, c, w, c1, x1)
			}
	
		val cc = checkCursorLineCache()
		val lineStart = cc.lineStart
		val lineEnd = lineStart+cc.line.length;
		return bsearchCol(lineStart, lineStart, 0, lineEnd, stringWidth(lineStart, lineEnd))-lineStart
	}
	
	private def cursorToMouse(x: Float, y: Float): Unit = {
		cursor.line = (if(singleLine) 0 else (y / _pixelScale / _lineHeight).toInt) max 0 min (lines.size-1)
		cursorX = Some(x)
		updateCursor()
	}
	
	private def updateCursor(): Unit = {
		cursor.col = searchCol(cursorX.get/_pixelScale-_x0)
	}
	
	def deselect(): Unit = {
		_sel = None
		updateSelRange()
	}
	
	def selectAll(): Unit ={
		val end = new Position(lines.size-1, lines(lines.size-1).length)
		_sel = Some((new Position(0, 0), end))
		cursor.set(end)
		updateSelRange()
	}
	
	def selectedText: Option[String] =
		_selRange match {
			case Some((min, max)) =>
				val start = lines(min.line).calcStart + min.col
				val end = lines(max.line).calcStart + max.col
				Some(_text.substring(start, end))
			case None => None
		}
	
	private def startSelection(): Unit =
		if(_sel.isEmpty) {
			_sel = Some((new Position(cursor), new Position(cursor)))
			updateSelRange()
		}
	
	private def modifySelection(keepStart: Boolean): Unit = _sel match {
		case Some((start, end)) =>
			end.set(cursor)
			if(start==end && !keepStart)
				deselect()
			updateSelRange()
		case None =>
	}
	private def modifySelection(): Unit = modifySelection(false)
	
	private def updateSelRange(): Unit =
		_selRange = _sel match {
			case Some((start, end)) => Some(
				if(start.line>end.line || start.line==end.line && start.col>end.col)
					(end, start)
				else
					(start, end)
			)
			case None =>  None
		}

	def setCursor(textPos: Int): Unit = {
		def rec(i: Iterator[Line], pos: Int, lineIndex: Int): Unit =
			if(i.hasNext) {
				val line = i.next
				val nextPos = pos+line.offs+line.length
				if(textPos<=nextPos) {
					cursor.line = lineIndex
					cursor.col = textPos - pos - line.offs
				}
				else
					rec(i, nextPos, lineIndex+1)
			}
		rec(lines.iterator, 0, 0)
	}
	
	def copySelection(): Unit =
		selectedText.foreach( s => {
			val clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()
			val con = new StringSelection(s)
			clipboard.setContents(con, con)
		} )
	
	def cutSelection(): Unit = {
		copySelection()
		deleteSelection()
		scrollToCursor()
	}
	
	def pasteAtCursor(): Unit = {
		history.push()
		
		var changed = deleteSelection(false)
		val cc = checkCursorLineCache()
		val pos = cc.lineStart+cursor.col

		val clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()
		if(clipboard.isDataFlavorAvailable(DataFlavor.stringFlavor)) {
			try {
				val add: String = clipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
				modify(pos, add, pos)
				// setText may strip newlines and tabs, setCursor must consider the difference
				val len = text.length
				setText(text, false)
				setCursor(pos+add.length-(len-_text.length))
				changed = true
			}
			catch {
				case _: UnsupportedFlavorException | _: IOException => ()
			}
		}
		
		if(changed) {
			history.push()
			historyAction = Unspecified
		}
		scrollToCursor()
	}
	
	private def deleteSelection(pushHistory: Boolean): Boolean =
		_selRange match {
			case Some((selMin, selMax)) =>
				if(pushHistory) history.push()
				
				cursor.set(selMin);
				if(selMin.line==selMax.line)
					modify(selMin.line, selMin.col, "", selMax.col);
				else {
					val start = lines(selMin.line).calcStart+selMin.col
					val end = lines(selMax.line).calcStart+selMax.col
					modify(start, "", end)
					setText(text, false)
				}
				deselect()
				
				if(pushHistory) {
					history.push()
					historyAction = Unspecified
				}
				true
			case _ => false
		}
	def deleteSelection(): Unit = deleteSelection(true)
	
	def indentSelection(indent: String): Unit = {
		var changed = false
		_selRange match {
			case Some((selMin, selMax)) =>
				val indentLen = indent.length
				val startLine = lines(selMin.line)
				var pos = startLine.calcStart-startLine.offs
				for(i <- selMin.line until selMax.line) {
					val line = lines(i)
					pos += line.offs
					if(line.length>0 && !(i==selMax.line && selMax.col==0)) {
						modify(pos, indent, pos)
						line.length += indentLen
						changed = true
					}
					pos += line.length
				}
				if(selMin.col>0) selMin.col += indentLen
				if(selMax.col>0) selMax.col += indentLen
				if(cursor.col>0) cursor.col += indentLen
			case None => ()
		}
		if(changed)
			history.push()
	}
	
	def unindentSelection(): Unit = {
		var changed = false
		_selRange match {
			case Some((selMin, selMax)) =>
				val startLine = lines(selMin.line)
				var pos = startLine.calcStart-startLine.offs
				for(i <- selMin.line until selMax.line) {
					val line = lines(i)
					pos += line.offs
					if(line.length>0 && !(i==selMax.line && selMax.col==0) &&
							Character.isWhitespace(text.charAt(pos))) {
						modify(pos, "", pos+1)
						line.length -= 1
						if(i==selMin.line && selMin.col>0) selMin.col -= 1
						if(i==selMax.line && selMax.col>0) selMax.col -= 1
						if(i==cursor.line && cursor.col>0) cursor.col -= 1
						changed = true
					}
					pos += line.length
				}
			case None => ()
		}
		if(changed)
			history.push()
	}
	
	private def joinLineWithNext(): Unit =
		if(!singleLine) {
			val cc = checkCursorLineCache()
			val next = lines(cursor.line+1)
			modify(cc.lineStart+cc.line.length, "", cc.lineStart+cc.line.length+next.offs)
			cc.line.length += next.length
			cc.line.width = None
			lines.remove(cursor.line+1)
		}

	private def splitLineAtCursor(): Int =
		if(singleLine) cursor.col
		else {
			val cc = checkCursorLineCache()
			val len = cc.line.length
			cc.line.length = cursor.col
			cc.line.width = None
			
			val indent =
				if(autoIndent) {
					indentRegex.findFirstMatchIn(_text.substring(cc.lineStart)).fold("")( m => {
						val s = m.group(0)
						if(cursor.col<s.length)
							s.substring(0, cursor.col)
						else s
					} )
				}
				else ""
			val indentLen = indent.length
			
			modify(cc.lineStart+cursor.col, newline+indent, cc.lineStart+cursor.col)
			val next = new Line()
			next.offs = newline.length
			next.length = len-cursor.col+indentLen
			lines.insert(cursor.line+1, next)
			
			indentLen
		}
	
	def modify(before: Int, add: String, after: Int): Int = {
		_text = _text.substring(0, before) + add + _text.substring(after)
		before-after+add.length
	}

	def modify(lineIndex: Int, before: Int, add: String, after: Int): Unit = {
		val cc = checkCursorLineCache()
		cc.line.length += modify(cc.lineStart+before, add, cc.lineStart+after)
		cc.line.width = None
	}
	
	private def checkPushHistory(action: HistoryAction): Unit =
		if(historyAction!=action) {
			if(historyAction!=Unspecified)
				history.push()
			historyAction = action
		}
		else {
			// TODO push after timer
		}
	
	protected def isCursorAtWordBoundary: Boolean = {
		val cc = checkCursorLineCache()
		if(cursor.col==0 || cursor.col==cc.line.length) true
		else {
			val ch = if(cc.lineStart+cursor.col==0) ' 'else _text.charAt(cc.lineStart+cursor.col-1)
			val ch1 = _text.charAt(cc.lineStart+cursor.col)
			Character.isWhitespace(ch) && !Character.isWhitespace(ch1) ||
					(Character.isAlphabetic(ch) || Character.isDigit(ch))!=(Character.isAlphabetic(ch1) || Character.isDigit(ch1)) ||
					Character.isLowerCase(ch) && Character.isUpperCase(ch1)
		}
	}
	
	override def keyPressed(ch: Char, code: Int, mods: Set[Modifier]): Boolean =
		super.keyPressed(ch, code, mods) || repaint {
			import KeyEvent._

			def moveActionDx[A](dx: Int)(action: => A): A = {
				checkPushHistory(Unspecified)
				if(mods.contains(Shift))
					startSelection()
				else {
					_selRange match {
						case Some((selMin, _)) if(dx<0) => cursor.set(selMin)
						case Some((_, selMax)) if(dx<0) => cursor.set(selMax)
						case _ => ()
					}
					deselect()
				}
				val res = action
				scrollToCursor()
				if(mods.contains(Shift))
					modifySelection(false)
				res
			}
			def moveAction[A](action: => A): A = moveActionDx(0)(action)
			
			code match {
				case VK_LEFT => moveActionDx(-1) {
					do {
						if(cursor.col>0) {
							cursor.col = (cursor.col min lines(cursor.line).length) - 1
							cursorX = None
						}
						else if(cursor.line>0) {
							cursor.line -= 1
							cursor.col = lines(cursor.line).length
						}
					} while(mods.contains(Ctrl) && !isCursorAtWordBoundary)
				}
				case VK_RIGHT => moveActionDx(1) {
					do {
						if(cursor.col<lines(cursor.line).length) {
							cursor.col += 1
							cursorX = None
						}
						else if(cursor.line<lines.size-1) {
							cursor.line += 1
							cursor.col = 0
						}
					} while(mods.contains(Ctrl) && !isCursorAtWordBoundary)
				}
				case VK_UP =>
					if(mods==Ctrl) {
						checkPushHistory(Unspecified)
						parent.pan(0, _lineHeight)
					}
					else moveAction {
						if(cursor.line>0) {
							cursor.line -= 1
							updateCursor()
						}
					}
				case VK_DOWN =>
					if(mods==Ctrl) {
						checkPushHistory(Unspecified)
						parent.pan(0, -_lineHeight)
					}
					else moveAction {
						if(cursor.line<lines.size-1) {
							cursor.line += 1
							updateCursor()
						}
					}
				case VK_PAGE_UP => moveAction {
						cursor.line = (cursor.line-_page) max 0
						updateCursor()
					}
				case VK_PAGE_DOWN => moveAction {
						cursor.line = (cursor.line+_page) min (lines.size-1)
						updateCursor()
					}
				case VK_HOME => moveAction {
						if(mods.contains(Ctrl)) cursor.line = 0
						cursor.col = 0
						cursorX = None
					}
				case VK_END => moveAction {
						if(mods.contains(Ctrl)) cursor.line = lines.size-1
						cursor.col = lines(cursor.line).length
						cursorX = None
					}
				
				case KeyEvent.VK_BACK_SPACE =>
					_sel match {
						case Some(_) =>
							deleteSelection()
							scrollToCursor()
						case None =>
							checkPushHistory(Deleting)
							if(cursor.col>0) {
								modify(cursor.line, cursor.col-1, "", cursor.col)
								cursor.col -= 1
								scrollToCursor()
							}
							else if(cursor.line>0) {
								cursor.col = lines(cursor.line-1).length
								cursor.line -= 1
								joinLineWithNext()
								scrollToCursor()
							}
					}
				case KeyEvent.VK_DELETE =>
					_sel match {
						case Some(_) =>
							deleteSelection()
							scrollToCursor()
						case None =>
							checkPushHistory(Deleting)
							if(cursor.col<lines(cursor.line).length) {
								modify(cursor.line, cursor.col, "", cursor.col+1)
							}
							else if(cursor.line<lines.size-1) {
								joinLineWithNext()
							}
					}
				
				case KeyEvent.VK_ENTER =>
					if(!singleLine) {
						deleteSelection()
						checkPushHistory(Typing)
						cursor.col = splitLineAtCursor()
						cursor.line += 1
						scrollToCursor()
					}
					else {
						checkPushHistory(Unspecified)
						base.resetFocus()
					}
				case KeyEvent.VK_ESCAPE =>
					checkPushHistory(Unspecified)
					base.resetFocus()
				
				case KeyEvent.VK_TAB =>
					if(!singleLine) _sel match {
						case None =>
							if(mods.none) {
								checkPushHistory(Typing)
								modify(cursor.line, cursor.col, "\t", cursor.col)
								cursor.col += 1
								scrollToCursor()
							}
						case Some(_) =>
							checkPushHistory(Unspecified);
							if(mods==Shift)
								unindentSelection()
							else
								indentSelection("\t")
					}
				
				case _ =>
					if(mods==Ctrl) {
						code match {
							case VK_A =>
								checkPushHistory(Unspecified)
								selectAll();
							case VK_X =>
								checkPushHistory(Unspecified)
								cutSelection()
							case VK_C =>
								checkPushHistory(Unspecified)
								copySelection()
							case VK_V =>
								checkPushHistory(Unspecified)
								pasteAtCursor()
							case VK_Z =>
								checkPushHistory(Unspecified)
								history.undo();
							case VK_Y =>
								checkPushHistory(Unspecified)
								history.redo();
							case _ => ()
						}
					}
					else {
						if(!Character.isISOControl(ch) && ch != KeyEvent.CHAR_UNDEFINED) {
							deleteSelection()
							checkPushHistory(Typing)
							modify(cursor.line, cursor.col, ch.toString, cursor.col)
							cursor.col += 1
							scrollToCursor()
						}
					}
			}
			true
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
			if(button == Left) repaint {
				if(!focused)
					base.focus = Some(this)
				else
					checkPushHistory(Unspecified)
				deselect()
				cursorToMouse(pos._1, pos._2)
				true
			}
			else false	)

	
	override def acceptDrag(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[DragActor] =
		Some(dragSelectActor).filter(_.notifyMouseDown(pos, button, mods))
	
	def focused: Boolean = base.focus.contains(this)
	
	override def focusGained(): Unit = repaint {
		super.focusGained()
	}

	override def focusLost(): Unit = repaint {
		deselect()
		super.focusLost()
	}
}
object UITextEditBase {
	val newlineRegex = """\r?\n""".r
	val indentRegex = """\s*""".r
	var newline = System.lineSeparator()
	
	class Position(var line: Int, var col: Int ) {
		def this(pos: Position) = this(pos.line, pos.col)
		def set(pos: Position) = { line = pos.line; col = pos.col }
		def == (pos: Position): Boolean = line==pos.line && col==pos.col
	}
		
	private sealed trait HistoryAction
	private object Unspecified extends HistoryAction
	private object Typing extends HistoryAction
	private object Deleting extends HistoryAction
}