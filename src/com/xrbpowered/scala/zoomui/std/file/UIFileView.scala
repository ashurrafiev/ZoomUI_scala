package com.xrbpowered.scala.zoomui.std.file

import java.awt.{Color, Font, GradientPaint}
import java.io.File
import java.nio.file.Paths
import java.text.SimpleDateFormat

import com.xrbpowered.scala.utils.CacheMap
import com.xrbpowered.scala.zoomui.icons.SvgIcon
import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer, UIElement, UIHoverElement}
import com.xrbpowered.scala.zoomui.std._

import scala.collection.mutable.ArrayBuffer

class UIFileView(parent: UIContainer, val groupTypes: Option[Array[String]],
		val autoTypes: Boolean) extends UIScrollContainer(parent) {
	import UIFileView._
	import UIElement._

	private class FileListItem(parent: UIContainer, val file: File) extends UIHoverElement(parent) {
		val info: Option[String] =
			if (file.isFile)
				Some(dateFmt.format(file.lastModified()) + ", " + formatFileSize(file.length()))
			else if (file.getName.isEmpty)
				Some(formatFileSize(file.getFreeSpace) + " free, " + formatFileSize(file.getTotalSpace) + " total")
			else None

		val system: Boolean = startsWithSymbol(file.getName) || file.isHidden && !file.getName.isEmpty

		private var textSize: Option[(Int, Int)] = None

		override def paint(g: GraphAssist): Unit = {
			val sel = selectedFile.contains(file)
			val bgColor =
				if (sel) colorSelection
				else if (hover) colorHighlight
				else colorBackground
			g.fill(this, bgColor)

			val (fileName, disk) =
				if (file.getName.isEmpty) (file.getAbsolutePath, true)
				else (file.getName, false)

			var style = if (sel) 1 else 0
			if (system) style += 2
			(if (disk) diskIcon else if (file.isFile) fileIcon else folderIcon)
				.paint(g.graph, style, 20, 8, 32, pixelScale, useCache = true)

			g.setFont(font)
			g.setColor(if (sel) colorSelectedText else colorText)
			if (textSize.isEmpty) {
				val fm = g.getFontMetrics
				textSize = Some((fm.stringWidth(fileName), fm.getAscent - fm.getDescent))
			}
			val y = if (info == null) height / 2f + textSize.get._2 / 2f else height / 2f - 3f
			if (textSize.get._1 + 60 >= width - 8 && g.pushClip(0, 0, width - 8, height)) {
				g.drawString(fileName, 60, y)
				g.popClip()
				g.setPaint(new GradientPaint(width - 32, 0, new Color(bgColor.getRGB & 0xffffff, true), width - 8, 0, bgColor))
				g.fillRect(width - 32, 0, 24, height)
			}
			else
				g.drawString(fileName, 60, y)
			if (info.isDefined) {
				g.setColor(if (sel) colorDisabledSelectedText else colorDisabledText)
				g.drawString(info.get, 60, height / 2f + 3f + textSize.get._2)
			}
		}

		override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
			if(button == Left) repaint {
				if(selectedFile.contains(file))
					selectionClicked()
				else {
					selectedFile = Some(file)
					fileSelected(file)
				}
				true
			}
			else false
	}

	private class FileGroupBox(val order: Int, val title: String) extends UIContainer(this.view) with Ordered[FileGroupBox] {

		var numFiles = 0

		val header: UIElement = new UIHoverElement(this) {
			override def paint(g: GraphAssist): Unit = {
				g.fill(this, if (hover) colorHighlight else colorBackground)

				val open = body.visible
				g.setColor(if (open) colorSelection else colorDisabledText)
				val str = s"$title ($numFiles)"
				val fm = g.getFontMetrics
				val textWidth = fm.stringWidth(str)
				g.drawString(str, 20, 2+font.getSize)

				g.setColor(colorText)
				val y = (height/2f).toInt
				if (open)
					UIScrollBar.drawDownArrow(g, 10, y)
				else
					UIScrollBar.drawRightArrow(g, 10, y)

				g.setColor(colorBorderLight)
				g.line(textWidth + 28, y, width - 8, y)
			}

			override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
				if(button == Left) repaint {
					toggleView()
					true
				}
				else false
		}

		val body: UIContainer = new UIContainer(this) {
			override def layout(): Unit = {
				val w = LIST_ITEM_WIDTH
				val h = LIST_ITEM_HEIGHT
				val maxw = width
				val (x, y) = children.foldLeft((0f, 0f)) { (pos, e) => {
					e.size = (w, h)
					val p = if (pos._1 + w > maxw) (0f, pos._2 + h) else pos
					e.location = p
					(p._1 + w, p._2)
				} }
				height = y + h
			}
		}

		def addFile(file: File): Unit = { numFiles += 1; new FileListItem(body, file) }

		def toggleView(): Unit = { body.visible = !body.visible; invalidateLayout() }

		override def layout(): Unit = {
			header.location = (0, 0)
			header.size = (width, font.getSize + 8)
			height = if(body.visible) {
				body.location = (0, header.height)
				body.width = width
				body.layout()
				body.height + header.height + 8
			}
			else
				header.height + 8
		}

		override def compare(that: FileGroupBox): Int = {
			val res = order.compareTo(that.order)
			if (res==0) title.compareToIgnoreCase(that.title) else res
		}
	}

	private var _directory: Option[File] = None
	var selectedFile: Option[File] = None
	private val groups = new ArrayBuffer[FileGroupBox]()

	def directory: Option[File] = _directory

	def setDirectory(dir: Option[File]): Boolean = {
		val (_dir, files) = dir match {
			case None =>
				(None: Option[File], File.listRoots())
			case Some(d) =>
				val f = Paths.get(d.toURI.normalize()).toFile
				(Some(f), f.listFiles())
		}
		if(files==null)
			false
		else {
			_directory = _dir

			files.sortWith { (f1, f2) =>
				if (f1.isDirectory == f2.isDirectory)
					f1.getName.toLowerCase < f2.getName.toLowerCase
				else
					f1.isDirectory < f2.isDirectory
			}

			view.removeAllChildren()
			groups.clear()
			var dirGroup: Option[FileGroupBox] = None
			var rootGroup: Option[FileGroupBox] = None
			var allGroup: Option[FileGroupBox] = None
			val groupMap = new CacheMap[String, FileGroupBox]()
			files.foreach { file =>
				if (file.isDirectory)
					if (file.getName.isEmpty) {
						if (rootGroup.isEmpty) {
							rootGroup = Some(new FileGroupBox(-1, "File systems"))
							groups += rootGroup.get
						}
						rootGroup.get.addFile(file)
					}
					else {
						if (dirGroup.isEmpty) {
							dirGroup = Some(new FileGroupBox(0, "Folders"))
							groups += dirGroup.get
						}
						dirGroup.get.addFile(file)
					}
				else {
					var ft: Option[String] = None
					if (autoTypes || groupTypes.isDefined) {
						val fileName = file.getName
						val dotIndex = fileName.lastIndexOf('.')
						if (!startsWithSymbol(fileName) && dotIndex > 0) {
							val ext = fileName.substring(dotIndex + 1)
							if (autoTypes)
								ft = Some(ext.toLowerCase)
							else {
								ft = groupTypes.get.find(_.equalsIgnoreCase(ext))
							}
						}
					}
					if (ft.isEmpty) {
						if (allGroup.isEmpty) {
							allGroup = Some(new FileGroupBox(2,
								if (groupTypes.isEmpty && !autoTypes) "All files" else "All other files"))
							groups += allGroup.get
						}
						allGroup.get.addFile(file)
					}
					else {
						val t = ft.get
						var grp = groupMap.getOrCache(t) {
							val g = new FileGroupBox(1, t.toUpperCase + " files")
							groups += g
							g
						}
						grp.addFile(file)
					}
				}
			}
			groups.sortWith(_<_)

			selectedFile = None
			nothingSelected()
			onDirectorySet(this)
			true
		}
	}

	def refresh(): Unit = setDirectory(_directory)

	def upDirectory: Boolean =
		if(_directory.isDefined) {
			val parent = Paths.get(_directory.get.toURI).getParent
			setDirectory(if(parent!=null) Some(parent.toFile) else None)
		}
		else false

	override def layoutView(): Float = groups.foldLeft(0f) { (y, grp) => {
			grp.location = (0, y)
			grp.size = (width, 0)
			grp.layout()
			y + grp.height
		} }

	var onFileSelected: (this.type, File) => Unit = { (_, _) => () }
	var onNothingSelected: this.type => Unit = { _ => () }
	var onDirectorySet: this.type => Unit = { _ => () }
	var onBrowse: this.type => Unit = { _ => () }
	var onSelectedFileClicked: this.type => Unit = { _ => () }

	def fileSelected(file: File): Unit = onFileSelected(this, file)
	def nothingSelected(): Unit = onNothingSelected(this)
	def selectionClicked(): Unit = if (selectedFile.isDefined) {
		if (selectedFile.get.isDirectory)
			if (setDirectory(selectedFile)) onBrowse(this)
			else {
				import UIMessageBox._
				show("Error", "Access denied.", Some(iconError), Array(Ok), None)
			}
		else onSelectedFileClicked(this)
	}

	override def paintSelf(g: GraphAssist): Unit =
		g.fill(this, colorBackground)

	override def paintChildren(g: GraphAssist): Unit = {
		super.paintChildren(g)
		g.border(this, colorBorder)
	}
}
object UIFileView {
	private val dateFmt = new SimpleDateFormat("d MMM yyyy, HH:mm")

	private def formatFileSize(size: Long): String = {
		val prefs = Array("bytes", "KB", "MB", "GB", "TB")
		def fmt(d: Int, s: Double): String =
			if (d < prefs.length) {
				if (d > 0 && s < 10.0) "%.2f %s".format(s, prefs(d))
				else if (d > 0 && s < 100.0) "%.1f %s".format(s, prefs(d))
				else if (s < 1000.0) "%.0f %s".format(s, prefs(d))
				else fmt(d + 1, s / 1024.0)
			}
			else ""
		fmt(0, size.toDouble)
	}

	private def startsWithSymbol(s: String): Boolean = {
		if (s.isEmpty)
			false
		else {
			val ch = s.charAt(0)
			!(Character.isLetter(ch) || Character.isDigit(ch)) && ch != '_'
		}
	}

	var font: Font = UIButton.font

	var colorBackground: Color = Color.WHITE
	var colorBorder: Color = UIListBox.colorBorder
	var colorBorderLight = new Color(0xcccccc)
	var colorText: Color = UIListItem.colorText
	var colorHighlight: Color = UIListItem.colorHighlight
	var colorSelection: Color = UIListItem.colorSelection
	var colorSelectedText: Color = UIListItem.colorSelectedText
	var colorDisabledSelectedText = new Color(0x99ccff)
	var colorDisabledText = new Color(0x888888)

	private val fileIcon = new SvgIcon(UIToolButton.iconPath+"file.svg", 160, UIToolButton.palette)
	private val folderIcon = new SvgIcon(UIToolButton.iconPath+"folder.svg", 160, UIToolButton.palette)
	private val diskIcon = new SvgIcon(UIToolButton.iconPath+"disk.svg", 160, UIToolButton.palette)

	private val LIST_ITEM_WIDTH = 256
	private val LIST_ITEM_HEIGHT = 48
}
