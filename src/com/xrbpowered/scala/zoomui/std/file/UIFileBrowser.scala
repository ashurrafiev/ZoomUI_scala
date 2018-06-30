package com.xrbpowered.scala.zoomui.std.file

import java.awt.{Color, Font}
import java.io.File
import java.util

import com.xrbpowered.scala.zoomui.BaseContainer.ModalBaseContainer
import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer}
import com.xrbpowered.scala.zoomui.std.{UIButton, UITextBox, UIToolButton}

class UIFileBrowser(parent: ModalBaseContainer[Option[File]]) extends UIContainer(parent) {
	import UIFileBrowser._

	val view = new UIFileView(this, None, autoTypes = true)
	view.onDirectorySet = _ => {
		btnUp.setEnabled(view.directory.isDefined)
		txtPath.text = view.directory.fold("This computer") { _.getAbsolutePath }
	}
	view.onBrowse = _ => pushHistory()
	view.onFileSelected = (_, file) => txtFileName.text = file.getName

	val history = new util.LinkedList[Option[File]]()
	var historyIndex: Int = -1

	// top pane
	val txtPath = new UITextBox(this)
	val btnBack: UIToolButton = new UIToolButton(this, UIToolButton.iconPath+"back.svg", 16, 2).disable
	btnBack.onAction = _ => if(historyIndex > 0) repaint { setHistory(historyIndex - 1) }
	val btnFwd: UIToolButton = new UIToolButton(this, UIToolButton.iconPath+"forward.svg", 16, 2).disable
	btnFwd.onAction = _ => if(historyIndex < history.size() - 1) repaint { setHistory(historyIndex + 1) }
	val btnRefresh: UIToolButton = new UIToolButton(this, UIToolButton.iconPath+"refresh.svg", 16, 2)
	btnRefresh.onAction = _ => repaint { view.refresh() }

	// left pane
	val btnUp = new UIToolButton(this, UIToolButton.iconPath+"up.svg", 32, 8)
	btnUp.onAction = _ => repaint { if (view.upDirectory) pushHistory() }
	val btnHome = new UIToolButton(this, UIToolButton.iconPath+"home.svg", 32, 8)
	btnHome.onAction = _ => repaint { if (view.setDirectory(Some(new File(System.getProperty("user.home"))))) pushHistory() }
	val btnRoots = new UIToolButton(this, UIToolButton.iconPath+"roots.svg", 32, 8)
	btnRoots.onAction = _ => repaint { if (view.setDirectory(None)) pushHistory() }

	// bottom pane
	val txtFileName = new UITextBox(this)
	val btnOk = new UIButton(this, "OK")
	btnOk.onAction = _ => parent.window.closeWithResult(view.selectedFile)
	val btnCancel = new UIButton(this, "Cancel")
	btnCancel.onAction = _ => parent.window.close()

	view.setDirectory(Some(new File(".")))
	pushHistory()

	private def setHistory(index: Int): Unit = {
		historyIndex = index
		view.setDirectory(history.get(index))
		btnBack.setEnabled(index > 0)
		btnFwd.setEnabled(index < history.size() - 1)
	}

	private def pushHistory(): Unit = {
		historyIndex += 1
		while (history.size > historyIndex) history.removeLast()
		history.add(view.directory)
		if (historyIndex > 0) btnBack.enable
	}

	override def layout(): Unit = {
		import com.xrbpowered.scala.zoomui.std.UIButton
		val top = txtFileName.height + 16
		val viewh = height - 24 - UIButton.defaultHeight * 2 - top
		view.location = (56, top)
		view.size = (width - 56, viewh)
		txtFileName.location = (56, height - UIButton.defaultHeight * 2 - 16)
		txtFileName.size = (width - 56 - 8, txtFileName.height)
		txtPath.location = (56, 8)
		txtPath.size = (width - 56 - 4 - 28, txtFileName.height)
		btnBack.location = (28 - 22, 8)
		btnFwd.location = (28 + 2, 8)
		btnRefresh.location = (width - 28, 8)
		btnUp.location = (4, top + 4)
		btnHome.location = (4, top + viewh - 48 * 2 - 4)
		btnRoots.location = (4, top + viewh - 48 - 4)
		btnOk.location = (width - UIButton.defaultWidth * 2 - 12, height - UIButton.defaultHeight - 8)
		btnCancel.location = (width - UIButton.defaultWidth - 8, height - UIButton.defaultHeight - 8)
		super.layout()
	}

	override def paintSelf(g: GraphAssist): Unit = {
		import GraphAssist._
		val top = (txtFileName.height + 16).asInstanceOf[Int]
		val viewh = height - 24 - UIButton.defaultHeight * 2 - top

		g.fillRect(0, 0, width, top, Color.WHITE)
		g.fillRect(0, top, width, height - top, new Color(0xf2f2f2))

		g.fillRect(0, top, 56, viewh, new Color(0xe4e4e4))
		g.setColor(new Color(0xcccccc))
		g.line(0, top, width, top)
		g.line(0, top + viewh, 56, top + viewh)

		g.setFont(font)
		g.setColor(colorText)
		g.drawString("File:", 52, txtFileName.y + txtFileName.height / 2f, Right)
	}
}
object UIFileBrowser {
	var font: Font = UIButton.font
	var colorText: Color = UIButton.colorText
}