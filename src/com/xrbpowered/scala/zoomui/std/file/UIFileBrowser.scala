package com.xrbpowered.scala.zoomui.std.file

import java.awt.{Color, Font}
import java.io.File

import com.xrbpowered.scala.zoomui.BaseContainer.ModalBaseContainer
import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer}
import com.xrbpowered.scala.zoomui.std.History
import com.xrbpowered.scala.zoomui.std.text.UITextBox
import com.xrbpowered.scala.zoomui.std.UIButton
import com.xrbpowered.scala.zoomui.std.UIToolButton

class UIFileBrowser(parent: UIContainer, resultHandler: Option[File] => Unit, cancelHandler: () => Unit) extends UIContainer(parent) {
	import UIFileBrowser._

	val view = new UIFileView(this, None, autoTypes = true)
	view.onDirectorySet = _ => {
		btnUp.setEnabled(view.directory.isDefined)
		txtPath.editor.text = view.directory.fold("This computer") { _.getAbsolutePath }
	}
	view.onBrowse = _ => history.push()
	view.onFileSelected = (_, file) => txtFileName.editor.text = file.getName

	val history = new History[Option[File]](64) {
		override protected def apply(item: Option[File]) =
			view.setDirectory(item)
		override def push(): Unit =
			push(view.directory)
		override def update(): Unit = {
			btnBack.setEnabled(canUndo)
			btnFwd.setEnabled(canRedo)
		}
	}

	// top pane
	val txtPath = new UITextBox(this)
	val btnBack: UIToolButton = new UIToolButton(this, UIToolButton.iconPath+"back.svg", 16, 2).disable
	btnBack.onAction = _ => repaint { history.undo() }
	val btnFwd: UIToolButton = new UIToolButton(this, UIToolButton.iconPath+"forward.svg", 16, 2).disable
	btnFwd.onAction = _ => repaint { history.redo() }
	val btnRefresh: UIToolButton = new UIToolButton(this, UIToolButton.iconPath+"refresh.svg", 16, 2)
	btnRefresh.onAction = _ => repaint { view.refresh() }

	// left pane
	val btnUp = new UIToolButton(this, UIToolButton.iconPath+"up.svg", 32, 8)
	btnUp.onAction = _ => repaint { if (view.upDirectory) history.push() }
	val btnHome = new UIToolButton(this, UIToolButton.iconPath+"home.svg", 32, 8)
	btnHome.onAction = _ => repaint { if (view.setDirectory(Some(new File(System.getProperty("user.home"))))) history.push() }
	val btnRoots = new UIToolButton(this, UIToolButton.iconPath+"roots.svg", 32, 8)
	btnRoots.onAction = _ => repaint { if (view.setDirectory(None)) history.push() }

	// bottom pane
	val txtFileName = new UITextBox(this)
	val btnOk = new UIButton(this, "OK")
	btnOk.onAction = _ => resultHandler(view.selectedFile)
	val btnCancel = new UIButton(this, "Cancel")
	btnCancel.onAction = _ => cancelHandler()

	view.setDirectory(Some(new File(".")))
	history.push()

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