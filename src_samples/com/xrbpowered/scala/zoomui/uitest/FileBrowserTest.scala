package com.xrbpowered.scala.zoomui.uitest

import java.io.File
import com.xrbpowered.scala.zoomui.std.file.UIFileBrowser
import com.xrbpowered.scala.zoomui.swing.SwingModalDialog

object FileBrowserTest {
	def main(args: Array[String]): Unit = {
		val dlg = new SwingModalDialog("Open file", 840, 480, canResize = true, None: Option[File])
		dlg.onResult = { (_, file) => {
			println(file)
			System.exit(0)
		} }
		new UIFileBrowser(dlg.container)
		dlg.show()
	}
}
