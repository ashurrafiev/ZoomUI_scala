package com.xrbpowered.scala.zoomui.uitest

import java.io.File
import com.xrbpowered.scala.zoomui.std.file.UIFileBrowser
import com.xrbpowered.scala.zoomui.swing.SwingModalDialog
import com.xrbpowered.scala.zoomui.UIWindowFactory

object FileBrowserTest {
	def main(args: Array[String]): Unit = {
		val dlg = UIWindowFactory.instance.createModal[Option[File]]("Open file", 840, 480, canResize = true,
			Some({ file => {
				println(file)
				System.exit(0)
			} }),
			Some({ () => {
				println("Cancelled")
				System.exit(0)
			} }))
		new UIFileBrowser(dlg.container, dlg.resultHandler, dlg.cancelHandler)
		dlg.show()
	}
}
