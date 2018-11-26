package com.xrbpowered.scala.zoomui.uitest

import com.xrbpowered.scala.zoomui.swing.SwingFrame
import com.xrbpowered.scala.zoomui.std.text.UITextArea
import com.xrbpowered.scala.zoomui.std.text.UITextEditBase
import java.io.IOException
import java.io.DataInputStream
import java.io.InputStream
import java.io.FileInputStream
import com.xrbpowered.scala.zoomui.GraphAssist
import java.awt.Font
import com.xrbpowered.scala.zoomui.UIWindow
import com.xrbpowered.scala.zoomui.std.UIScrollContainer
import com.xrbpowered.scala.zoomui.UIWindowFactory

object TextEditTest {
	val TEST_INPUT = "src_samples/com/xrbpowered/scala/zoomui/uitest/TextEditTest.scala"
	
	def loadBytes(s: InputStream): Array[Byte] = {
		val in = new DataInputStream(s)
		val bytes = Array.ofDim[Byte](in.available())
		in.readFully(bytes)
		in.close()
		bytes
	}
	
	def loadString(path: String): String = try {
		new String(loadBytes(new FileInputStream(path)))
	}
	catch {
		case e: IOException => e.printStackTrace(); ""
	}
	
	def main(args: Array[String]): Unit = {
		val frame = UIWindowFactory.instance.create("TextEditTest", 800, 600, canResize = true)
		val text = new UITextArea(frame.container) {
			override def paintBorder(g: GraphAssist): Unit =
				g.border(this, GraphAssist.Top, UIScrollContainer.colorBorder)
		}
		text.editor.font = new Font("Verdana", Font.PLAIN, GraphAssist.ptToPixels(10f))
		text.editor.text = loadString(TEST_INPUT)
		frame.onClosing = UIWindow.confirmCosing
		frame.show()
	}
}