package com.xrbpowered.scala.zoomui.std.text

import com.xrbpowered.scala.zoomui.std.UIButton
import com.xrbpowered.scala.zoomui.UIContainer
import com.xrbpowered.scala.zoomui.UIPanView
import java.awt.Color
import java.awt.Font
import com.xrbpowered.scala.zoomui.GraphAssist

class UITextBox(parent: UIContainer) extends UIPanView(parent) {
	import UITextBox._
	
	val editor: UITextEditBase = createEditor
	protected def createEditor: UITextEditBase = new UITextEditBase(this, true)
	size = (defaultWidth, defaultHeight)
	
	override def layout(): Unit = {
		editor.location = (0, 0)
		editor.updateSize()
	}
	
	override def paintChildren(g: GraphAssist): Unit = {
		super.paintChildren(g)
		g.border(this,
			if(editor.focused) colorSelection
			else if(editor.hover) colorText
			else colorBorder)
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