package com.xrbpowered.scala.zoomui.std.text

import com.xrbpowered.scala.zoomui.UIContainer
import com.xrbpowered.scala.zoomui.std.UIScrollContainer

class UITextArea(parent: UIContainer) extends UIScrollContainer(parent) {
	val editor: UITextEditBase = createEditor
	protected def createEditor: UITextEditBase = new UITextEditBase(view, false)
	
	override def layoutView(): Float = {
		editor.location = (0, 0)
		editor.updateSize()
		editor.height
	}
}