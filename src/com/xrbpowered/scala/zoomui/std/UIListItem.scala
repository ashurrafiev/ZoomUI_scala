package com.xrbpowered.scala.zoomui.std

import java.awt.{Color, Font}

import com.xrbpowered.scala.zoomui.{GraphAssist, UIElement}
import com.xrbpowered.scala.zoomui.std.text.UITextBox

class UIListItem[T](val list: UIListBox[T], val index: Int, val data: T) extends UIElement(Some(list.view)) {
	import UIElement._
	import UIListItem._

	protected var hover = false

	override def paint(g: GraphAssist): Unit = {
		import GraphAssist._

		val sel = list.selectedIndex.contains(index)
		g.fill(this,
			if (sel) colorSelection
			else if (hover) colorHighlight
			else Color.WHITE )
		g.setColor(if (sel) colorSelectedText else colorText)
		g.setFont(font)
		g.drawString(data.toString, 8, height/2, Left)
	}

	override def mouseIn(): Unit = repaint { hover = true; super.mouseIn() }
	override def mouseOut(): Unit = repaint { hover = false; super.mouseIn() }
	override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
		super.mouseDown(pos, button, mods) || (
		if(button == Left) repaint {
			if(list.selectedIndex.contains(index))
				list.selectionClicked()
			else
				list.select(index)
			true
		}
		else false )

}
object UIListItem {
	val font: Font = UIButton.font

	var colorText: Color = UITextBox.colorText
	var colorHighlight = new Color(0xe8f0ff)
	var colorSelection: Color = UITextBox.colorSelection
	var colorSelectedText: Color = UITextBox.colorSelectedText

	var itemHeight: Float = 20f
}
