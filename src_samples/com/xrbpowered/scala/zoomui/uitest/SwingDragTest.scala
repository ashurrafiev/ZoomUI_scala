package com.xrbpowered.scala.zoomui.uitest

import com.xrbpowered.scala.zoomui.UIContainer
import com.xrbpowered.scala.zoomui.swing.SwingWindowFactory
import com.xrbpowered.scala.zoomui.DragWindowActor
import com.xrbpowered.scala.zoomui.std.UIButton
import com.xrbpowered.scala.zoomui.std.UIFormattedLabel
import com.xrbpowered.scala.zoomui.GraphAssist
import java.awt.Color
import com.xrbpowered.scala.zoomui.UIElement
import com.xrbpowered.scala.zoomui.DragActor

class SwingDragTest(parent: UIContainer) extends UIContainer(parent) {
	import UIElement._
	
	val dragActor = new DragWindowActor(this)
	
	val btnClose = new UIButton(this, "Close")
	btnClose.onAction = _ => { base.window.close() }
	val label = new UIFormattedLabel(this, "You can <b>drag</b> this window using <span style=\"color:#777777\">Left Mouse Button</span>.");
	
	override def layout(): Unit = {
		btnClose.location = ((width-btnClose.width)/2f, height-btnClose.height-16)
		label.location = (16, 32)
		label.size = (width-32, 0)
	}
	
	override def paintSelf(g: GraphAssist): Unit = {
		g.fill(this, Color.WHITE)
	}

	override def acceptDrag(pos: (Float, Float), button: Button, mods: Set[Modifier]): Option[DragActor] =
		if(dragActor.notifyMouseDown(pos, button, mods)) Some(dragActor) else None

	override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
		isInside(pos) && dragActor.isTrigger(button, mods)

}
object SwingDragTest {
	def main(args: Array[String]): Unit = {
		val frame = SwingWindowFactory.use().createUndecorated(200, 200)
		new SwingDragTest(frame.container)
		frame.show
	}
}