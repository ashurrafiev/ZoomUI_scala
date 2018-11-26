package com.xrbpowered.scala.zoomui.uitest

import java.awt.{Color, Font}
import com.xrbpowered.scala.zoomui._
import com.xrbpowered.scala.zoomui.swing.SwingFrame

class PanViewTest(parent: UIContainer) extends UIPanView(parent) {
	var selected: Option[TestButton] = None

	class TestButton(parent: Option[UIContainer], var label: String) extends UIElement(parent) {
		import UIElement._

		var hover = false

		override def mouseIn(): Unit = repaint { hover = true }
		override def mouseOut(): Unit = repaint { hover = false }
		override def mouseDown(pos: (Float, Float), button: Button, mods: Set[Modifier]): Boolean =
			if (button == Left) repaint {
				selected = Some(this)
				true
			}
			else false

		override def paint(g: GraphAssist): Unit = {
			import GraphAssist._
			val sel = selected.contains(this)

			g.fill(this,
				if (sel) new Color(0x0077dd)
				else if (hover) new Color(0xe8f0ff)
				else Color.WHITE )

			g.setColor(if (sel) Color.WHITE else Color.BLACK)
			g.setFont(PanViewTest.font)
			g.drawString(label, 10, height/2, Left)
		}
	}

	private val btn = Array.tabulate(30) { i => new TestButton(Some(this), s"List item $i") }

	override def layout(): Unit = {
		val y = btn.foldLeft(0f) {(y, b) => {
			b.size = (width, 20f)
			b.location = (0, y)
			y + 20f
		}}
		panRange = (0, (y - height).toInt max 0)
	}

	override def paintSelf(g: GraphAssist): Unit =
		g.fill(this, Color.WHITE)

	override def paintChildren(g: GraphAssist): Unit = {
		super.paintChildren(g)

		if(panRange._2>0) {
			g.setColor(new Color(0xdddddd))
			g.fillRect(width - 4f, 0, 4, height, new Color(0xdddddd))

			val s = height / (panRange._2.toFloat + height)
			val top = getPan._2 * s
			val h = height * s
			g.fillRect(width - 5f, top, 5, h, new Color(0x777777))
		}
	}
}
object PanViewTest {
	val font: Font = new Font("Tahoma", Font.PLAIN, GraphAssist.ptToPixels(9f))

	def main(args: Array[String]): Unit = {
		new PanViewTest(UIWindowFactory.instance.create("PanViewTest", 400, 300, canResize = true).container).base.window.show()
	}
}
