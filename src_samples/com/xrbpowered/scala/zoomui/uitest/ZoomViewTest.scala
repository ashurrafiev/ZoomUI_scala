package com.xrbpowered.scala.zoomui.uitest

import java.awt.Color
import com.xrbpowered.scala.zoomui.std._
import com.xrbpowered.scala.zoomui._
import com.xrbpowered.scala.zoomui.icons.SvgIcon
import com.xrbpowered.scala.zoomui.swing.SwingFrame
import com.xrbpowered.scala.zoomui.std.text.UITextBox

class ZoomViewTest(parent: UIContainer) extends UIZoomView(parent) {

	val onClick: UIButton => Unit = b => { println(s"Clicked ${b.label}") }
	val btn1 = new UIButton(this, "Browse...")
	btn1.onAction = onClick
	val btn2 = new UIButton(this, "OK")
	btn2.onAction = onClick
	val btn3 = new UIButton(this, "Cancel")
	btn3.onAction = onClick

	val list = new UIListBox(this, (0 until 20).map( i => s"List item $i") )
	list.onItemSelected = (_, item) => { println(s"${item.data} selected") }

	val text = new UITextBox(this)
	text.editor.text = "Hello world"

	val toolBtn = new UIToolButton(this, ZoomViewTest.fileIcon, 32, 8)
	toolBtn.onAction = _ => {
		import UIMessageBox._
		show("Alert", "An instance of <b>UIToolButton</b> has been clicked" +
			" invoking <b>UIMessageBox</b> via <b>onAction</b> handler.",
			Some(iconAlert), Array(Ok), None)
	}

	val html: UIFormattedLabel = new UIFormattedLabel(this, "This is an example of a <b>formatted label</b>. Click <a href=\"link\">here</a> to test if the link works or not.") {
		override def setupHtmlKit(): Unit = {
			htmlKit.defaultHoverColor = Some(new Color(0x0099ff))
			htmlKit.defaultColor = UIButton.colorBorder
			htmlKit.getStyleSheet.addRule("a { text-decoration: none; color: #0077dd }")
		}
		onHrefClicked = (_, href) => println(s"[$href] clicked")
	}

	setPan(-64, 0)

	override def layout(): Unit = {
		btn1.location = (16, 16)
		btn2.location = (16, 16+24)
		btn3.location = (16+88+4, 16+24)
		list.location = (16, 16+48)
		list.size = (88*2+4, 120)
		list.layout()
		text.location = (16, 32+48+120)
		text.width = list.width
		toolBtn.location = (-40, list.y)

		html.location = (16, 64+48+120)
		html.size = (list.width, 0)
	}

	override def paintSelf(g: GraphAssist): Unit = {
		g.fill(this, Color.WHITE)
	}
}
object ZoomViewTest {

	val fileIcon = new SvgIcon(UIToolButton.iconPath+"file.svg", 160, UIToolButton.palette)

	def main(args: Array[String]): Unit = {
		val frame = new SwingFrame("ZoomViewTest", 800, 600)
		new UIContainer(frame.container) {
			val top: UIZoomView = new ZoomViewTest(this)
			val bottom: UIElement = new UIElement(Some(this)) {
				override def paint(g: GraphAssist): Unit = {
					import GraphAssist._
					g.fill(this, new Color(0xeeeeee))
					g.border(this, Top, new Color(0x999999))
					g.setColor(Color.BLACK)
					g.setFont(UIButton.font)
					g.drawString("Test UIZoomView and std controls", 16, height/2, Left)
				}
			}

			override def layout(): Unit = {
				bottom.size = (width, 100)
				bottom.location = (0, height - bottom.height)
				top.size = (width, bottom.y)
				top.location = (0, 0)
				top.layout()
			}
		}
		frame.onClosing = UIWindow.confirmCosing
		frame.show()
	}
}