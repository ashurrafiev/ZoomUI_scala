package com.xrbpowered.scala.zoomui.std

import java.awt.Color
import java.awt.event.KeyEvent

import com.xrbpowered.scala.zoomui.BaseContainer.ModalBaseContainer
import com.xrbpowered.scala.zoomui.icons.{IconPalette, SvgIcon}
import com.xrbpowered.scala.zoomui.std.UIMessageBox._
import com.xrbpowered.scala.zoomui.swing.SwingModalDialog
import com.xrbpowered.scala.zoomui._

class UIMessageBox(parent: ModalBaseContainer[MessageResult],
		message: String, val icon: Option[SvgIcon], val options: Array[MessageResult])
		extends UIContainer(parent) with KeyInputHandler {

	val buttons: Seq[UIButton] = for(i <- options.indices) yield new UIButton(this, options(i).label) {
		override def action(): Unit = notifyResult(options(i))
	}

	val label = new UIFormattedLabel(this, message)

	def notifyResult(res: MessageResult): Unit =
		parent.window.closeWithResult(res)

	override def keyPressed(ch: Char, code: Int, mods: Set[UIElement.Modifier]): Boolean =
		options.find(code==_.keyCode)
				.orElse { if (code==Cancel.keyCode) Some(Cancel) else None }
        		.fold(false) { opt => { notifyResult(opt); true } }

	override def layout(): Unit = {
		val x = if (icon.isEmpty) 0 else iconSize + 12
		val h = Math.max(label.height, if (icon.isEmpty) 0 else iconSize + 8)
		label.location = (x + 16, (h - label.height) / 2f + 8)
		label.size = (width - x - 32, 0)
		label.layout()

		val y = height - 8 - UIButton.defaultHeight
		for((btn, i) <- buttons.zipWithIndex) btn.location = (width - 4 - (UIButton.defaultWidth+4)*(buttons.length-i), y)
	}

	override def paint(g: GraphAssist): Unit = {
		val hlabel = label.height
		super.paint(g)
		val h = label.height max (if (icon.isEmpty) 0 else iconSize + 8)
		icon.foreach { _.paint(g.graph, 0, 16, 12, iconSize, pixelScale, useCache = true) }
		if (hlabel != label.height)
			base.window.clientSize = (width.toInt, (h + UIButton.defaultHeight + 40).toInt)
	}

	override def paintSelf(g: GraphAssist): Unit = {
		val w = width
		val y = height - 16 - UIButton.defaultHeight
		g.fillRect(0, 0, w, y, Color.WHITE)
		g.fillRect(0, y, w, height - y, new Color(0xf2f2f2))
		g.setColor(new Color(0xcccccc))
		g.line(0, y, w, y)
	}
}
object UIMessageBox {
	import KeyEvent._
	sealed class MessageResult(val label: String, val keyCode: Int)
	object Ok extends MessageResult("OK", VK_ENTER)
	object Yes extends MessageResult("Yes", VK_Y)
	object No extends MessageResult("No", VK_N)
	object Cancel extends MessageResult("Cancel", VK_ESCAPE)

	val iconError = new SvgIcon(UIToolButton.iconPath+"error.svg", 160, new IconPalette(Array(
		(new Color(0xeeeeee), new Color(0xeecccc), new Color(0xaa0000), Color.RED)
	)))
	val iconAlert = new SvgIcon(UIToolButton.iconPath+"alert.svg", 160, new IconPalette(Array(
		(new Color(0xeeeeee), new Color(0xeeddbb), new Color(0xdd5500), new Color(0xffaa00))
	)))
	val iconQuestion = new SvgIcon(UIToolButton.iconPath+"question.svg", 160, new IconPalette(Array(
		(new Color(0xeeeeee), new Color(0xccddee), new Color(0x0077dd), new Color(0x00bbff))
	)))
	val iconOk = new SvgIcon(UIToolButton.iconPath+"ok.svg", 160, new IconPalette(Array(
		(new Color(0xeeeeee), new Color(0xcceecc), new Color(0x007700), new Color(0x00ee00))
	)))

	val iconSize = 32

	def show(factory: UIWindowFactory, title: String, message: String, icon: Option[SvgIcon], options: Array[MessageResult],
			 onResult: Option[MessageResult => Unit]): Unit = {
		val width = Math.max(options.length * 2 + 1, 6) * (UIButton.defaultWidth + 4) / 2 + 32
		val dlg = factory.createModal[MessageResult](title, width, UIButton.defaultHeight + 40, false,
				onResult, UIModalWindow.cancelWithDefault(onResult, Cancel))
		new UIMessageBox(dlg.container, message, icon, options)
		dlg.show()
	}
	def show(factory: UIWindowFactory, title: String, message: String, icon: Option[SvgIcon], options: Array[MessageResult])
			(onResult: MessageResult => Unit): Unit =
		show(factory, title, message, icon, options, Some(onResult))
	def show(title: String, message: String, icon: Option[SvgIcon], options: Array[MessageResult],
			 onResult: Option[MessageResult => Unit]): Unit =
		show(UIWindowFactory.instance, title, message, icon, options, onResult)
	def show(title: String, message: String, icon: Option[SvgIcon], options: Array[MessageResult])
			(onResult: MessageResult => Unit): Unit =
		show(UIWindowFactory.instance, title, message, icon, options, Some(onResult))
}