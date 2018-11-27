package com.xrbpowered.scala.zoomui.swing

import java.awt.Cursor
import java.awt.event.{WindowAdapter, WindowEvent}

import com.xrbpowered.scala.zoomui.UIModalWindow
import javax.swing.{JDialog, WindowConstants}

class SwingModalDialog[A](factory: SwingWindowFactory, title: String, w: Int, h: Int, canResize: Boolean) extends UIModalWindow[A](factory) {
	val dialog: JDialog = new JDialog
	dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
	dialog.setTitle(" "+title)
	dialog.setResizable(canResize)
	dialog.setModal(true)
	dialog.addWindowListener(new WindowAdapter {
		override def windowClosing(e: WindowEvent): Unit = requestClosing()
	})


	val panel = new BasePanel(this)
	dialog.setContentPane(panel)
	clientSize = (w, h)
	center()

	override def clientWidth: Int = panel.getWidth
	override def clientHeight: Int = panel.getHeight
	override def clientSize_=(size: (Int, Int)): Unit = panel.resize(size)
	override def x: Int = dialog.getX
	override def y: Int = dialog.getY
	override def moveTo(x: Int, y: Int): Unit = dialog.setLocation(x, y)
	override def center(): Unit = dialog.setLocationRelativeTo(null)
	override def show(): Unit = dialog.setVisible(true)
	override def repaint(): Unit = panel.repaint()
	override def setCursor(cursor: Cursor): Unit = panel.setCursor(cursor)

	override def closeWithResult(result: A): Unit = { dialog.dispose(); super.closeWithResult(result) }

	override def baseToScreen(pos: (Float, Float)): (Int, Int) = panel.baseToScreen(pos)
	override def screenToBase(pos: (Int, Int)): (Float, Float) = panel.screenToBase(pos)
}

