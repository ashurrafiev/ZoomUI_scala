package com.xrbpowered.scala.zoomui.swing

import java.awt.Cursor
import java.awt.event.{WindowAdapter, WindowEvent}

import com.xrbpowered.scala.zoomui.UIWindow
import javax.swing.{JFrame, WindowConstants}

class SwingFrame(factory: SwingWindowFactory, title: String, w: Int, h: Int, canResize: Boolean, undecorated: Boolean) extends UIWindow(factory) {

	val frame: JFrame = new JFrame
	frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
	frame.setTitle(title)
	frame.setResizable(canResize && !undecorated)
	frame.setUndecorated(undecorated)
	frame.addWindowListener(new WindowAdapter {
		override def windowClosing(e: WindowEvent): Unit = requestClosing()
	})

	val panel = new BasePanel(this)
	frame.setContentPane(panel)
	if(w>0 && h>0) clientSize = (w, h)
	center()

	override def clientWidth: Int = panel.getWidth
	override def clientHeight: Int = panel.getHeight
	override def clientSize_=(size: (Int, Int)): Unit = panel.resize(size)
	override def center(): Unit = frame.setLocationRelativeTo(null)
	override def show(): Unit = frame.setVisible(true)
	override def repaint(): Unit = panel.repaint()
	override def setCursor(cursor: Cursor): Unit = panel.setCursor(cursor)

	override def close(): Unit = { frame.dispose(); super.close(); System.exit(0) }
}
