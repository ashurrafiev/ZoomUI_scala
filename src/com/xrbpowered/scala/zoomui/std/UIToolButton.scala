package com.xrbpowered.scala.zoomui.std

import java.awt.Color

import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer}
import com.xrbpowered.scala.zoomui.icons.{IconPalette, SvgIcon}

class UIToolButton(parent: UIContainer, val icon: SvgIcon, val iconSize: Int, padding: Int) extends UIButtonBase(parent) {
	import UIToolButton._
	def this(parent: UIContainer, iconUri: String, iconSize: Int, padding: Int) =
		this(parent, new SvgIcon(iconUri, UIToolButton.defaultIconSize, UIToolButton.palette), iconSize, padding)

	size = { val s = iconSize + padding * 2; (s, s) }

	override def paint(g: GraphAssist): Unit = {
		if(down)
			g.fill(this, colorDown)
		else if(hover) {
			g.fill(this, colorHover)
			g.border(this, colorBorder)
		}
		icon.paint(g.graph, if(disabled) STYLE_DISABLED else STYLE_NORMAL,
			(width - iconSize)/2f, (height - iconSize)/2f, iconSize, pixelScale, useCache = true)
	}
}
object UIToolButton {
	var colorDown: Color = UIButton.colorDown
	var colorHover: Color = new Color(0xe8e8e8)
	var colorBorder: Color = UIButton.colorBorder

	val STYLE_NORMAL = 0
	val STYLE_SELECTED = 1
	val STYLE_DISABLED = 2
	val STYLE_DISABLED_SELECTED = 3

	var palette: IconPalette = new IconPalette(Array(
		(new Color(0xeeeeee), new Color(0xf8f8f8), new Color(0x888888), Color.BLACK),
		(new Color(0x66aaff), new Color(0x4499ee), new Color(0xddeeff), Color.WHITE),
		(new Color(0xf9f9f9), new Color(0xfdfdfd), new Color(0xd8d8d8), new Color(0xababab)),
		(new Color(0x4298f3), new Color(0x2c8de8), new Color(0x90c4f3), new Color(0xa6d0f3))
	))

	var defaultIconSize = 160
	var iconPath = "com/xrbpowered/scala/zoomui/std/icons/"
}