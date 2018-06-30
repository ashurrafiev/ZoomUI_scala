package com.xrbpowered.scala.zoomui.std

import java.awt._
import java.net.URL

import com.xrbpowered.scala.zoomui.icons.SvgIcon
import com.xrbpowered.scala.zoomui.std.UIFormattedLabel.ZoomUIHtmlEditorKit
import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer, UIElement}
import javax.swing.JEditorPane
import javax.swing.text.DefaultStyledDocument.ElementSpec
import javax.swing.text._
import javax.swing.text.html.{CSS, HTML, HTMLDocument, HTMLEditorKit, ImageView, InlineView, StyleSheet}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class UIFormattedLabel(parent: UIContainer, init: String) extends UIContainer(parent) {

	val htmlKit = new ZoomUIHtmlEditorKit(this)
	setupHtmlKit()

	private val htmlAssist: JEditorPane = new JEditorPane
	htmlAssist.setOpaque(false)
	htmlAssist.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true)
	htmlAssist.setEditorKit(htmlKit)

	private var _html: String = init
	def html: String = _html
	def html_= (html: String): Unit = {
		_html = html
		htmlKit.rebuildUI = true
	}

	def setupHtmlKit(): Unit = { }

	override def invalidateLayout(): Unit = { }
	override def layout(): Unit = {
		htmlKit.rebuildUI = true
	}

	def drawFormattedString(g: GraphAssist, html: String, pixelScale: Float, x: Float, y: Float, w: Float): Float = {
		g.pushTx()
		g.clearTransform()
		g.translate(g.tx.getTranslateX, g.tx.getTranslateY)
		val scale = 1 / pixelScale
		if (htmlKit.rebuildUI || htmlKit.scale!=scale) {
			val font = htmlKit.defaultFont
			htmlAssist.setFont(font.deriveFont(font.getSize * scale))
			htmlAssist.setForeground(htmlKit.defaultColor)
			htmlAssist.setBounds(0, 0, (w * scale).toInt, 1)
			htmlAssist.invalidate()
			if (htmlKit.setScale(scale)) htmlAssist.setEditorKit(htmlKit)
			htmlAssist.setText(html)
		}
		if (htmlKit.rebuildUI) htmlKit.resetUI()
		htmlAssist.paint(g.graph)
		htmlKit.rebuildUI = false
		g.popTx()
		htmlAssist.getPreferredSize.getHeight.asInstanceOf[Float] * pixelScale
	}

	override def paintSelf(g: GraphAssist): Unit = {
		val h = drawFormattedString(g, html, pixelScale, x, y, width)
		size = (width, h)
	}

	var onHrefMouseIn: (this.type, String) => Unit = { (_, _) => () }
	var onHrefMouseOut: (this.type, String) => Unit = { (_, _) => () }
	var onHrefClicked: (this.type, String) => Unit = { (_, _) => () }
}
object UIFormattedLabel {
	def htmlString(str: String): String =
		"<html>" + str.replaceAll("\\&", "&amp;")
				.replaceAll("\\<", "&lt;")
				.replaceAll("\\>", "&gt;")

	class ZoomableCss(css: String) {
		import ZoomableCss._
		val baseCss = new StyleSheet
		baseCss.addRule(css)

		val zoomRules = new ArrayBuffer[ZoomRule]()

		def addZoomRule(selector: String, property: String, value: Float): Unit =
			zoomRules += new ZoomRule(selector, property, value)
		def addPtZoomRule(selector: String, property: String, value: Float): Unit =
			zoomRules += new PtZoomRule(selector, property, value)

		def makeZoomedStyle(scale: Float): StyleSheet = {
			val css = new StyleSheet
			css.addStyleSheet(baseCss)
			zoomRules.foreach(r => css.addRule(r.rule(scale)))
			css
		}
	}
	object ZoomableCss {
		class ZoomRule(selector: String, property: String, val value: Float) {
			val format = s"$selector {$property: %%d}"
			def value(scale: Float): Int = Math.round(value)
			def rule(scale: Float): String = format.format(value(scale))
		}

		class PtZoomRule(selector: String, property: String, value: Float) extends ZoomRule(selector, property, value) {
			override def value(scale: Float): Int = Math.round(GraphAssist.ptToPixels(value * scale))
		}
	}

	class ZoomUIHtmlEditorKit(val container: UIFormattedLabel) extends HTMLEditorKit {
		import ZoomUIHtmlEditorKit._

		var zoomableCss: Option[ZoomableCss] = None
		var defaultHoverColor: Option[Color] = None
		var defaultColor: Color = Color.BLACK
		var defaultFont: Font = UIButton.font

		val icons = new mutable.HashMap[String, SvgIcon]

		var rebuildUI = true
		private val hrefMap = new mutable.HashMap[String, UIElement]
		private var hoverHref: Option[String] = None
		private var _scale: Float = 0f
		def scale: Float = _scale
		def setScale (s: Float): Boolean =
			if (_scale!=s) {
				_scale = s
				rebuildUI = true
				zoomableCss.fold(false) {css => { setStyleSheet(css.makeZoomedStyle(_scale)); true } }
			}
			else false
			
		def resetUI(): Unit = {
			container.removeAllChildren()
			hrefMap.clear()
			rebuildUI = false
		}

		class SvgImageView(elem: Element) extends ImageView(elem) {
			val icon: SvgIcon = icons(elem.getAttributes.getAttribute(HTML.Attribute.SRC).asInstanceOf[String])
			val iconSize: Int = Integer.parseInt(elem.getAttributes.getAttribute(HTML.Attribute.SIZE).asInstanceOf[String])
			val dy: Int = elem.getAttributes.getAttribute("dy") match {
				case sdy: String => Integer.parseInt(sdy)
				case null => 0
			}

			override def getPreferredSpan(axis: Int): Float =
				if(axis==View.X_AXIS) iconSize * scale else 0

			override def paint(g: Graphics, a: Shape): Unit = {
				val rect = a.asInstanceOf[Rectangle]
				icon.paint(g.asInstanceOf[Graphics2D], 0, rect.x, rect.y, rect.y - (iconSize + dy) * scale, iconSize * scale, useCache = false)
			}
		}

		class HrefView(elem: Element) extends InlineView(elem) {
			val href: String = elem.getAttributes.getAttribute(HTML.Attribute.HREF).asInstanceOf[String]
			val hoverColor: Option[Color] = elem.getAttributes.getAttribute("hover") match {
				case hover: String => cssStringToColor(hover)
				case null => None
			}

			override def getForeground: Color =
				if (!hoverHref.contains(href)) super.getForeground
				else hoverColor.getOrElse(defaultHoverColor.getOrElse(super.getForeground))

			override def paint(g: Graphics, a: Shape): Unit = {
				val rect = a.asInstanceOf[Rectangle]
				if(!hrefMap.contains(href)) {
					val ui = new UIElement(container) {
						override def mouseIn(): Unit = repaint {
							hoverHref = Some(href)
							base.window.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
							container.onHrefMouseIn(container, href)
						}
						override def mouseOut(): Unit = repaint {
							hoverHref = None
							base.window.setCursor(Cursor.getDefaultCursor)
							container.onHrefMouseOut(container, href)
						}
						override def mouseDown(pos: (Float, Float), button: UIElement.Button, mods: Set[UIElement.Modifier]): Boolean =
							if(button==UIElement.Left && mods.none) {
								container.onHrefClicked(container, href)
								true
							}
							else false
						override def paint(g: GraphAssist): Unit = {}
					}
					hrefMap += ((href, ui))
					ui.location = (rect.x / scale, rect.y / scale)
					ui.size = (rect.width / scale, rect.height / scale)
				}
				super.paint(g, a)
			}
		}

		private class CustomHtmlDocument(styles: StyleSheet) extends HTMLDocument(styles) {
			override def getReader(pos: Int): HTMLEditorKit.ParserCallback = {
				getProperty(Document.StreamDescriptionProperty) match {
					case desc: URL => setBase(desc)
					case _ => ()
				}
				new HTMLReader(pos) {
					override def handleStartTag(t: HTML.Tag, a: MutableAttributeSet, pos: Int): Unit = {
						if (t.toString == "a") registerTag(t, new CharacterAction() {
							override def start(t: HTML.Tag, attr: MutableAttributeSet): Unit = {
								attr.addAttribute(StyleConstants.NameAttribute, t)
								val es = new DefaultStyledDocument.ElementSpec(attr.copyAttributes, ElementSpec.StartTagType)
								parseBuffer.addElement(es)
								super.start(t, attr)
							}
							override def end(t: HTML.Tag): Unit = {
								val es = new DefaultStyledDocument.ElementSpec(null, ElementSpec.EndTagType)
								parseBuffer.addElement(es)
								super.end(t)
							}
						})
						super.handleStartTag(t, a, pos)
					}
				}
			}
		}

		override def createDefaultDocument: Document = {
			val css = new StyleSheet
			css.addStyleSheet(getStyleSheet)
			val doc = new CustomHtmlDocument(css)
			doc.setParser(getParser)
			doc
		}

		override def getViewFactory: ViewFactory = new HTMLEditorKit.HTMLFactory() {
			override def create(elem: Element): View = {
				val attrs = elem.getAttributes
				val elementName = attrs.getAttribute(AbstractDocument.ElementNameAttribute)
				val o = if (elementName != null) null else attrs.getAttribute(StyleConstants.NameAttribute)
				o match {
					case kind: HTML.Tag if kind==HTML.Tag.IMG => new SvgImageView(elem)
					case kind: HTML.Tag if kind==HTML.Tag.A => new HrefView(elem)
					case _ => super.create(elem)
				}
			}
		}
	}
	object ZoomUIHtmlEditorKit {
		def cssStringToColor(s: String): Option[Color] = { // *facepalm*
			try {
				val m = classOf[CSS].getDeclaredMethod("stringToColor", classOf[String])
				m.setAccessible(true)
				Some(m.invoke(null, s).asInstanceOf[Color])
			} catch {
				case e: Exception => e.printStackTrace(); None
			}
		}
	}
}