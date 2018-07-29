package com.xrbpowered.scala.zoomui.std

import java.awt.{Color, Graphics2D}

import com.xrbpowered.scala.zoomui.{GraphAssist, UIContainer}
import com.xrbpowered.scala.zoomui.std.text.UITextBox

class UIListBox[T](parent: UIContainer, items: Seq[T]) extends UIScrollContainer(parent) {
	import UIListBox._

	protected var listItems: Seq[UIListItem[T]] = createItems(items)
	private var _selectedIndex: Option[Int] = None
	def selectedIndex: Option[Int] = _selectedIndex
	def selectedItem: Option[UIListItem[T]] =
		_selectedIndex.fold(None: Option[UIListItem[T]])( i => Some(listItems(i)))

	def deselect(): Unit = {
		_selectedIndex = None
		nothingSelected()
	}

	def select(index: Int): Unit =
		if(index>=0 && index<listItems.length) {
			_selectedIndex = Some(index)
			itemSelected(listItems(index))
		}

	protected def createItem(index: Int, item: T) = new UIListItem(this, index, item)

	private def createItems(items: Seq[T]): Seq[UIListItem[T]] =
		for ((item, i) <- items.zipWithIndex) yield createItem(i, item)

	def setItems(items: Seq[T]): Unit = {
		view.removeAllChildren()
		listItems = createItems(items)
		deselect()
	}

	var onItemSelected: (this.type, UIListItem[T]) => Unit = { (_, _) => () }
	var onNothingSelected: this.type => Unit = { _ => () }
	var onSelectionClicked: this.type => Unit = { _ => () }

	def itemSelected(item: UIListItem[T]): Unit = onItemSelected(this, item)
	def nothingSelected(): Unit = onNothingSelected(this)
	def selectionClicked(): Unit = onSelectionClicked(this)

	override def layoutView(): Float =
		listItems.foldLeft(0f) { (y, item) => {
			item.size = (width, UIListItem.itemHeight)
			item.location = (0, y)
			y + item.height
		} }

	override def paintSelf(g: GraphAssist): Unit =
		g.fill(this, colorBackground)
		
}
object UIListBox {
	var colorBackground: Color = UITextBox.colorBackground
}
