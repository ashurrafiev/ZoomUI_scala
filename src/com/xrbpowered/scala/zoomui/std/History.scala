package com.xrbpowered.scala.zoomui.std

import java.util

abstract class History[T](val maxSize: Int) {
	private val history = new util.LinkedList[T]()
	private var _size = 0
	private var _index = -1
	
	protected def apply(item: T)
	def push(): Unit
	
	def canUndo: Boolean = _index > 0
	def undo(): Boolean = if(canUndo) { jumpTo(_index-1); true } else false
	def canRedo: Boolean = _index < _size-1
	def redo(): Boolean = if(canRedo) { jumpTo(_index+1); true } else false
	
	def jumpTo(index: Int) = {
		_index = index
		apply(history.get(_index))
		update()
	}
	
	def clear(): Unit = {
		history.clear()
		_size = 0
		_index = -1
	}
	
	protected def push(item: T) = {
		_index += 1
		while(_size>_index) {
			history.removeLast()
			_size -= 1
		}
		history.add(item)
		_size += 1;
		while(maxSize>0 && _size>maxSize) {
			history.removeFirst()
			_size -= 1
		}
		update()
	}
	
	def update(): Unit = {}
	
}