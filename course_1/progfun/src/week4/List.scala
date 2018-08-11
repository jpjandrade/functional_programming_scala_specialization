package week4
import java.util.NoSuchElementException

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend [U >: T] (elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, Nil))
  def apply[T](x: T): List[T] = new Cons(x, Nil)
  def apply[T](): List[T] = Nil
}