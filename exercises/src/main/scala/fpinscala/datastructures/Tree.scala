package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //Ex 3.25: count nr of nodes in a tree
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(a) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
      case _ =>  0
    }
  }

  def maximum(t: Tree[Int]): Int = {
      t match {
        case Leaf(a) => a
        case Branch(left, right) => maximum(left) max maximum(right)
      }
   }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }




}