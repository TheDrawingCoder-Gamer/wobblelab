package net.bulbyvr.wobblelab.util

trait PrettyPrint[T]:
  def prettyPrint(x: T): String
  
object PrettyPrint:
  def apply[T](using x: PrettyPrint[T]): PrettyPrint[T] = x
  
  
