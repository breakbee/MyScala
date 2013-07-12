package prml

import java.util.Arrays
import scala.Array.canBuildFrom

/**
 * Arrow Vector Class.
 */
class Vector(value : Array[Double]){
	
	private val elements = value
	
	private def this(vector : Vector) = this(vector.elements)
	
	def +(that: Vector) : Vector = {
		new Vector(for((a, b) <- elements zip that.elements) yield a + b)
	}
	
	def +(d: Double) : Vector = {
		new Vector(for(e <- elements) yield e + d)
	}
	
	def unary_- : Vector = {
		new Vector(for(e <- elements) yield -e)
	} 

	def -(that: Vector) : Vector = {
		new Vector(for((a, b) <- elements zip that.elements) yield a-b)
	}
	
	def *(that: Vector) : Double = {
		(for((a, b) <- elements zip that.elements) yield a*b).sum
	}
	
	def *(d: Double) : Vector = {
		new Vector(for(e <- elements) yield e*d)
	}
	
	def *:(d: Double) : Vector = {
		this * d
	}
	
	def transpose() : Matrix = {
		new Matrix(Array(elements))
	}
	
	def getData() : Array[Double] = {
		elements
	}
	
	override def toString = {
		"Vector : " + Arrays.toString(elements)
	}
}