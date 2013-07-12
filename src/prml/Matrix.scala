package prml


class Matrix(elements : Array[Array[Double]]){
	
	private val matrix = new Jama.Matrix(elements);
	
	private def this(matrix : Jama.Matrix) = this(matrix.getArray())
	
	def + (that: Matrix) : Matrix = {
		new Matrix(matrix.plus(that.matrix))
	}
	
	def + (d: Double) : Matrix = {
		val elements = 
			(for(row <- matrix.getArray()) yield 
				(for(elem <- row) yield elem + d).toArray).toArray
		new Matrix(elements)
	}
	
	def +: (d: Double) : Matrix = {
		this + d
	}
	
	def - (that: Matrix) : Matrix = {
		new Matrix(matrix.minus(that.matrix))
	}

	def * (that: Matrix) : Matrix = {
		new Matrix(matrix.times(that.matrix))
	}
	
	def * (v: Vector) : Vector = {
		new Vector((this * v.transpose.transpose).toSingleArray)
	}
	
	def *: (v : Vector) : Matrix = {
		v.transpose.transpose * this
	}
	
	def * (d: Double) : Matrix = {
		new Matrix(matrix.times(d))
	}
	
	def *: (d: Double) : Matrix = {
		this * d
	}
	
	def inv() : Matrix = {
		new Matrix(matrix.inverse())
	}
	
	def det() : Double = {
		matrix.det()
	}
	
	def trace() : Double = {
		matrix.trace()
	}
	
	def transpose() : Matrix = {
		new Matrix(matrix.transpose())
	}
	
	def getData() : Array[Array[Double]] = {
		matrix.getArray()
	}
	
	def toSingleArray() : Array[Double] = {
		matrix.getRowPackedCopy()
	}
	
	override def toString = {
		"Matrix : \n" + 
		(for(row <- matrix.getArray) 
			yield (for(e <- row) 
				yield e.toString).mkString(", ")).mkString("\n")
	}
}

object Matrix{
	def eye(dim: Int) : Matrix = {
		new Matrix(
			(for(i <- 0 until dim) yield 
				(for(j <- 0 until dim) yield 
					if(i==j) 1.0 else 0.0).toArray).toArray)
	}
	
	def zeros(dim: Int) : Matrix = {
		new Matrix(
			(for(i <- 0 until dim) yield 
				(for(j <- 0 until dim) yield 0.0).toArray).toArray)
	}
}