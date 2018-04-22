case class Aap(x: Int, y:Int)

val z = Function.tupled(Aap)

val x = (1,2)

println(z(x))
