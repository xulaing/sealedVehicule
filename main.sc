sealed abstract class Vehicule[A]{}
case class Car[T]() extends Vehicule[T]{}
case class Motorbike[T]() extends Vehicule[T]{}
case class Boat[T]() extends Vehicule[T]{}
