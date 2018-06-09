
/**
  * Type class to perturbate a value.
  *
  * @tparam T type param
  */
trait Perturbator[T] {
  /**
    * Function perturbate a value by adding some error to it.
    *
    * @param value value to be perturbated
    * @return perturbated value
    */
  def perturbate(value: T): T
}

/**
  * Companion object.
  *
  * Refer `https://github.com/underscoreio/shapeless-guide` to understand the automatic derivation of type class which
  * is used below.
  */
object Perturbator {

  import shapeless.Generic
  import shapeless.{HList, HNil, ::}
  import shapeless.Lazy
  import PerturbatorImpl._

  private def create[A](func: A => A): Perturbator[A] = {
    new Perturbator[A] {
      def perturbate(value: A): A = func(value)
    }
  }

  def apply[A](implicit pert: Lazy[Perturbator[A]]): Perturbator[A] = pert.value


  implicit def genericPerturbator[A, R <: HList](
                                                  implicit
                                                  generic: Generic.Aux[A, R],
                                                  parser: Lazy[Perturbator[R]]
                                                ): Perturbator[A] = {
    create(arg => generic.from(parser.value.perturbate(generic.to(arg))))
  }

  implicit def hlistPerturbator[H, T <: HList](
                                                implicit
                                                hPerturbator: Lazy[Perturbator[H]],
                                                tPerturbator: Perturbator[T]
                                              ): Perturbator[H :: T] = {
    create(arg => hPerturbator.value.perturbate(arg.head) :: tPerturbator.perturbate(arg.tail))
  }

  implicit val intPerturbator: Perturbator[Int] = create(perturbateInt(_))

  implicit val stringPerturbator: Perturbator[String] = create(perturbateString(_))

  implicit val intVectorPerturbator: Perturbator[Vector[Int]] = create(perturbateVector[Int](_))

  implicit val stringVectorPerturbator: Perturbator[Vector[String]] = create(perturbateVector[String](_))

  implicit val hnilParser: Perturbator[HNil] = create(_ => HNil)

  /**
    * Implicit conversion to call perturbate directly on value like `value.perturbate`
    */
  implicit class PerturbatorOps[A: Perturbator](a: A) {
    def perturbate = Perturbator[A].perturbate(a)
  }


}
