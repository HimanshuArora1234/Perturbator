
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

  /**
    * Create Perturbator instance for given type.
    *
    * @param func perturbation function
    * @tparam A type
    * @return Perturbator[A
    */
  private def create[A](func: A => A): Perturbator[A] = {
    new Perturbator[A] {
      def perturbate(value: A): A = func(value)
    }
  }

  /**
    * The `apply` method causes the Scala compiler to search for an implicit `Perturbator[A]`
    *
    * @param pert `Perturbator[A]` in implicit scope
    * @tparam A type param
    * @return Perturbator[A]
    */
  def apply[A](implicit pert: Lazy[Perturbator[A]]): Perturbator[A] = pert.value


  /**
    * This causes the compiler to look for an implicit `Generic.Aux[A, R]` and `Perturbator[R]`
    *
    * @param generic to convert from a concrete type (e.g. a case class) to a generic
    * @param perturbator perturbator of Hlist
    * @tparam A concrete type
    * @tparam R Hlist
    * @return Perturbator[A]
    */
  implicit def genericPerturbator[A, R <: HList](
                                                  implicit
                                                  generic: Generic.Aux[A, R],
                                                  perturbator: Lazy[Perturbator[R]]
                                                ): Perturbator[A] = {
    create(arg => generic.from(perturbator.value.perturbate(generic.to(arg))))
  }

  /**
    * This causes the compiler to find the Perturbator of `HList`. Implicit Perturbator[H] is satisfied by the
    * implicit values that handle primitive types defined below and the Perturbator[T] is handled recursively
    * until the terminal HNil case is reached.
    *
    * @param hPerturbator perturbator for head of HList
    * @param tPerturbator perturbator for tail of HList
    * @tparam H head of HList
    * @tparam T tail of HList
    * @return Perturbator[H :: T]
    */
  implicit def hlistPerturbator[H, T <: HList](
                                                implicit
                                                hPerturbator: Lazy[Perturbator[H]],
                                                tPerturbator: Perturbator[T]
                                              ): Perturbator[H :: T] = {
    create(arg => hPerturbator.value.perturbate(arg.head) :: tPerturbator.perturbate(arg.tail))
  }


  // Instances for Perturbator type class

  implicit val intPerturbator: Perturbator[Int] = create(perturbateInt(_))

  implicit val stringPerturbator: Perturbator[String] = create(perturbateString(_))

  implicit def vectorPerturbator[A](implicit pert: Perturbator[A]): Perturbator[Vector[A]] = create(perturbateVector[A](_))

  implicit val nothingVectorPerturbator: Perturbator[Vector[Nothing]] = create(value => value)

  implicit val hnilPerturbator: Perturbator[HNil] = create(_ => HNil)

  /**
    * Implicit conversion to call perturbate directly on value like `value.perturbate`
    */
  implicit class PerturbatorOps[A: Perturbator](a: A) {
    def perturbate = Perturbator[A].perturbate(a)
  }

}


