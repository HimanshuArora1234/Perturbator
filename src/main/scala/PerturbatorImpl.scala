import scala.collection.immutable.NumericRange
import scala.util.Random

/**
  * Object containing implementations of perturbation of primitive types and vector.
  */
object PerturbatorImpl {

  private val alphabates: NumericRange.Inclusive[Char] = 'A' to 'Z'

  /**
    * Perturbates an integer value.
    *
    * @param value int value
    * @return perturbed int value
    */
  def perturbateInt(value: Int): Int = {
    val percentage: Int = Random.nextInt(11)
    randomAction match {
      case AddPerturbationAction => value + ((value.toDouble * percentage) / 100).toInt
      case RemovePerturbationAction => value - ((value.toDouble * percentage) / 100).toInt
    }
  }

  /**
    * Perturbates a string value.
    *
    * @param value string value
    * @return perturbed string value
    */
  def perturbateString(value: String): String = {
    val position: Int = Random.nextInt(value.length)

    randomAction match {
      case AddPerturbationAction =>
        val char: Char = alphabates(Random.nextInt(alphabates.size))
        s"${value.take(position)}$char${value.takeRight(value.size - position)}"

      case RemovePerturbationAction =>
        s"${value.take(position)}${value.takeRight(value.size - position - 1)}"
    }
  }

  /**
    * Perturbates a vector.
    *
    * @param value vector to be perturbed
    * @tparam T vector type
    * @return perturbed vector
    */
  def perturbateVector[T](value: Vector[T])(implicit pert: Perturbator[T]): Vector[T] = {
    val position: Int = Random.nextInt(value.length)

    randomAction match {
      case AddPerturbationAction =>
        val valToPerturbed: T = value(position)
        val newPosition: Int = Random.nextInt(value.length)
        (value.take(newPosition) :+ pert.perturbate(valToPerturbed)) ++ value.takeRight(value.length - newPosition)

      case RemovePerturbationAction =>
        value.take(position) ++ value.takeRight(value.length - position - 1)
    }
  }


  /**
    * Returns a random [[PerturbationAction]] implementation.
    *
    * @return [[PerturbationAction]] implementation
    */
  private def randomAction: PerturbationAction = Random.nextInt(2) match {
    case 0 => AddPerturbationAction
    case 1 => RemovePerturbationAction
  }


}


/**
  * Actions possible for perturbation.
  */
sealed trait PerturbationAction

case object AddPerturbationAction extends PerturbationAction

case object RemovePerturbationAction extends PerturbationAction
