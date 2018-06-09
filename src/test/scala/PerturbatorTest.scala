import org.scalatest.{FlatSpec, Matchers}

import Perturbator._

/**
  * Unit tests suite for [[Perturbator]].
  */
class PerturbatorTest extends FlatSpec with Matchers {

  "Perturbator" should "perturbate a integer" in {
    (1 to 10).foreach { value =>
      val perturbatedVal = value.perturbate
      perturbatedVal should (be >= (value - (value * 0.1)).toInt and be <= (value + (value * 0.1)).toInt)
    }
  }

  "Perturbator" should "perturbate a string" in {
    val values: Seq[String] = Seq("Scala", "is", "the", "best", "programming", "language")
    values.foreach { value =>
      value.perturbate shouldNot equal(value)
    }
  }

  "Perturbator" should "perturbate a string vector" in {
    val values: Vector[String] = Vector("Scala", "is", "the", "best", "programming", "language")
    values.perturbate shouldNot equal(value)
  }

  "Perturbator" should "perturbate a integer vector" in {
    val values: Vector[Int] = Vector(10, 20, 30, 40, 50)
    values.perturbate shouldNot equal(value)
  }

  "Perturbator" should "perturbate a class Person with string vector" in {
    val person = Person("himanshu", 20, Vector("abc", "xyz", "pqr"))
    val perturbatedPerson = person.perturbate
    perturbatedPerson.name shouldNot equal(person.name)
    perturbatedPerson.age should (be >= (person.age - (person.age * 0.1)).toInt and be <= (person.age + (person.age * 0.1)).toInt)
    perturbatedPerson.data shouldNot equal(person.data)
  }

  "Perturbator" should "perturbate a class Person with integer vector" in {
    val person = Person("himanshu", 20, Vector(10, 20, 30, 40, 50))
    val perturbatedPerson = person.perturbate
    perturbatedPerson.name shouldNot equal(person.name)
    perturbatedPerson.age should (be >= (person.age - (person.age * 0.1)).toInt and be <= (person.age + (person.age * 0.1)).toInt)
    perturbatedPerson.data shouldNot equal(person.data)
  }

}

case class Person[T](name: String, age: Int, data: Vector[T])