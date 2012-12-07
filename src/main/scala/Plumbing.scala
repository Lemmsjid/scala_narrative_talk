import scalaz.Scalaz._
import scalaz._
import scalaz.Success

/*             )\._.,--....,'``.
.b--.        /;   _.. \   _\  (`._ ,.
`=,-,-'~~~   `----(,_..'--(,_..'`-.;.'  */

package object ScalaTalk extends Semigroups {
  implicit def toValidationDecorater[A, B](v: Validation[A, B]) = new ValidationDecorater(v)

  implicit def makeExtruder[A, B](validations: Iterable[Validation[NonEmptyList[A], B]]): Extruder[A, B] = new Extruder[A, B](validations)

}



/**
 * There's some gnarly syntax to sequence a list of validations.  Fortunately that can be abstracted away
 * using this class.  This class is intended to be the result of an implicit def imported via grvz._
 * @param validations
 * @tparam B This should be implied, so that you only have to call extrude
 */
class Extruder[A, B](validations: Iterable[ValidationNEL[A, B]]) {

  /**
   * This is a silly name for what is essentially a wrapper around the sequence method.  The reason for
   * the existence of this wrapper is that the syntax you see below is an eyesore.
   *
   * This will take a list of ValidationNEL instances and make it into a single validation with a list
   * of successes or failures.
   * @return
   */
  def extrude: ValidationNEL[A, Seq[B]] = {
    validations.toSeq.sequence[({type l[a] = ValidationNEL[A, a]})#l, B]
  }

}


class ValidationDecorater[A, B](v: Validation[A, B]) {

  /**
   * Allows two validators to be run simultaneously (fail-slow), with the success of the second validator
   * @param other
   * @tparam AA
   * @return
   */
  def and[AA >: A : Semigroup](other: => Validation[AA,B]) : Validation[AA,B]  = v match {
    case Success(_) => other
    case Failure(as1) => other match {
      case Success(_) => other
      case Failure(as2) => Failure((as1: AA) |+| as2)
    }
  }

  /**
   * Short-circuits the first Success of this and another Validation, or else appends their Failures.
   *
   * @todo Scalaz 7 extends Validations a bit and may subsume this method.
   * @todo Scalaz 7 favors readable aliases (yay!). In honor of this, rename this to firstSuccessOrAppendFailures or something.
   * @see http://stackoverflow.com/questions/9195837/scalaz-validation-aggregate-errors-or-return-any-success
   */
  def orElse[AA >: A : Semigroup](fallback: => Validation[AA, B]): Validation[AA, B] = v match {
    case Success(_) => v
    case Failure(as1) => fallback match {
      case Success(_) => fallback
      case Failure(as2) => Failure((as1: AA) |+| as2)
    }
  }
}
