
import collection.GenTraversableOnce
import scalaz._
import Scalaz._

import ScalaTalk._
import util.Random




class BakeFailure(message: String)

object Pantry {
  case class NotInPantry(name: String) extends BakeFailure(name + " is missing")


  private val pantryStorage = Set("Pepper", "Margarine")

  def retrieve(name: String): ValidationNEL[BakeFailure,String] = {
    def checkForFreshness(name:String) = ""

    pantryStorage.contains(name) match {
      case true => name.successNel
      case false => NotInPantry(name).failNel
    }
  }


}

object BackOfThePantry {

  case class NoSubstitution(name: String) extends BakeFailure("There is no substitution for " + name)

  case class NotFresh(name:String) extends BakeFailure(name + " is not fresh")

  private val substitutions = Map("Butter" -> "Margarine")

  def checkForMold(name:String) = if(name != "Margarine") name.successNel else NotFresh("Margarine is moldy").failNel

  def checkSmell(name:String) = if(name != "Margarine") name.successNel else NotFresh("Margarine is smelly").failNel

  def substitute(name: String) : ValidationNEL[BakeFailure,String] = {
    substitutions.get(name) match {
      case Some(substitute) => checkForMold(substitute) and checkSmell(substitute)
      case None => NoSubstitution(name).failNel
    }
  }
}

object Store {

  case class TooExpensive(name: String, price: Double) extends BakeFailure(name + " is too expensive at " + price)
  case class NotAtStore(name: String) extends BakeFailure(name + " is not at the store")

  private val store = Map("Butter" -> 10.0, "Margarine" -> 5.0)

  def buy(name: String, mostWillPay: Double): ValidationNEL[BakeFailure,String] = {
    store.get(name) match {
      case Some(price) if (price <= mostWillPay) => name.successNel
      case None => NotAtStore(name).failNel
      case Some(price) if (price >= mostWillPay) => TooExpensive(name, price).failNel
    }
  }
}

object Mixer {

  case class DroppedPan(message:String) extends BakeFailure(message)

  val problemSimulator = new Random()
  def mix(ingredients: Seq[String]): ValidationNEL[BakeFailure,String] = {
    problemSimulator.nextInt() match {
      case i if i % 2 == 0 => DroppedPan("Oops I dropped the pan!").failNel
      case _ => (ingredients.mkString(",") + " are all mixed up").successNel
    }
  }
}




object Recipes1 extends App {
  println("Hello world")
}

object MakeBrownies extends App {
  (for {
    butter <- Pantry.retrieve("Butter") orElse Store.buy("Butter", 3) orElse BackOfThePantry.substitute("Butter")
    salt <- Pantry.retrieve("Salt") orElse Store.buy("Salt", 3) orElse BackOfThePantry.substitute("Salt")
    cocoa <- Pantry.retrieve("Cocoa") orElse Store.buy("Cocoa", 1) orElse BackOfThePantry.substitute("Cocoa")
    mixed <- Mixer.mix(Seq(butter, salt, cocoa))
  } yield butter) match {
    case Success(brownies) => println("Brownies made!")
    case Failure(failures) => {
      failures.foreach {
        case Mixer.DroppedPan(message) => println("That must have hurt")
        case failure => println(failure)
      }
    }
  }
}

object MakeBrowniesRefactored extends App {

  def ingredient(name:String, price:Int) = Pantry.retrieve(name) orElse Store.buy(name, price) orElse BackOfThePantry.substitute(name)


  (for {
    butter <- ingredient("Butter",3)
    salt <- ingredient("Salt",2)
    cocoa <- ingredient("Cocoa",1)
    mixed <- Mixer.mix(Seq(butter, salt, cocoa))
  } yield butter) match {
    case Success(brownies) => println("Brownies made!")
    case Failure(failures) => {
      failures.foreach {
        case Mixer.DroppedPan(message) => println("That must have hurt")
        case failure => println(failure)
      }
    }
  }



}

object MakeBrowniesWithApplicator extends App {
  def ingredient(name:String, price:Int) = Pantry.retrieve(name) orElse Store.buy(name, price) orElse BackOfThePantry.substitute(name)

  val hi = (ingredient("Butter",1) |@| ingredient("Salt",1)){_+_}

  println(hi)
}

object MakeBrowniesFunctionally extends App {
  def ingredient(name:String, price:Int) = Pantry.retrieve(name) orElse Store.buy(name, price) orElse BackOfThePantry.substitute(name)

  Seq("Butter" -> 10, "Cocoa" -> 3, "Salt" -> 1).map{ case (name,price) => ingredient(name,price)}.extrude.flatMap(Mixer.mix) match {
    case Success(result) => println(result)
    case Failure(fails) => fails.foreach(println)
  }
}

object MakeBrowniesByList extends App {
  //That was if we need to get one of the ingredients at a time
  //Start with a list of things, get them all at the same time, then deal with them
  val ingredients = Seq("Butter" -> 10, "Cocoa" -> 3, "Salt" -> 1)
  (for {
    results <- ingredients.map {
      case (ingredient, maxPrice) =>
            Pantry.retrieve(ingredient) orElse Store.buy(ingredient, maxPrice) orElse BackOfThePantry.substitute(ingredient)
           }.extrude
    mix <- Mixer.mix(results)
  } yield results) match {
    case Success(brownies) =>
    case Failure(failures) => {
      failures.foreach{
        failure => println(failure)
      }
    }
  }
}
