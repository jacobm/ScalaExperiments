import cats.data.Coproduct
import cats.free._
import cats.{Id, ~>}

import scala.collection.mutable.ListBuffer

object Combination {
  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  sealed trait DataOp[A]
  case class AddCat(a: String) extends DataOp[Unit]
  case class GetAllCats() extends DataOp[List[String]]

  sealed trait UserOps[A]
  case class InsertUser(name: String) extends UserOps[Unit]
  case class GetUser(name: String) extends UserOps[Option[String]]

  type Co1[A] = Coproduct[DataOp, Interact, A]

  type FullApp[A] = Coproduct[UserOps, Co1, A]

  object InteractOps {
    def ask(prompt: String): Free[Interact, String] =
      Free.liftF[Interact, String](Ask(prompt))

    def tell(msg: String): Free[Interact, Unit] =
      Free.liftF[Interact, Unit](Tell(msg))
  }

  object UserOps {
    def insertUser(name: String): Free[UserOps, Unit] =
      Free.liftF[UserOps, Unit](InsertUser(name))

    def getUser(name: String): Free[UserOps, Option[String]] =
      Free.liftF[UserOps, Option[String]](GetUser(name))
  }

  class Users[F[_]](implicit I: Inject[UserOps, F]) {
    def insertUser(name: String): Free[F, Unit] =
      Free.inject[UserOps, F](InsertUser(name))
    def getUser(name: String): Free[F, Option[String]] =
      Free.inject[UserOps, F](GetUser(name))
  }

  object Users {
    implicit def users[F[_]](implicit I: Inject[UserOps, F]): Users[F] =
      new Users[F]
  }

  class Interacts[F[_]](implicit I: Inject[Interact, F]) {
    def tell(msg: String): Free[F, Unit] = Free.inject[Interact, F](Tell(msg))
    def ask(prompt: String): Free[F, String] =
      Free.inject[Interact, F](Ask(prompt))
  }

  object Interacts {
    implicit def interacts[F[_]](
        implicit I: Inject[Interact, F]): Interacts[F] = new Interacts[F]
  }

  class DataSource[F[_]](implicit I: Inject[DataOp, F]) {
    def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
    def getAllCats: Free[F, List[String]] =
      Free.inject[DataOp, F](GetAllCats())
  }

  object DataSource {
    implicit def dataSource[F[_]](
        implicit I: Inject[DataOp, F]): DataSource[F] = new DataSource[F]
  }

  object ConsoleCatsInterpreter extends (Interact ~> Id) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) =>
        println(prompt)
        readLine()
      case Tell(msg) =>
        println(msg)
    }
  }

  object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {

    private[this] val memDataSet = new ListBuffer[String]

    def apply[A](fa: DataOp[A]) = fa match {
      case AddCat(a) => memDataSet.append(a); ()
      case GetAllCats() => memDataSet.toList
    }
  }

  object InMemoryUserInterpreter extends (UserOps ~> Id) {

    private[this] val memDataSet = new ListBuffer[String]

    def apply[A](fa: UserOps[A]) = fa match {
      case InsertUser(name) => memDataSet.append(name); ()
      case GetUser(name) => memDataSet.find(x => x == name)
    }
  }

  val dbAndInteractInterpreter: Co1 ~> Id = {
    InMemoryDatasourceInterpreter or ConsoleCatsInterpreter
  }

  val interpreterCombined: FullApp ~> Id = {
    //val fisk = InMemoryDatasourceInterpreter or ConsoleCatsInterpreter // <- wont work
    InMemoryUserInterpreter or dbAndInteractInterpreter
  }
}

object FreeMonads extends App {
  import Combination.InteractOps._
  import Combination.{Ask, Interact, Tell}

  val program = for {
    cat <- ask("What's the kitty's name?")
    _ <- if (cat == "dingo") {
      tell("Well hello dingo")
    } else {
      tell(s"You said $cat")
    }
  } yield ()

  def interpreter: Interact ~> Id = new (Interact ~> Id) {
    def apply[A](fa: Interact[A]): Id[A] = fa match {
      case Ask(prompt) => println(prompt); readLine
      case Tell(msg) => println(msg)
    }
  }
  //val evaluated = program foldMap interpreter

  import Combination._

  def programCombined(implicit I: Interacts[FullApp],
                      D: DataSource[FullApp],
                      U: Users[FullApp]): Free[FullApp, Unit] = {

    import I._, D._, U._

    for {
      _ <- insertUser("hest")
      cat <- ask("What's the kitty's name?")
      _ <- addCat("dingo")
      _ <- addCat("fisk")
      _ <- addCat(cat)
      cats <- getAllCats
      _ <- tell(cats.toString)
      user <- getUser("hest")
      _ <- tell(user.get)
    } yield ()
  }

  val evaled: Unit = programCombined.foldMap(interpreterCombined)

  println("dingo")
}
