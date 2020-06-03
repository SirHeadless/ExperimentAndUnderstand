package com.sirheadless.freeMonade

import scala.concurrent.Future


/*
Problems with this implementations

1. **schwer testbar**:
  * Ausklammern des Datenbank-Backends ist nicht vorgesehen
  * Integrationsets sind moeglich
  * Unit tests sind schwer zu realisieren (hoher Aufwand)

2. **Seiteneffekte werden sofort ausgefuerht**
  * In der funktionalen Programmierung sollen Seiteneffekte moeglichst erst "am Ende des Universums" augefuehrt werden
  * Die Funktion ```removeBielefeld``` wuerde bei Mehrfachausfuehrung unterschiedliche
    Ergebnisse liefern

3. **Schnittstelle ist zu speziell:
  * Die Trennung von Beschreibung und Ausfuehrung ist ein Konzept, welches nicht erfuellt wird
    * Bei der Beschreibung des Codes sollsten wir uns nicht mit Future, Either und Throwable abmuehen muessen
    * Wir sollten unabhaengig von einer DB sein (in diesem Fall eine asynchrone welche Exceptions verwendet)
    *
 */
case class Address(id: Long, name: String, street: String, town: String, zip: String, company: String)

object AddressBookRepo {
  def put(address: Address): Future[Either[Throwable, Unit]] = ???

  def get(id: Long): Future[Either[Throwable, Option[Address]]] = ???

  def delete(address: Address): Future[Either[Throwable, Unit]] = ???

  def filter(foo: Address => Boolean): Future[Either[Throwable, List[Address]]] = ???

  import scala.concurrent.ExecutionContext.Implicits._

  def removeBielefeld: Future[Either[Throwable, List[Address]]] = {
    val bielefeldFE = filter(_.town == "Bielefeld")
    for {
      addressesE <- bielefeldFE
    } yield {
      addressesE.map(_.foreach(address => delete(address)))
      addressesE
    }
  }
}

/*
Um die Operationen als Entitäten zu formulieren, erstellen wir für jede Operation eine Datenrepräsentation.
Diese Repräsentation fassen wir als algebraischen Summentyp zusammen.

Der Typ A des Traits bestimmt dabei den Rueckgabewert.
 */

sealed trait AddressBookOp[A]

case class Put(address: Address) extends AddressBookOp[Unit]

case class Get(id: Long) extends AddressBookOp[Option[Address]]

case class Delete(address: Address) extends AddressBookOp[Unit]

case class Filter(foo: Address => Boolean) extends AddressBookOp[List[Address]]

/*
Damit lassen sich einfach Programme beschreiben, wie zB durch Listen von Befehle.
Es lassen sich jedoch nicht komplexe Zusammenhaenge formulieren. Es ist nicht moeglich gelesene Daten zu referenzieren
ohne einen Seiteneffekt auszuloesen, wie z.b in der Funktion removeBielefeld.
 */

import cats.free.Free
import cats.free.Free.liftF

object FreeAddressBookOps {

  // Typ-Alias für lifted AddressBookOps
  type AddressBookOpF[A] = Free[AddressBookOp, A]

  def put(address: Address): AddressBookOpF[Unit] =
    liftF[AddressBookOp, Unit](Put(address))

  def get(id: Long): AddressBookOpF[Option[Address]] =
    liftF[AddressBookOp, Option[Address]](Get(id))

  def delete(address: Address): AddressBookOpF[Unit] =
    liftF[AddressBookOp, Unit](Delete(address))

  def filter(foo: Address => Boolean): AddressBookOpF[List[Address]] =
    liftF[AddressBookOp, List[Address]](Filter(foo))
}


import cats.instances.list._
import cats.syntax.traverse._
import FreeAddressBookOps._


object FreeApp {

  // !?!?! removeBielefeld is a Monade again!?
  val removeBielefeld: AddressBookOpF[List[Address]] = for {
    addresses <- filter(_.town == "Bielefeld")
    _ <- addresses.traverse(delete(_))
  } yield addresses

//  def renameBielefeld: AddressBookOpF[Unit] =
//    for {
//      bielefelders <- removeBielefeld
//      _ <- bielefelders.traverse(address => put(address.copy(town = "not existant")))
//    } yield ()

}

//import cats.data.{State}
//import cats.~>
//
//object StateInterpreter {
//
//  type AddressBook = Map[Long, Address]
//  type AddressState[A] = State[AddressBook, A]
//
//  // !?!?! interpret is a functor transformation from `AddressBookOp` to `AddressState`
//  val interpret = new (AddressBookOp ~> AddressState) {
//    def apply[A](fa: AddressBookOp[A]) : AddressState[A] = fa match {
//      case Put(address) => State.modify[AddressBook]{_ + (address.id -> address)}
//      case Get(id) => State.inspect[AddressBook, Option[Address]]{_.get(id)}
//      case Delete(address) => State.modify[AddressBook]{_ - address.id}
//      case Filter(foo) => State.inspect{_.values.filter(foo).toList}
//    }
//  }
//}
//
//
//object Test extends App {
//
//  // !?!?! How does this work !?!?!
//  //
//  // Anwendung des State-Interpreters
//  val state: AddressState[List[Address]] = FreeApp.removeBielefeld.foldMap(StateInterpreter.interpret)
//  // Ausführung der State-Monade
//  val initialStorage = Map(1 -> Address(1, "Santa Clause", "Santa Street", "Bielefeld", "77777", "Santa Inc"))
//  val (storage, bielefelder) = state.run(initialStorage)
//}

