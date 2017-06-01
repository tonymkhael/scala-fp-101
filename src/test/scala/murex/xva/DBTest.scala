package murex.xva

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers}
import slick.driver.H2Driver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

// Definition of the SUPPLIERS table
class Suppliers(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {

  def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column

  def name = column[String]("SUP_NAME")

  def street = column[String]("STREET")

  def city = column[String]("CITY")

  def state = column[String]("STATE")

  def zip = column[String]("ZIP")

  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id, name, street, city, state, zip)
}


// Definition of the COFFEES table
class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {

  def name = column[String]("COF_NAME", O.PrimaryKey)

  def supID = column[Int]("SUP_ID")

  def price = column[Double]("PRICE")

  def sales = column[Int]("SALES")

  def total = column[Int]("TOTAL")

  def * = (name, supID, price, sales, total)

}

@RunWith(classOf[JUnitRunner])
class DBTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with Checkers {

  "DB" can "b used" in {
    val suppliers = TableQuery[Suppliers]
    val coffees = TableQuery[Coffees]

    val setup = DBIO.seq(
      // Create the tables, including primary and foreign keys
      (suppliers.schema ++ coffees.schema).create,

      // Insert some suppliers
      suppliers += (101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199"),
      suppliers += (49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460"),
      suppliers += (150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966"),
      // Equivalent SQL code:
      // insert into SUPPLIERS(SUP_ID, SUP_NAME, STREET, CITY, STATE, ZIP) values (?,?,?,?,?,?)

      // Insert some coffees (using JDBC's batch insert feature, if supported by the DB)
      coffees ++= Seq(
        ("Colombian", 101, 7.99, 0, 0),
        ("French_Roast", 49, 8.99, 0, 0),
        ("Espresso", 150, 9.99, 0, 0),
        ("Colombian_Decaf", 101, 8.99, 0, 0),
        ("French_Roast_Decaf", 49, 9.99, 0, 0)
      )
      // Equivalent SQL code:
      // insert into COFFEES(COF_NAME, SUP_ID, PRICE, SALES, TOTAL) values (?,?,?,?,?)
    )

    val db = Database.forConfig("h2mem1")
    try {
      val setupFuture = db.run(setup)
      Await.ready(setupFuture, 1 second)

      // Read all coffees and print them to the console
      println("All Coffees:")
      Await.ready(db.run(coffees.result).map(_.foreach {
        case (name, supID, price, sales, total) =>
          println("  " + name + "\t" + supID + "\t" + price + "\t" + sales + "\t" + total)
      }), 1 second)
      // Equivalent SQL code:
      // select COF_NAME, SUP_ID, PRICE, SALES, TOTAL from COFFEES

      // Why not let the database do the string conversion and concatenation?
      val q1 = for (c <- coffees)
        yield LiteralColumn("  ") ++ c.name ++ "\t" ++ c.supID.asColumnOf[String] ++
          "\t" ++ c.price.asColumnOf[String] ++ "\t" ++ c.sales.asColumnOf[String] ++
          "\t" ++ c.total.asColumnOf[String]
      // The first string constant needs to be lifted manually to a LiteralColumn
      // so that the proper ++ operator is found

      // Equivalent SQL code:
      // select '  ' || COF_NAME || '\t' || SUP_ID || '\t' || PRICE || '\t' SALES || '\t' TOTAL from COFFEES

      println("Stream")
      db.stream(q1.result).foreach(println)

      // Perform a join to retrieve coffee names and supplier names for
      // all coffees costing less than $9.00
      val q2 = for {
        c <- coffees if c.price < 9.0
        s <- suppliers if s.id === c.supID
      } yield (c.name, s.name)
      // Equivalent SQL code:
      // select c.COF_NAME, s.SUP_NAME from COFFEES c, SUPPLIERS s where c.PRICE < 9.0 and s.SUP_ID = c.SUP_ID

      println("Coffees costing less than 9, joined with suppliers")
      Await.ready(db.run(q2.result).map(_.foreach {
        case (coffee, supplier) =>
          println(s"coffee: $coffee supplied by $supplier")
      }), 1 second)

    } finally db.close

  }

}
