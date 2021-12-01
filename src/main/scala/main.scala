package crunch

import java.time.format.DateTimeFormatter
import java.time.LocalDate

import scala.annotation.tailrec
import scala.io.Source.fromFile
import scala.util.Try

object CurrencyMappedStatement:

  /** 
   * FIXME - Start off with just one EUR account and one GBP account
   * 
   * inputs 
   * - fx rates from ECB
   * - GBP starling account
   *   EUR starling account
   */

  case class DateRank (at: LocalDate, rank: Int)

  sealed trait Entry:
    def date: LocalDate

  case class BaseEntry (
    date: LocalDate, 
    counter: String, 
    reference: String, 
    kind: String, 
    amount: Double, 
    balance: Double) extends Entry

  case class TransferEntry (
    orderedAt: DateRank, 
    counter: String, 
    reference: String, 
    amount: Double, 
    balance: Double) extends Entry:
    def date: LocalDate =
      orderedAt.at


  case class Fx (
    at: LocalDate,
    fixedAt: LocalDate,
    eur2gbp: Double
  )

  def line2Entry (line: String): BaseEntry = 
    val Array (date, counter, ref, kind, amount, balance, _) = 
      line.split (",").map (_.trim)
    val formatter = DateTimeFormatter.ofPattern("dd/MM/uuuu")
    
    BaseEntry (LocalDate.parse (date, formatter), counter, ref, kind, amount.toDouble, balance.toDouble)
      

  def line2Fx (line: String): Fx = 
    val Array (at, fixedAt, eur2gbp) = 
      line.split (",").map (_.trim)
    Fx (LocalDate.parse (at), LocalDate.parse (fixedAt), eur2gbp.toDouble)

  def eurHeader: String = 
    "Date,Counter Party,Reference,Type,Amount (EUR),Balance (EUR),Spending Category,Notes"

  def gbpHeader: String = 
    "Date,Counter Party,Reference,Type,Amount (GBP),Balance (GBP),Spending Category,Notes"
  
  def readEntries (path: String): Try [IndexedSeq [Entry]] =
    @tailrec
    def impl (curr: BaseEntry, rank: Int, input: Iterator [String], output: IndexedSeq [Entry]): IndexedSeq [Entry] =
      if input.hasNext then
        val next = line2Entry (input.next)
        if curr.kind == "CURRENCY TRANSFER" then
          val dr = if curr.date == output.last.date then DateRank (curr.date, rank + 1) else DateRank (curr.date, 0)
          val transfer = TransferEntry (dr, curr.counter, curr.reference, curr.amount, curr.balance)
          
          impl (next, dr.rank, input, output :+ transfer)
        else
          impl (next, rank, input, output :+ curr)
      else
        output
    Try {
      val source = fromFile (path)
      val lines = source.getLines.drop (1)
      val be = line2Entry (lines.next)

      impl (be, 0, lines, IndexedSeq.empty [Entry])     
    }
  
  def readFx (path: String): Try [Iterator [Fx]] =
    Try {
      val source = fromFile (path)
      for
        line <- source.getLines.drop (1)
      yield
        line2Fx (line)
    }

  @main
  def run =
    runAccounts (
      "data/Reconciliation FX Account - FX EUR-GB.csv", 
      "data/StarlingStatement_2021-03-01_2021-11-20-eur.csv", 
      "data/StarlingStatement_2021-03-01_2021-11-20-gbp.csv")

  def runAccounts (fxPath: String, eurPath: String, gbpPath: String) =
    val maybeEurEntries = readEntries (eurPath)
    val maybeGbpEntries = readEntries (gbpPath)
    val maybeFxEntries = readFx (fxPath)

    for 
      fxEntries <- maybeFxEntries
      gbpEntries <- maybeGbpEntries
      eurEntries <- maybeEurEntries
    do
      val fxMap = fxEntries.map (x => (x.fixedAt -> x.eur2gbp)).toMap
      
      for 
        eurEntry <- eurEntries
      do
        eurEntry match 
          case te @ TransferEntry (DateRank (dt, rnk), counter, ref, amt, bal) => 
            ???
          case ent @ BaseEntry (dt, counter, ref, kind, amt, bal) =>
            ???
        println (s"DEBUG--${gbpEntries.head}")
      
