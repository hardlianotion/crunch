package crunch

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, Duration}
import java.io.{FileWriter, BufferedWriter, File}

import scala.annotation.tailrec
import scala.io.Source.fromFile
import scala.collection.SortedMap
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

  case class FxEntry (
    at: LocalDate,
    eur2gbp: Double
  )

  def line2Entry (line: String): BaseEntry = 
    val Array (date, counter, ref, kind, amount, balance, _) =
      line.split (",")//.map (_.trim)
    val formatter = DateTimeFormatter.ofPattern("dd/MM/uuuu")
    
    BaseEntry (LocalDate.parse (date, formatter), counter, ref, kind, amount.toDouble, balance.toDouble)
      

  def line2Fx (line: String): FxEntry =
    val Array (at, eur2gbp) =
      line.split (",").map (_.trim)
    FxEntry (LocalDate.parse (at), eur2gbp.toDouble)

  def eurHeader: String = 
    "Date,Counter Party,Reference,Type,Amount (EUR),Balance (EUR),Spending Category,Notes"

  def gbpHeader: String = 
    "Date,Counter Party,Reference,Type,Amount (GBP),Balance (GBP),Spending Category,Notes"
  
  def readEntries (reconcileWith: String, path: String): Try [IndexedSeq [Entry]] =
    @tailrec
    def impl (curr: BaseEntry, input: Iterator [String], output: IndexedSeq [Entry]): IndexedSeq [Entry] =
      if input.hasNext then
        val next = line2Entry (input.next)
        if curr.kind == "CURRENCY TRANSFER" && curr.counter == reconcileWith then
          val transfer = TransferEntry (DateRank (curr.date, 0), curr.counter, curr.reference, curr.amount, curr.balance)
          
          impl (next, input, output :+ transfer)
        else
          impl (next, input, output :+ curr)
      else
        output
    Try {
      val source = fromFile (path)
      val lines = source.getLines.drop (1)
      val be = line2Entry (lines.next)

      impl (be, lines, IndexedSeq.empty [Entry])     
    }
  
  def readFx (path: String): Try [Iterator [Option [FxEntry]]] =
    Try {
      val source = fromFile (path)
      for
        line <- source.getLines.drop (1)
      yield
        Try {line2Fx (line)}.toOption
    }

  def buildFxMap (entries: Iterator [Option [FxEntry]]): SortedMap [LocalDate, Double] =
    entries.foldLeft (List.empty [FxEntry]) { (agg, rhs) => rhs match
      case Some (entry) => entry :: agg
      case None => agg
    }.map (x => (x.at.plusDays (1) -> x.eur2gbp)).to [SortedMap [LocalDate, Double]] (SortedMap)

//  val file = File(s"data/service/service-time-gap-${ServiceTime.minServiceGap}-arr-${ServiceTime.arrivalSearchLimit}-pro-${ServiceTime.proximityLimit}-spd-${ServiceTime.loSpeedLimit}-dil-${ServiceTime.timeDilator}-ovr-${ServiceTime.acceptOverlap}.csv ")
//      val baselineOut = BufferedWriter (FileWriter (file))

  def writeEntry (entry: BaseEntry, writer: BufferedWriter): Unit = 
    val amountStr = f"${entry.amount}%2.2f"
    val balStr = f"${entry.balance}%2.2f"
    writer.write (s"${entry.date},${entry.counter},${entry.reference},${entry.kind},${amountStr},${balStr}\n")

  def runAccounts (reconcileWith: String, fxPath: String, eurPath: String, gbpPath: String) =
    val maybeEurEntries = readEntries (reconcileWith, eurPath)
    val maybeGbpEntries = readEntries (reconcileWith, gbpPath)
    val maybeFxEntries = readFx (fxPath)
    val file = File ("eur2gbpout.csv")
    val output = BufferedWriter (FileWriter (file))

    maybeGbpEntries.foreach (i => i.foreach (println))
    for 
      fxEntries <- maybeFxEntries
      gbpEntries <- maybeGbpEntries
      eurEntries <- maybeEurEntries
    do
      val fxMap = buildFxMap (fxEntries)
      val gbpTransfers = 
        gbpEntries
          .foldLeft (Map.empty [DateRank, TransferEntry]) { 
            case (agg, trans @ TransferEntry (dr, counter, ref, amt, bal)) =>
              @tailrec
              def impl (dr: DateRank): DateRank = 
                if agg.contains (dr) then
                  impl (DateRank (dr.at, dr.rank + 1))
                else
                  dr
              agg + (impl (dr) -> trans)
            case (agg, base: BaseEntry) => 
              agg
          }

      for 
        eurEntry <- eurEntries
      do
        eurEntry match 
          case te @ TransferEntry (dt, counter, ref, amt, bal) =>
            val gbpTran = gbpTransfers (dt)
            val amount = -gbpTran.amount
            writeEntry (BaseEntry (dt.at, counter, ref, "TRANSFER CURRENCY", amount, bal + amount), output)           
          case ent @ BaseEntry (dt, counter, ref, kind, amt, bal) =>
            val fxTran = fxMap.get (dt).orElse (fxMap.maxBefore (dt).map ( x => x._2)).getOrElse (Double.NegativeInfinity)
            val amountTran = fxTran * amt
            val balTran = fxTran * bal
            writeEntry (BaseEntry (dt, counter, ref, kind, amountTran, balTran), output)
    output.close
  @main
  def run =
    runAccounts (
      "Ergates Limited",
      "data/fx-eurgbp.csv",
      "data/StarlingStatement_2021-01-15_2021-12-29-eur.csv",
      "data/StarlingStatement_2021-01-01_2021-12-29-gbp.csv")

