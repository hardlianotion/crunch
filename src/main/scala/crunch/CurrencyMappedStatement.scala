package crunch

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDate}
import java.io.{BufferedWriter, File, FileWriter}
import java.time.temporal.{ChronoField, ChronoUnit}
import scala.annotation.tailrec
import scala.io.Source.fromFile
import scala.collection.SortedMap
import scala.util.{Try, Using}
import zio.json.*


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
    amount: Double) extends Entry:
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

  def starlingLine2Entry (line: String): BaseEntry =
    val Array (date, counter, ref, kind, amount, balance, _) =
      line.split (",").map (_.trim)
    val formatter = DateTimeFormatter.ofPattern ("dd/MM/uuuu")
    
    BaseEntry (LocalDate.parse (date, formatter), counter, ref, kind, amount.toDouble, balance.toDouble)
//"TransferWise ID",Date,Amount,Currency,Description,"Payment Reference","Running Balance","Exchange From","Exchange To","Exchange Rate","Payer Name","Payee Name","Payee Account Number",Merchant,"Card Last Four Digits","Card Holder Full Name",Attachment,Note,"Total fees"
  def wiseLine2Entry (line: String): BaseEntry =
    val Array (id, date, amount, curr, desc, ref, balance, from, to, rate, payer, payee,_, _, _, _, _, _,fees) =
      line.split (",").map (_.trim)
    val formatter = DateTimeFormatter.ofPattern ("dd-MM-uuuu")

    BaseEntry (LocalDate.parse (date, formatter), to, ref, desc, amount.toDouble, balance.toDouble)

  def line2Fx (line: String): FxEntry =
    val Array (at, eur2gbp) =
      line.split (",").map (_.trim)
    FxEntry (LocalDate.parse (at), eur2gbp.toDouble)

  def eurHeader: String = 
    "Date,Counter Party,Reference,Type,Amount (EUR),Balance (EUR),Spending Category,Notes"

  def gbpHeader: String = 
    "Date,Counter Party,Reference,Type,Amount (GBP),Balance (GBP),Spending Category,Notes"
  
  def readEntries (line2Entry: String => BaseEntry) (reconcileWith: String, path: String): Try [IndexedSeq [Entry]] =
    @tailrec
    def impl (input: Iterator [String], output: IndexedSeq [Entry]): IndexedSeq [Entry] =
      if input.hasNext then
        val currInput = input.next
        val curr = line2Entry (currInput)

        if curr.kind == "CURRENCY TRANSFER" && curr.counter == reconcileWith then
          val idx =
            output.last match
              case TransferEntry (DateRank (curr.date, idx), _, _, _) => idx + 1
              case _ => 0

          val transfer = TransferEntry (DateRank (curr.date, idx), curr.counter, curr.reference, curr.amount)
          impl (input, output :+ transfer)
        else
          impl (input, output :+ curr)
      else
        output
    Try {
      val source = fromFile (path)
      val lines = source.getLines.drop (1)

      val input = impl (lines, IndexedSeq.empty [Entry])
      if input.head.date.isBefore (input.last.date) then
        input
      else
        input.reverse
    }

  def buildFxMap (entries: Iterator [Option [FxEntry]]): SortedMap [LocalDate, Double] =
    entries.foldLeft (List.empty [FxEntry]) { (agg, rhs) => rhs match
      case Some (entry) => entry :: agg
      case None => agg
    }.map (x => x.at.plusDays (1) -> x.eur2gbp).to [SortedMap [LocalDate, Double]] (SortedMap)

//  val file = File(s"data/service/service-time-gap-${ServiceTime.minServiceGap}-arr-${ServiceTime.arrivalSearchLimit}-pro-${ServiceTime.proximityLimit}-spd-${ServiceTime.loSpeedLimit}-dil-${ServiceTime.timeDilator}-ovr-${ServiceTime.acceptOverlap}.csv ")
//      val baselineOut = BufferedWriter (FileWriter (file))

  def transformEurEntry (eurEntry: Entry, fxMap: SortedMap [LocalDate, Double], gbpTransfers: Map [DateRank, TransferEntry]): BaseEntry =
    eurEntry match
      case te @ TransferEntry (dt, counter, ref, amt) =>
        val gbpTran = gbpTransfers (dt)
        val amount = -gbpTran.amount

        BaseEntry (dt.at, counter, ref, "TRANSFER CURRENCY", amount, 0.0)
      case ent @ BaseEntry (dt, counter, ref, kind, amt, bal) =>
        val fxTran = fxMap.get (dt).orElse (fxMap.maxBefore (dt).map ( x => x._2)).getOrElse (Double.NegativeInfinity)
        val amountTran = fxTran * amt

        BaseEntry (dt, counter, ref, kind, amountTran, 0.0)


  def writeEntry (entry: BaseEntry, writer: BufferedWriter): Unit =
    val amountStr = f"${entry.amount}%2.2f"
    val balStr = f"${entry.balance}%2.2f"
    val dateStr = entry.date.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
    writer.write (s"$dateStr,${entry.counter},${entry.reference},${entry.kind},$amountStr,$balStr\n")

  def runAccounts (
    line2Entry: String => BaseEntry, acctName: String, gbpBalance: Double
  ) (
    reconcileWith: String,
    fxPath: String,
    eurPath: String,
    gbpPath: String,
    outDir: String
  ): Unit =
    val maybeEurEntries = readEntries (line2Entry) (reconcileWith, eurPath)
    val maybeGbpEntries = readEntries (line2Entry) (reconcileWith, gbpPath)
    val maybeFxEntries = FxReader.fromXml (fxPath)
    val file = File (s"$outDir/$acctName-eur2gbpout.csv")
    val output = BufferedWriter (FileWriter (file))

    for
      fxEntries <- maybeFxEntries
      gbpEntries <- maybeGbpEntries
      eurEntries <- maybeEurEntries
    do
      val fxMap = buildFxMap (fxEntries)
      val gbpTransfers = 
        gbpEntries
          .foldLeft (Map.empty [DateRank, TransferEntry]) {
            case (agg, trans @ TransferEntry (dr, counter, ref, amt)) =>
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
      // transform entries into GBP
      // aggregate the balance
      // write to file.
      println (s"There are ${eurEntries.size} EUR entries and ${gbpEntries.size} GBP entries.")
      eurEntries
        .map (e => transformEurEntry (e, fxMap, gbpTransfers))
        .foldLeft (IndexedSeq.empty [BaseEntry]) { (agg, rhs) =>
          if agg.isEmpty then
            val balance = f"${rhs.amount + gbpBalance}%2.2f".toDouble
            IndexedSeq (BaseEntry (rhs.date, rhs.counter, rhs.reference, rhs.kind, rhs.amount, balance))
          else
            val amount = f"${rhs.amount}%2.2f".toDouble
            agg :+ BaseEntry (rhs.date, rhs.counter, rhs.reference, rhs.kind, rhs.amount, agg.last.balance + amount)
        }
        .foreach (e => writeEntry (e, output))
    output.close ()

  case class NamedBalance (name: String, balance: Double)

  implicit val matchDecoder: JsonDecoder [NamedBalance] =
  DeriveJsonDecoder.gen [NamedBalance]

  @main
  def run (): Unit =

    //get the starting balances from config ...

    val maybeBalances = Using (io.Source.fromFile ("data/opening_balances.json")) { _.mkString }.toEither
                          .flatMap { _.fromJson [Array [NamedBalance]] }

    val balances =
      for
        balances <- maybeBalances
      do
        val balanceMap = balances.foldLeft (Map.empty [String, Double]) { (agg, item) => agg + (item.name -> item.balance) }
        val fxEntries = FxReader.fromXml ("data/fx-eur-gbp-2023-03-15.xml")
        fxEntries.map {entries => FxReader.toCsv ("out/fx-eur-gbp-2023-03-15.csv", entries)}
        runAccounts (starlingLine2Entry, "starling", balanceMap ("starling")) (
          "Ergates Limited",
          "data/fx-eur-gbp-2023-03-15.xml",
          "data/StarlingStatement_2021-12-01_2022-11-30-eur.csv",
          "data/StarlingStatement_2021-12-01_2022-11-30-gbp.csv", "out")
        runAccounts (wiseLine2Entry, "wise", balanceMap ("wise")) (
          "Ergates Limited",
          "data/fx-eur-gbp-2023-03-15.xml",
          "data/statement_20178858_EUR_2021-12-01_2022-11-30.csv",
          "data/statement_20203273_GBP_2021-12-01_2022-11-30.csv",
          "out"
        )

