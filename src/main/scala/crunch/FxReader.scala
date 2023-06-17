package crunch

import crunch.CurrencyMappedStatement.{FxEntry, line2Fx}

import java.time.LocalDate
import scala.io.Source.fromFile
import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Try
import scala.xml

object FxReader:

  def fromCsv (path: String): Try [Iterator [Option [FxEntry]]] =
    Try {
      val source = fromFile (path)
      for
        line <- source.getLines.drop (1)
      yield
        Try {line2Fx (line)}.toOption
    }

  def fromXml (path: String): Try [Iterator [Option [FxEntry]]] =
    val result = Try {xml.XML.loadFile (path)}
    result.map { xml =>
      (xml \\ "Obs")
        .iterator
        .map (x => (x \ "@TIME_PERIOD").text -> (x \ "@OBS_VALUE").text)
        .map ((at, fx) => Try {FxEntry (LocalDate.parse (at), fx.toDouble)}.toOption)
    }

  def toCsv (path: String, entries: Iterator [Option [FxEntry]]): Unit =
    val file = File (path)
    val writer = BufferedWriter (FileWriter(file), 7)
    writer.write("date,eurgbp,gbpeur\n")

    for
      maybeEntry <- entries
    do
      for
        entry <- maybeEntry
      do
        writer.write (s"${entry.at},${entry.eur2gbp},${1.0 / entry.eur2gbp}\n")
    writer.close()
