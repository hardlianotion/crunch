package crunch

import crunch.CurrencyMappedStatement.{FxEntry, line2Fx}

import java.time.LocalDate
import scala.io.Source.fromFile
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
