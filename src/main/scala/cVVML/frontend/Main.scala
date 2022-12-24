package cVVML.frontend

import cVVML.lang.Syntax.Program
import caos.frontend.Site.initSite

/** Main function called by ScalaJS' compiled javascript when loading. */
object Main {
  def main(args: Array[String]):Unit =
    initSite[Program](CaosConfig)
}

