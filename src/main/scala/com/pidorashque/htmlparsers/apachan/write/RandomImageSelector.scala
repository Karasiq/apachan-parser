package com.pidorashque.htmlparsers.apachan.write

import com.gargoylesoftware.htmlunit.html._

import scala.collection.JavaConversions._

private[apachan] object RandomImageSelector {
   def apply(selector: HtmlSelect): RandomImageSelector = new RandomImageSelector {
     override val options: Seq[HtmlOption] = selector.getOptions.toIndexedSeq

     override def setSelectedOption(opt: HtmlOption): Unit = {
       selector.setSelectedAttribute(opt, true)
     }
   }
 }

private[apachan] abstract class RandomImageSelector {
   def options: Seq[HtmlOption]
   def setSelectedOption(opt: HtmlOption): Unit

   def selectBy(f: HtmlOption => Boolean): Unit = {
     setSelectedOption(options.find(f).get)
   }
   def selectByName(name: String): Unit = selectBy(_.getText.startsWith(name))
   def selectByIndex(i: Int): Unit = selectBy(_.getValueAttribute.toInt == i)
 }