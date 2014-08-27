package com.pidorashque.htmlparsers.apachan.write

import com.gargoylesoftware.htmlunit.html._
import com.pidorashque.htmlparsers.apachan.read.{Post, Thread}
import com.pidorashque.networkutils.HtmlUnitUtils._

abstract class CaptchaPage {
  def image: HtmlImage

  def variants: Set[String]

  def post: Option[Post]

  def submit(answer: String): Option[HtmlPage]

  override def toString: String = s"CaptchaPage($image, $variants)"
}

private[apachan] object CaptchaPage {
  def apply(htmlPage: HtmlPage): CaptchaPage = new CaptchaPage {
    private val radioInputs =
      htmlPage.byXPath[HtmlRadioButtonInput]("/html/body/form/input[@type='radio']")

    override val image: HtmlImage =
      htmlPage.byXPath[HtmlImage]("/html/body/form/img").head

    override def variants: Set[String] =
      radioInputs.map(_.getValueAttribute).toSet


    override def post: Option[Post] = {
      htmlPage.elementOption(_.getElementByName[HtmlHiddenInput]("comment")).flatMap(e => {
        val url = s"http://apachan.net/${e.getValueAttribute.toLong}.html"
        htmlPage.getWebClient.htmlPageOption(url).flatMap(page => Thread(page).originalPost)
      })
    }

    override def submit(answer: String): Option[HtmlPage] = {
      radioInputs.find(_.getValueAttribute.equalsIgnoreCase(answer)).map(_.click[HtmlPage]())
    }
  }
}