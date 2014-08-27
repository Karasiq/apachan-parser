package com.pidorashque.htmlparsers.apachan.write

import com.gargoylesoftware.htmlunit.html._

private[apachan] class PostForm(page: HtmlPage) {
  def sageCheckbox: HtmlCheckBoxInput =
    page.getElementByName[HtmlCheckBoxInput]("no_up")

  def titleField: HtmlTextInput =
    page.getHtmlElementById[HtmlTextInput]("title1")

  def textField: HtmlTextArea =
    page.getHtmlElementById[HtmlTextArea]("datafield")

  def urlField: HtmlTextInput =
    page.getElementByName[HtmlTextInput]("img_url")

  def fileField: HtmlFileInput =
    page.getElementByName[HtmlFileInput]("userfile")

  def randomSelector: RandomImageSelector =
    RandomImageSelector(page.getElementByName[HtmlSelect]("ins_random"))

  def postButton: HtmlSubmitInput =
    page.getElementByName[HtmlSubmitInput]("SubmitButton")
}

object PostImage {
  private[apachan] class PagePostApply(f: PostForm => Unit) {
    def apply(frm: PostForm) = f(frm)
  }

  private[apachan] val clearImages = new PagePostApply(f => {
    f.urlField.setText("")
    f.fileField.setData(null)
  })

  val NO_IMAGE = new PagePostApply(_ => ())

  def fromUrl(url: String) = new PagePostApply(_.urlField.setText(url))

  def fromBytes(data: Array[Byte]) = new PagePostApply(_.fileField.setData(data))

  // def fromRandom(random: HtmlOption) = new PagePostApply(_.randomSelector.setSelectedOption(random))

  def fromRandom(random: String) = new PagePostApply(_.randomSelector.selectByName(random))

  def fromRandom(random: Int) = new PagePostApply(_.randomSelector.selectByIndex(random))
}

abstract class PostSender {
  def sendPost(text: String, title: String = "", sage: Boolean = false, image: PostImage.PagePostApply = PostImage.NO_IMAGE): CaptchaPage
}

class PostSenderImpl(postForm: PostForm) extends PostSender {
  override def sendPost(text: String, title: String = "", sage: Boolean = false, image: PostImage.PagePostApply = PostImage.NO_IMAGE): CaptchaPage = CaptchaPage {
    postForm.titleField.setText(title)
    postForm.sageCheckbox.setChecked(sage)
    postForm.textField.setText(text)
    PostImage.clearImages(postForm)
    image(postForm)
    postForm.postButton.click[HtmlPage]()
  }
}

object PostSender {
  def apply(htmlPage: HtmlPage): PostSender = {
    assert(htmlPage.getWebClient.getOptions.isJavaScriptEnabled, "Cannot post without javascript")
    new PostSenderImpl(new PostForm(htmlPage))
  }
}
