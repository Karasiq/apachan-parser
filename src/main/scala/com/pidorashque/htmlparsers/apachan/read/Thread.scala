package com.pidorashque.htmlparsers.apachan.read

import com.gargoylesoftware.htmlunit.html._
import com.pidorashque.networkutils.HtmlUnitUtils._

case class Thread(pages: Set[ThreadPage]) {
  assert(pages.nonEmpty, "Cannot create empty thread")

  val originalPost: Option[Post] = pages.head.posts.headOption

  def pagesCount = pages.head.pagesCount

  lazy val posts: IndexedSeq[Post] = pages.flatMap(_.posts).toIndexedSeq.sortBy(_.id)

  lazy val pagination: Map[Int, HtmlAnchor] = pages.flatMap(_.pagination).toMap

  /**
   * Gets first page
   * @return First page of the thread
   */
  def firstPage: ThreadPage =
    page(1).getOrElse(pages.head)

  /**
   * Load page with number `i`
   * @param i Page number
   * @return `i` page of the thread
   */
  def page(i: Int): Option[ThreadPage] = pages.find(_.currentPage.contains(i)).orElse {
    pagination.get(i)
      .flatMap(p => p.webClient.flatMap(_.htmlPageOption(p.fullHref)))
      .map(ThreadPage.apply)
  }

  def fromFirstPage = copy(pages = Set(firstPage))

  /**
   * Loads page `i` and return Thread copy with this page
   * @param i Page number
   * @return Thread with page `i`
   */
  def withPage(i: Int): Thread = {
    val p = page(i)
    if (p.nonEmpty) copy(pages = this.pages + p.get) else this
  }

  def withAllPages: Thread = copy(pages = {
    this.pages ++ pagination.view.flatMap(v => page(v._1))
  })

  def sequencedThread: Thread = copy(pages = {
    this.pages.foldLeft(1 -> IndexedSeq(firstPage))((seq, post) => {
      if (post.currentPage.contains(seq._1 + 1)) (seq._1 + 1, seq._2 ++ Some(post))
      else seq
    })._2.toSet
  })

  /**
   * Finds post that post `post` answered to or returns OP-post
   * @param post Post
   * @return `post` answered to post or [[originalPost]]
   */
  def answerTo(post: Post): Option[Post] =
    if (post.answerToId.nonEmpty) post.answerToId.flatMap(id => posts.find(_.id == id))
    else originalPost filter (_ != post)

  /**
   * Finds answers to post `post`
   * @param post Post
   * @return Answers
   */
  def answers(post: Post): Seq[Post] =
    posts.filter(p => answerTo(p).contains(post))
}

object Thread {
  /**
  * See [[ThreadPage.apply]]
  */
  def apply(htmlPage: HtmlPage): Thread = new Thread(Set(ThreadPage(htmlPage)))

  def apply(post: Post): Thread = apply(post.postHtmlPage)
}

object Section {
  /**
   * See [[ThreadPage.apply]]
   */
  def apply(htmlPage: HtmlPage): Thread = new Thread(Set(ThreadPage(htmlPage))) {
    override lazy val posts: IndexedSeq[Post] = {
      pages.toIndexedSeq.sortBy(_.currentPage).flatMap(_.posts)
    }

    /**
     * Threads not answering to other posts
     * @param th Post
     * @return [[None]]
     */
    override def answerTo(th: Post): Option[Post] = None

    /**
     * Loads answers to thread `th`
     * @param th Thread
     * @return Answers
     */
    override def answers(th: Post): Seq[Post] = {
      Thread(th).answers(th)
    }
  }
}
