package com.twitter.finagle.http

import scala.annotation.tailrec
import HeaderMap.Header

/**
 * Prefix trie for headers, where headers are stored on a lower-case prefix of
 * their name. This trie is used as the backing store of TrieHeaderMap
 */
final class HeaderTrie private(var header: Header, var path: List[Int], private var children: HeaderTrie.Children) {
  import HeaderTrie._

  def foreach[U](f: (String, String) => U): Unit = {
    var cur = header
    while(cur != null){
      f(cur.name, cur.value)
      cur = cur.next
    }
    if(children != null){
      children.focus.foreach(f)
      children.smaller.foreach(_.foreach(f))
      children.larger.foreach(_.foreach(f))
    }
  }

  def pretty(indent: Int): Unit = {
    print(" " * indent)
    print(showPath(path))
    if(header != null) print(s"  $header")
    println()
    if(children != null){
      for(child <- children.iterator) child.pretty(indent + path.length)
    }
  }

  @tailrec
  private def forNameOrNull(name: String, index: Int): Header = {
    if(name.length == index) header
    else if (children == null) null
    else {
      val head = hashChar(name(index))
      val child = children.findBranch(head).focus

      /* loop manually unrolled so that outer can be @tailrec
       * the loop is easier to understand as the below method
      @tailrec
      def rec(nextIndex: Int, branch: List[Int]): Header = {
        if(branch.isEmpty) child.forNameOrNull(name, nextIndex)
        else if (nextIndex >= name.length) null
        else if (branch.head == hashChar(name(nextIndex))) rec(nextIndex + 1, branch.tail)
        else null
      }
      rec(index, child.path)
      */
      var nextIndex = index
      var branch = child.path

      while(branch.nonEmpty && name.length < nextIndex && branch.head == hashChar(name(nextIndex))) {
        nextIndex += 1
        branch = branch.tail
      }

      if(branch.isEmpty) child.forNameOrNull(name, nextIndex)
      else null
      
    }
  }

  def leaves: Iterator[Header] = {
    def flatChildren = children.iterator.flatMap(_.leaves)
    if (children == null && header == null) Iterator.empty
    else if (header == null) flatChildren
    else if (children == null) Iterator.single(header)
    else flatChildren ++ Iterator.single(header)
  }

  def forNameOrNull(name: String): Header = forNameOrNull(name, 0)

  /**
   * Sets `toSet`, overwriting any known headers with the same name
   */
  def setHeader(toSet: Header): this.type = {
    updateHeader(0, toSet.name, toSet, true)
    this
  }
  /**
   * Adds header `toAdd`, adding this value to already present headers with the same name
   */
  def addHeader(toAdd: Header): this.type = {
    updateHeader(0, toAdd.name, toAdd, false)
    this
  }

  @tailrec
  private def updateHeader(index: Int, name: String, newHeader: Header, overwrite: Boolean): Unit = {
    if(index == name.length) {
      val replacement = if (header == null || overwrite) newHeader
                        else { header.add(newHeader); header }
      header = replacement
      ()
    }
    else {
      def mkChild = new HeaderTrie(newHeader, pathEndFor(name, index), null)
      if(children == null) {
        children = Children(Nil, mkChild, Nil)
        ()
      } else {
        val nextChar = hashChar(name(index))
        val focussed = children.findBranch(nextChar)
        val focus = focussed.focus
        //no child for this head, insert path directly
        //before larger or before smaller focus
        if(nextChar > focus.path.head) {
          children = Children(focus :: focussed.smaller, mkChild, focussed.larger)
          ()
        }
        else if(nextChar < focus.path.head) {
          children = Children(focussed.smaller, mkChild, focus :: focussed.larger)
          ()
        }
        else {
          //there is a branch with the correct starting character.
          //thre are three scenarios:
          //1. the path to the child is a prefix of the path to the new header.
          //in that case, advance the path index and recurse on the child
          //2. the path to the new header is a prefix of the path to the child.
          //in that case, create a new node along the path with this header
          //3. the paths diverge
          //in that case, create a new child node at the end of the common path
          //that has no header, that has two children of its own, the old one with
          //the shared prefix truncated, and a new one with the added header and its diverging path
          val (afterLastCommonIndex, remainingPath, commonPrefixTail) = stripCommonIndex(name, index + 1, focus.path.tail)
          if (remainingPath.isEmpty) {
            focus.updateHeader(afterLastCommonIndex, name, newHeader, overwrite)
          }
          else if(afterLastCommonIndex == name.length) {
            val grandchild = new HeaderTrie(focus.header, remainingPath, focus.children)
            val newChild = new HeaderTrie(newHeader, focus.path.head :: commonPrefixTail, Children(Nil, grandchild, Nil))
            children = Children(focussed.smaller, newChild, focussed.larger)
            ()
          }
          else {
            val oldGrandChild = new HeaderTrie(focus.header, remainingPath, focus.children)
            val pathEnd = pathEndFor(name, afterLastCommonIndex)
            val newGrandChild = new HeaderTrie(newHeader, pathEnd, null)
            val grandChildren = if (remainingPath.head < pathEnd.head)
                                   Children(List(oldGrandChild), newGrandChild, Nil)
                                else Children(Nil, newGrandChild, List(oldGrandChild))
            val focusChild = new HeaderTrie(null, focus.path.head :: commonPrefixTail, grandChildren)
            children = Children(focussed.smaller, focusChild, focussed.larger)
            ()
          }
        }
      }
    }
  }

  def clearName(name: String): this.type = {
    clearPath(pathFor(name))
    this
  }
  
  //todo: refactor into index-based for performance
  private def clearPath(path: List[Int]): this.type = path match {
    case Nil => {
      //leaf found
      header = null
      children match {
        case null => this
        case Children(Nil, c, Nil) => {
          //special case, "become" singlular child directly attached to parent
          header = c.header
          children = c.children
          this.path = this.path ::: c.path
          this
        }
        case _ => this
      }
    }
    case (head :: tail) => {
      if (children == null) {
        this
      }
      else {
        val children = this.children.findBranch(head)
        val child = children.focus
        if (child.path.head != head) {
          this
        }
        else {
          val (lookupRemainder, branchRemainder, traversedTail) = stripCommonPrefix(tail, child.path.tail)
          if (branchRemainder.nonEmpty) {
            this
          }
          else {
            child.clearPath(lookupRemainder)
            //cleanup. Not doing cleanup and wallowing in our filth may be faster?
            if(child.header == null && child.children == null) {
              if (children.smaller.nonEmpty){
                this.children = Children(children.smaller.tail, children.smaller.head, children.larger)
              }
              else if (children.larger.nonEmpty){
                this.children = Children(children.smaller, children.larger.head, children.larger.tail)
              }
              else {
                this.children = null
              }
            }
            this
          }
        }
      }
    }
  }
}

object HeaderTrie {
  //ad-hoc zipper. Linear search. Justified by low number of collisions. Three is going to be a stretch.
  //alternatives to possibly try out:
  //* specializations for cases 1, 2 and 3
  //** fall back to Vector and binary search for bigger.
  //** fall back to plain unordered List for bigger, and iterate fully (but cheap insert)
  //** fall back to plain sorted List (linear search that can stop once bigger, but expensive insert)
  final private case class Children(smaller: List[HeaderTrie], focus: HeaderTrie, larger: List[HeaderTrie]) {
    def unsafedown = Children(smaller.tail, smaller.head, focus :: larger)
    def unsafeup = Children(focus :: smaller, larger.head, larger.tail)
    /**
     * returns the closest branch to start
     */
    def findBranch(start: Int): Children = {
      if (start < focus.path.head) findBranchDown(start)
      else findBranchUp(start)
    }

    @tailrec
    private def findBranchDown(start: Int): Children = {
      if (start < focus.path.head && smaller.nonEmpty)
        Children(smaller.tail, smaller.head, focus :: larger).findBranchDown(start)
      else this
    }

    @tailrec
    private def findBranchUp(start: Int): Children = {
      if (start > focus.path.head && larger.nonEmpty)
        Children(focus :: smaller, larger.head, larger.tail).findBranchUp(start)
      else this
    }
    
    def iterator: Iterator[HeaderTrie] = smaller.iterator ++ Iterator(focus) ++ larger.iterator
  }

  def empty: HeaderTrie = new HeaderTrie(null, Nil, null)
  
  private def hashChar(c: Char): Int =
    if (c >= 'A' && c <= 'Z') c + 32
    else c

  private def pathEndFor(name: String, first: Int): List[Int] = {
    @tailrec
    def rec(agg: List[Int], i: Int): List[Int] = {
      if(i < first) agg
      else rec(hashChar(name(i)) :: agg, i - 1)
    }
    rec(Nil, name.length - 1)
  }

  private def pathFor(name: String): List[Int] = name.map(hashChar).toList

  private def showPath(path: List[Int]): String = path.map(_.toChar).mkString

  private def stripCommonPrefix[A](l: List[A], r: List[A]): (List[A], List[A], List[A]) = {
    val prefixBuilder = List.newBuilder[A]
    def rec(ll: List[A], rr: List[A]): (List[A], List[A], List[A]) = {
      (ll, rr) match {
        case (hl :: tl, hr :: tr) if (hl == hr) => {
          prefixBuilder += hl
          rec(tl, tr)
        }
        case _ => (ll, rr, prefixBuilder.result)
      }
    }
    rec(l, r)
  }

  private def stripCommonIndex(name: String, start: Int, path: List[Int]): (Int, List[Int], List[Int]) = {
    if (start >= name.length || path.isEmpty || path.head != hashChar(name(start))) (start, path, Nil)
    else {
      val pb = List.newBuilder[Int]
      @tailrec
      def rec(i: Int, remainder: List[Int]): (Int, List[Int], List[Int]) = {
        if(remainder.isEmpty) (i, remainder, path)
        else if (i >= name.length || remainder.head != hashChar(name(i)))
          (i, remainder, pb.result())
        else {
          pb += remainder.head
          rec(i + 1, remainder.tail)
        }
      }
      pb += path.head
      rec(start + 1, path.tail)
    }
  }

}