package lockfree

import scala.annotation.tailrec

class SortedList extends AbstractSortedList {

  // The sentinel node at the head.
  private val _head = createNode(0, None, isHead=true)

  // The first logical node is referenced by the head.
  def firstNode: Option[Node] = _head.next

  // Finds the first node whose value satisfies the predicate.
  // Returns the predecessor of the node and the node.
  def findNodeWithPrev(pred: Int => Boolean): (Node, Option[Node]) = {
    def traverse(prev: Node, e: Option[Node]): (Node, Option[Node]) =
      e match{
        case None => (prev, e)
        case Some(e1) => 
          if(e1.deleted) {
            //saves predecessor
            val prevAtomic = prev.atomicState
            //uses compareAndSet to find element
            prevAtomic.compareAndSet((e, false), (e1.next,false))
            //restarts at head regardless
            traverse(_head, firstNode)
          }
          else if (pred(e1.value)) (prev, e)
          else traverse(e1, e1.next)
      }
    traverse(_head, firstNode)
  }

  // Insert an element in the list.
  /*
   * Your first goal will be to code the insert method of the
   * lock-free list. The idea is very simple:
   * Locate the position you need to insert by using the
   * findNodeWithPrev method you have just implemented.
   * Create a new node which holds the value and points
   * to the correct next node. For this, you should use
   * createNode(value, nextNode).
   * Use the compareAndSet operation to make the previous
   * node point to your newly created node.
   * If the operation failed, retry from the start!
   * Otherwise, the operation is done!
   */
  def insert(e: Int): Unit = {
    val (prev, elem) = findNodeWithPrev({x: Int => x >= e})
    val update = createNode(e, elem)    
    val prevAtomic = prev.atomicState
    val success = prevAtomic.compareAndSet((elem, false), (Some(update), false))
    if(!success)
      insert(e)
  }

  // Checks if the list contains an element.
  /*
   * Next, you should implement the contains method,
   * which checks if the list contains a given element.
   * You may of course use the findNodeWithPrev method
   * you have implemented.
   */
  def contains(e: Int): Boolean = {
    findNodeWithPrev({x: Int => x == e})._2 match {
      case Some(v) => true
      case None => false
    }
  }

  // Delete an element from the list.
  // Should only delete one element when multiple occurences are present.
  /*
   * Now, we can implement a version of delete that is
   * not vulnerable to the problem exposed earlier.
   * The idea is to:
   * Find the node to delete and its predecessor using findNodeWithPrev.
   * Mark the node as deleted using its method.
   * If it returns false, retry from the start!
   * Once this is done, we still have to modify the other
   * list methods to handle the fact that nodes can be marked.
   * Marked node will be deleted within the findNodeWithPrev method.
   */
  def delete(e: Int): Boolean = {
    val (prev, elem) = findNodeWithPrev({x: Int => x == e})
    elem match {
      case Some(v) => 
          if(v.deleted) delete(e)
          else if(v.mark) true
          else delete(e)
      case None => false
    }
  }
}
