// A class of objects to represent a set


//####### NEED TO WRITE SCALATEST TESTS

/*

Do I use a dummy header node?  - Yes - easier to check for empty set with same type. 
Do I allow multiple of the same integer or no?  - No - makes everything easier
Do I store in increasing order?  - Yes - makes contains more efficient, equals more effifient, remove too
Anything else in the state for efficiency?

Operations: 
- Initialise empty set
- Write the set as a string
- Add element e to the set
- Allow for initialisation of sets with particular elements
- Size function
- Contains x function - if two bits of code are the same factor out in a priv function
- Any - return any element
- Equals
- Remove from set 
- subsetOf another set
- set union
- set intersection
- set map
- set filter

Write O(...) for each. 
Write invariants for loops. 
Write size of sets as arguments. 

*/


class IntSet{


  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}
  private var theSet : Node = Node(Int.MinValue, null) // or however empty set is represented

  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */

  // O(n)
  override def toString: String = {
    var str = new String("{")
    var currentNode = theSet.next
    // Invariant: str contains the first n nodes in the set s as strings ^ currentNode is of type node 
    while (currentNode != null) {
      str = str ++ (currentNode.datum.toString)
      if (currentNode.next != null) str = str ++ ", "
      currentNode = currentNode.next
    }
    str = str ++ "}"
    str
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  // O(n)
  def add(e: Int) : Unit = {
    var currentNode = theSet

    // Invariant - currentNode has a next ^ each node passed has datum < e
    while (currentNode.next != null && e > currentNode.next.datum) currentNode = currentNode.next
    
    if (currentNode.next == null) currentNode.next = Node(e, null)
    else if (currentNode.next.datum == e) {return}
    else currentNode.next = Node(e, currentNode.next)
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  // O(n)
  def size : Int = {
    var currentNode = theSet.next
    var count = 0

    // Invariant - count = len(Set[0..count])
    while (currentNode != null) {
      count += 1
      currentNode = currentNode.next
    }
    count
  }

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  // O(n)
  def contains(e: Int) : Boolean = {
    var currentNode = theSet.next

    // Invariant: the nodes so far all have values <= e
    while (currentNode != null && currentNode.datum <= e) {
      if (currentNode.datum == e) return true
      currentNode = currentNode.next
    }
    return false
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = ???

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  
  // O(n)
  override def equals(that: Any) : Boolean = that match {
    case s: IntSet => {
      var currentNode = theSet
      var otherCurrentNode = s.theSet
      // Invariant: each node up to the one being compared are equal. 
      while (otherCurrentNode.next.datum == currentNode.next.datum) {
        if (currentNode.next.next == null && otherCurrentNode.next.next == null) return true
        else if (currentNode.next.next == null || otherCurrentNode.next == null) return false
        otherCurrentNode = otherCurrentNode.next
        currentNode = currentNode.next
      }
      false
    }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  
  // O(n)
  def remove(e: Int) : Boolean = {
    var currentNode = theSet
    // Invariant - the set passed through so far does not contain e. 
    while (currentNode.next != null && currentNode.next.datum <= e) {
      if (currentNode.next.datum == e) {
        currentNode.next = currentNode.next.next
        return true
      }
      currentNode = currentNode.next
    }
    false
  }
    
  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  
  // O(n)
  def subsetOf(that: IntSet) : Boolean = {
    var thisCurrentNode = theSet.next
    var thatCurrentNode = that.theSet.next

    // Invariant: the nodes passed of thisCurrentNode are in thatCurrentNode
    while (thatCurrentNode != null && thisCurrentNode != null) {
      if (thisCurrentNode.datum < thatCurrentNode.datum) return false

      if (thisCurrentNode.datum == thatCurrentNode.datum) {
        thisCurrentNode = thisCurrentNode.next
      }
      thatCurrentNode = thatCurrentNode.next
    }
    thisCurrentNode == null
  }

  // ----- optional parts below here -----

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  //O(n+m)
  def union(that: IntSet) : IntSet = {
    var thisCurrentNode = theSet
    var thatCurrentNode = that.theSet
    var newSet = IntSet()
    var newSetSet = newSet.theSet
    while (thatCurrentNode.next != null && thisCurrentNode.next != null) {
      if (thisCurrentNode.next.datum == thatCurrentNode.next.datum) {
        newSetSet.next = Node(thisCurrentNode.next.datum, null)
        thisCurrentNode = thisCurrentNode.next
        thatCurrentNode = thatCurrentNode.next
      } else if (thisCurrentNode.next.datum > thatCurrentNode.next.datum) {
        newSetSet.next = Node(thatCurrentNode.next.datum, null)
        thatCurrentNode = thatCurrentNode.next
      } else {
        newSetSet.next = Node(thisCurrentNode.next.datum, null)
        thisCurrentNode = thisCurrentNode.next
      }
      newSetSet = newSetSet.next

    }  // Maybe make into subroutine
    if (thatCurrentNode.next == null) {
      // Both invariants that newSetSet[endOfOldWhile: now] = thisCurrentNode[endOfOldWhile: now]
      while (thisCurrentNode.next != null) {
        newSetSet = Node(thisCurrentNode.next.datum, null)
        newSetSet = newSetSet.next
        thisCurrentNode = thisCurrentNode.next
      }
    } else if (thisCurrentNode.next == null) {
        while (thatCurrentNode.next != null) {
          newSetSet.next = Node(thatCurrentNode.next.datum, null)
          newSetSet = newSetSet.next
          thatCurrentNode = thatCurrentNode.next
        }
    }
    newSet
  }

  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
    var thisCurrentNode = theSet
    var thatCurrentNode = that.theSet
    var newSet = IntSet()
    var newSetSet = newSet.theSet
    while (thatCurrentNode.next != null && thisCurrentNode.next != null) {
      if (thisCurrentNode.next.datum == thatCurrentNode.next.datum) {
        newSetSet.next = Node(thisCurrentNode.next.datum, null)
        newSetSet = newSetSet.next
        thisCurrentNode = thisCurrentNode.next
        thatCurrentNode = thatCurrentNode.next
      } else if (thisCurrentNode.next.datum > thatCurrentNode.next.datum) {
        thatCurrentNode = thatCurrentNode.next
      } else {
        thisCurrentNode = thisCurrentNode.next
      }
      

    }  

    newSet
  }
  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = {
    var newSet = IntSet()
    var newSetSet = newSet.theSet

    var thisSet = theSet.next
    while (thisSet != null) {
      newSetSet.next = Node(f(thisSet.datum), null)
      newSetSet = newSetSet.next
      thisSet = thisSet.next
    }

    newSet
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = {
    var newSet = IntSet()
    var newSetSet = newSet.theSet

    var thisSet = theSet.next
    while (thisSet != null) {
      if (p(thisSet.datum)) {
        newSetSet.next = Node(thisSet.datum, null)
        newSetSet = newSetSet.next
      }
      thisSet = thisSet.next

    }

    newSet
  }
}


// The companion object
object IntSet {
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  def main(args: Array[String]) : Unit = {
    val set = IntSet()
    println(set.toString)
    set.add(5)
    set.add(5)
    println(set)
    set.add(2)
    println(set)
    set.add(3)
    println(set)        
    println(set.size)
    println(set.contains(5))
    println(set.contains(2)) 
    println(set.contains(22))
    
    println(set.equals(IntSet(5, 3, 2, 33)))
    println(set.remove(4))
    println(set.remove(3))
    println(set)
    println(set.subsetOf(IntSet(5,3,1,2)))
    println(set.intersect(IntSet(5,3,2,3,6,1)))
    println(set.map(addOne))
    println(set.filter(equalsTwo))
  } 
  def addOne(x: Int) : Int = {
    x + 1
  }
  def equalsTwo(x: Int) : Boolean = {
    x == 2
  }
  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
