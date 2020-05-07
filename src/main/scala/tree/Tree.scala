package tree

object NaiveBST {
  sealed trait Node[+V] {
    val l: Node[V] = Empty
    val r: Node[V] = Empty
  } 
  case class OccupiedNode[V](v: V, override val l: Node[V] = Empty, override val r: Node[V] = Empty) extends Node[V]
  case object Empty extends Node[Nothing]

  object BST {
    def apply[V]()(implicit o: V => Ordered[V]): BST[V] = BSTImpl(Empty)
    def apply[V](v: V)(implicit o: V => Ordered[V]): BST[V] = BSTImpl(OccupiedNode(v))
  }
  
  sealed trait BST[V] {
    def insert(v: V): BST[V]
    def insert(vs: V*): BST[V]
    def find(v: V): Option[OccupiedNode[V]]
    def root: Node[V]
    def delete(v: V): BST[V]
  }

  // users should not be able to construct a BST themselves using a invalid root node for root of BST
  private case class BSTImpl[V](root: Node[V])(implicit o: V => Ordered[V]) extends BST[V] {

    // influenced by: http://erichgess.github.io/blog/2016/03/21/purely-functional-data-structures-chapter-2-binary-search-trees/
    def insert(v: V): BST[V] = {
      def insert(root: Node[V], v: V): Node[V] = {
        root match {
          case Empty => OccupiedNode(v)
          case OccupiedNode(ov, l ,r) => {
            if (v < ov) {
              OccupiedNode(ov, insert(l, v), r)
            } else if (v > ov) {
              OccupiedNode(ov, l, insert(r, v))
            } else {
              // don't allow duplicates for now
              root
            }
          }
        }
      }
      BSTImpl(insert(root, v))
    }

    def insert(vs: V*): BST[V] = vs.foldLeft(BST[V]())((r, v) => r.insert(v))

    // find returns an option of a occupied node since empty cannot be returned from find; instead find returns None
    def find(v: V): Option[OccupiedNode[V]] = {
      def find(root: Node[V], v: V): Option[OccupiedNode[V]] = {
        root match {
          case Empty => None
          case OccupiedNode(ov, l, r) => {
            if (v < ov) {
              find(l, v) 
            } else if (v > ov) {
              find(r, v) 
            } else {
              Some(OccupiedNode(ov, l, r))
            }
          }
        }
      }
      find(root, v)
    }

    // successor should only be called on a occupied node and return an occupied node of a right subtree
    private def successor(root: OccupiedNode[V]): OccupiedNode[V] = {
      root match {
        case OccupiedNode(v, OccupiedNode(lv, ll, lr), r) => successor(OccupiedNode(lv, ll, lr))
        case _ => root
      }
    }

    // compiler will NOT perform exhaustiveness checking with guards in cases: https://stackoverflow.com/questions/34022160/exhaustiveness-check-for-pattern-matching-in-scala-2-11
    // Influenced by https://codereview.stackexchange.com/questions/187716/deleting-a-node-from-binary-tree-in-ocaml
    private def delete(root: Node[V], v: V): Node[V] = root match {
      case Empty => root
      case OccupiedNode(ov, l, r) => {
        if (v < ov) OccupiedNode(ov, delete(l, v), r) else if (v > ov) OccupiedNode(ov, l, delete(r, v)) else {
          root match {
            case Empty => root
            case OccupiedNode(ov, l, Empty) => l
            case OccupiedNode(ov, l, OccupiedNode(rv, rl, rr)) => {
              val s = successor(OccupiedNode(rv, rl, rr))
              val nv = s.v
              OccupiedNode(nv, l, delete(OccupiedNode(rv, rl, rr), nv))
            }    
          }
        }
      }
    }

    def delete(v: V): BST[V] = BSTImpl(delete(root, v))
  }
}