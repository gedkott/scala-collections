package tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import NaiveBST._

// examples and influence from here: http://www.mathcs.emory.edu/~cheung/Courses/171/Syllabus/9-BinTree/BST-delete2.html
class TreeSpec extends AnyFlatSpec with Matchers {
  "inserting into naive BST" should "work" in {
    val bst = BST[Int](10)
    
    bst.insert(15).root shouldBe OccupiedNode(10, r=OccupiedNode(15))
  }

  "inserting into naive BST" should "not allow duplicates" in {
    val bst = BST[Int]()

    bst.insert(10).insert(15).insert(10).root shouldBe OccupiedNode(10, r=OccupiedNode(15))
  }

  "finding a value in a naive BST" should "work" in {
    val bst = BST[Int]()

    bst.insert(10).insert(15).insert(10).insert(3).insert(4).find(3) shouldBe Some(OccupiedNode(3, r=OccupiedNode(4)))
  }

  "finding a value in a naive BST" should "return None when value isn't in tree" in {
    val bst = BST[Int]()

    bst.insert(10).insert(15).insert(10).insert(3).insert(4).find(200) shouldBe None
  }

  "deleting a value in a naive BST" should "work on empty trees" in {
    val bst = BST[Int]()

    bst.delete(10).root shouldBe Empty
  }

  "deleting a value in a naive BST" should "work on trees with one child of root" in {
    val bst = BST[Int]()

    /*
     *        15
     *      10
     * 
     *        ||
     *        \/
     * 
     *        10
     */
    bst.insert(15).insert(10).delete(15).root shouldBe OccupiedNode(10)
  }

  "deleting a value in a naive BST" should "work on trees with two children of root" in {
    val bst = BST[Int]()

    /*
     *        15
     *      10  17
     * 
     *        ||
     *        \/
     * 
     *        17
     *      10
     */
    bst.insert(15).insert(10).insert(17).delete(15).root shouldBe OccupiedNode(17, l=OccupiedNode(10))
  }

  "deleting a value in a naive BST" should "work on deeper trees with two children of root" in {
    val bst = BST[Int]()

    /*
     *        15
     *     10     17
     *    4  11 16  18
     * 
     *        ||
     *        \/
     * 
     *        15
     *     10     18
     *    4  11 16
     */
    bst.insert(15).insert(10).insert(17).insert(4).insert(11).insert(16).insert(18).delete(17).root shouldBe OccupiedNode(15,OccupiedNode(10,OccupiedNode(4,Empty,Empty),OccupiedNode(11,Empty,Empty)),OccupiedNode(18,OccupiedNode(16,Empty,Empty),Empty))
  }

  "deleting a value in a naive BST" should "work on deeper trees with two children of root and deleting from left side" in {
    val bst = BST[Int]()

    /*
     *        15
     *     10     17
     *    4  11 16  18
     * 
     *        ||
     *        \/
     * 
     *        15
     *     11     17
     *   4      16  18
     */
    bst.insert(15).insert(10).insert(17).insert(4).insert(11).insert(16).insert(18).delete(10).root shouldBe OccupiedNode(15,OccupiedNode(11,OccupiedNode(4,Empty,Empty),Empty),OccupiedNode(17,OccupiedNode(16,Empty,Empty),OccupiedNode(18,Empty,Empty)))
  }

  "deleting a value in a naive BST" should "work on trees that just have a root node and no children" in {
    val bst = BST[Int]()

    bst.insert(15).delete(15).root shouldBe Empty
  }

  "deleting a value in a naive BST" should "work on deep successor nodes" in {
    val bst = BST[Int]()

    /*
     *         6
     *      2     9
     *    1      8  15
     *             13 18
     *            11
     * 
     * 
     *        ||
     *        \/
     * 
     *         6
     *      2     11
     *    1      8  15
     *             13 18
     *            
     */
    bst.insert(6, 9, 2, 15, 8, 1, 18, 13, 11).delete(9).root shouldBe bst.insert(6, 11, 2, 15, 8, 1, 18, 13).root
  }
}
