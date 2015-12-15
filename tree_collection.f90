! creating node-type of the tree !
module node
  implicit none

  type, public                      :: Node
    character(len=:), allocatable   :: word
    integer                         :: appearance = 0
    logical                         :: color
    type (Node), pointer            :: left
    type (Node), pointer            :: right
    type (Node), pointer            :: parent
  end  type
 
end module node

module  GLOBAL
   use  node

   private
   logical, public                 :: Seen_EOF
   logical, parameter, public      :: Red = .true., Black = .false.
   type (Node), pointer, public    :: Root, Current

end  module  GLOBAL

! functions and subroutines to use binary tree: insert, rotate, traversal !
module tree_collection 
   implicit none
   public :: CREATE_NODE, INSERT_IN_TREE, ROTATE_LEFT, ROTATE_RIGHT, &
             REBALANCE_TREE, TRAVERSE

contains

!! This procedure receives an isolated node NEW, in which all pointers are null, and which
!! contains a value, ready for insertion in the tree.
!! The procedure causes the tree to be searched from the root, along a path determined by
!! the node New's value, until the path terminates in a null pointer.  The node is then
!! inserted in that position, and is colored red.  The pointer to it's parent is then
!! initialized.  The pointer V indicates the position of the current node in the tree as the
!! search progresses along a branch.  Pointer W gives the current parent of V.
subroutine  INSERT_IN_TREE (new)
      !! INCOMING: New = A pointer to a node to be inserted into the tree.
      use  Node
      use  GLOBAL
      type (Node), pointer         :: new
      type (Node), pointer         :: V, W

      V => Root                                   !! The initial position of the search.
      nullify (W)                                 !! A pointer to the parent of V.

      !! Find the place in the tree to graft the new node - travel from the root to a node
      !! containing a null pointer.
      do
         if (.not. associated (V) ) then
            exit
         end  if
         W => V
         if (compare(New%word,V%word)) then              !! Take the left branch.
            V => V%Left
         else                                     !! Take the right branch.
            V => V%Right
         end  if
      end  do
      !! We have found a node W whose left or right pointer field is null.

      New%Parent => W                             !! Make the new node point at its parent.
      if ( .not. associated(W) ) then             !! There's only one node in the tree.
         Root => New
      else if (New%Item < W%Item) then            !! Make the parent point at the new node.
         W%Left => New                            !! The new node is in the left branch.
      else
         W%Right => New                           !! The new node is in the right branch.
      end  if

end  subroutine  INSERT_IN_TREE

!! « insert subroutine ROTATE_LEFT from Figure 3.11 »

!! This procedure performs a left rotation of a branch of a Red-Black tree, with node <Pivot>
!! as the pivot.
subroutine  ROTATE_LEFT (Pivot)
      !! INCOMING: Pivot = a pointer to the node about which a branch of the tree is to be rotated.
      use  Node_Definition
      use  GLOBAL
      type (Node), pointer         :: Pivot

      type (Node), pointer         :: Y, X

      X => Pivot                                  !! Can't use Pivot directly.  Must use a temporary.

      Y => X%Right                                !! Y is the right child of X.
      X%Right => Y%Left                           !! Change the left subtree of Y into X's right
                                                  !! subtree.
      if (associated(Y%Left) ) then
         Y%Left%Parent => X                       !! The left sub-node of Y points back at X --
                                                  !! that is, X is now the parent of Y's left subtree.
      end  if
      Y%Parent => X%Parent                        !! X's parent now becomes the parent of Y.
      if (.not. associated (X%Parent) ) then      !! We are at the root node.
         Root => Y
      else if (associated (X, X%Parent%Left) ) then   !! X is on the left of its parent.
         X%Parent%Left => Y                       !! The left pointer of X's parent points at Y.
      else                                        !! X is on the right subtree of its parent.
         X%Parent%Right => Y                      !! The right pointer of X's parent points at Y.
      end  if
      Y%Left => X                                 !! Make X a left child of node Y.  (X's left sub-
                                                  !! tree comes with X).
      X%Parent => Y                               !! X's parent is now Y.

end  subroutine  ROTATE_LEFT

!! « insert subroutine ROTATE_RIGHT from Figure 3.12 »

!! This procedure performs a right rotation of a branch of a Red-Black tree, with node <Pivot>
!! as the pivot.  This procedure is the mirror image of ROTATE_LEFT, with the words Left
!! and Right interchanged.
subroutine  ROTATE_RIGHT (Pivot)
      !! INCOMING: Pivot = a pointer to the node about which a branch of the tree is to be rotated.
      use  Node_Definition
      use  GLOBAL
      type (Node), pointer         :: Pivot

      type (Node), pointer         :: Y, X

      X => Pivot                                  !! Can't use Pivot directly.  Must use a temporary.

      Y => X%Left                                 !! Y is the left child of X.
      X%Left => Y%Right                           !! Change the right subtree of Y into X's left
                                                  !! subtree.
      if (associated (Y%Right) ) then
         Y%Right%Parent => X                      !! The right sub-node of Y points back at X --
                                                  !! that is, X is now the parent of Y's right subtree
      end  if
      Y%Parent => X%Parent                        !! X's parent now becomes the parent of Y.
      if (.not. associated (X%Parent) ) then      !! We are at the root node.
         Root => Y
      else if (associated (X, X%Parent%Right) ) then  !! X is on the right of its parent.
         X%Parent%Right => Y                      !! The right pointer of X's parent points at Y.
      else                                        !! X is on the left of its parent.
         X%Parent%Left => Y                       !! The left pointer of X's parent points at Y.
      end  if
      Y%Right => X                                !! Put X on the left of Y.
      X%Parent => Y                               !! X's parent is now Y.

end  subroutine  ROTATE_RIGHT

!! « insert subroutine REBALANCE_TREE from Figure 3.13 »

!! This procedure rebalances and re-colors the nodes of a Red-Black sorted binary tree,
!! following an insertion.  The node pointed at by X has just been inserted into the tree,
!! and is the node where one of the 4 properties of Red-Black trees may have been violated.
!! After a breach of Rule 1 has been rectified, X is adjusted to point at its grandparent,
!! where tests for a breach of the rules is again carried out, and so on for each grandparent
!! in turn.  If Rules 2 or 3 are breached, one or two rotations of a branch of the tree are
!! carried out (about the current node X), and the procedure terminates.
!! When this procedure terminates, the root node is black, and the tree is quasi-balanced,
!! and the four properties of Red-Black trees are satisfied:
!!
!! Rule 1. Every node is either red or black;
!! Rule 2. Every null pointer is considered to be pointing to an imaginary black node;
!! Rule 3. If a node is red, then both its children are black; and
!! Rule 4. Every direct path from the root to a leaf contains the same number of black nodes.

subroutine  REBALANCE_TREE (Dummy_Current)
      !! INCOMING: Dummy_Current = a pointer to a (red) node that has been inserted in the tree.
      use  Node_Definition
      use  GLOBAL
      type (Node), pointer         :: Dummy_Current

      type (Node), pointer         :: X, Y
      logical                      :: Red_Uncle
      logical                      :: Iterating

      X => Dummy_Current

      do                                          !! We execute this loop, and re-execute it,
                                                  !! when X and its parent are both red.
         Iterating = .not. associated (X, Root)   !! Cannot iterate when we are at the root.
         if (Iterating) then                      !! There must be a parent . . . .
            Iterating = X%Parent%Color .eqv. Red  !!
         end  if
         if (Iterating) then                      !! . . . and there must be a grandparent.
            Iterating = associated (X%Parent%Parent)
         end  if
         if (.not. Iterating) then                !! We enter this loop when X and its parent are both red.
            exit
         end  if

         if (associated (X%Parent, X%Parent%Parent%Left) ) then
                                                  !! The parent is a left node of X's grandparent.
            Y => X%Parent%Parent%Right            !! Get the address of the uncle.
            Red_Uncle = associated(Y)
            if (Red_Uncle) then
               Red_Uncle = Y%Color .eqv. Red
            end  if
            if (Red_Uncle) then
               !! CASE 1.  There is an uncle.  X and its parent and
               !! uncle are all red.  Fix violation of Rule 3.
               X%Parent%Color = Black             !! The parent must be black.
               Y%Color = Black                    !! The uncle must be black.
               X%Parent%Parent%Color = Red
                                                  !! The grandparent must be red.
               X => X%Parent%Parent               !! Move 2 places up the tree, to the grandparent.
            else                                  !! The uncle is black, or is non-existent.
               if (associated (X, X%Parent%Right) ) then !! CASE 2.
                  X => X%Parent                   !! Move up the tree.
                  call ROTATE_LEFT (X)
               end  if
               !! CASE 3.
               X%Parent%Color = Black
               X%Parent%Parent%Color = Red
               call ROTATE_RIGHT (X%Parent%Parent)
            end  if
         !! This segment is the mirror image of the code for the "then" part,
         !! with the words Right and Left interchanged.
         else                                     !! The parent is a right node of X's grandparent.
            Y => X%Parent%Parent%Left             !! Get the address of the uncle.
            Red_Uncle = associated(Y)
            if (Red_Uncle) then
               Red_Uncle = Y%Color .eqv. Red
            end  if
            if (Red_Uncle) then                   !! CASE 1.
               X%Parent%Color = Black             !! The parent must be black.
               Y%Color = Black                    !! The uncle must be black.
               X%Parent%Parent%Color = Red
                                                  !! The grandparent must be red.
               X => X%Parent%Parent               !! Move 2 places up the tree, to the grandparent.
            else                                  !! X and its parent are red, but its uncle is black
                                                  !! or is missing.  Fix violation of Rule 3.
               if (associated (X, X%Parent%Left) ) then !! CASE 2.
                  X => X%Parent                   !! Move up the tree.
                  call ROTATE_RIGHT (X)
               end  if
               !! CASE 3.
               X%Parent%Color = Black
               X%Parent%Parent%Color = Red
               call ROTATE_LEFT (X%Parent%Parent)
            end  if
         end  if
      end  do

      Root%Color = Black                          !! Ensure that the root is black.

end  subroutine  REBALANCE_TREE

!! « insert subroutine TRAVERSE from Figure 3.7 »

   !! This recursive subroutine retrieves and prints the values from the tree in ascending order.
   recursive  subroutine  TRAVERSE (Current)
      use  Node_Definition
      type (Node), pointer         :: Current

      if (associated (Current%Left) ) then        !! Take the left subtree.
         call TRAVERSE (Current%Left)
      end  if

      print *, Current%Item                       !! Retrieve value from tree and print it.

      if (associated (Current%Right) ) then       !! Take the right subtree.
         call TRAVERSE (Current%Right)
      end  if

   end  subroutine  TRAVERSE

   !! This procedure creates a node, reads in data and stores it in the node, and initializes the
   !! node's left & right pointers.  It then calls the procedure INSERT_IN_TREE to insert the
   !! node in the tree.
   subroutine  CREATE_NODE ( )
      use  node
      use  GLOBAL
      use  word_collection

      read_one()
      read (unit=*, fmt=*, iostat=Status) Number  !! The next value to be inserted.
      if (Status < 0) then                        !! We encountered the end of the file.
         Seen_EOF = .true.
         return
      end  if

      if (Number == 0) then                       !! A zero will also terminate input.
         Seen_EOF = .true.
         return
      end  if

      allocate (Current)                          !! Create a node.
      Current%Item = Number
      Current%Color = Red                         !! New nodes are always Red.
      nullify (Current%Left)
      nullify (Current%Right)
      call INSERT_IN_TREE (Current)               !! Insert the new node in the tree.

   end  subroutine  CREATE_NODE

end  module  GROUP

!! This program implements a Red-Black binary search tree, and accepts data, installs it into
!! the appropriate position in the tree, and automatically performs rotations of its branches
!! when the tree becomes unbalanced, thereby maintaining the tree quasi-balanced.  The
!! program then prints the data in increasing order, by means of an in-order traversal.
program  RED_BLACK
   use  node
   use  GLOBAL
   use  tree_collection
   implicit  none

   integer                         :: N

!   Seen_EOF = .false.

!   print *, "Please enter the elements to be sorted:"
!   print *, "Please type values, one to a line (to end, enter 0, or give the end-of-file key sequence):"

   nullify (Root)
   call CREATE_NODE ( )
   Root => Current
   nullify (Root%Parent)                          !! The root has no parent.
   Root%Color = Black                             !! The root is always Black.

   do
      call CREATE_NODE ( )
      if (Seen_EOF)  then
         exit
      end  if
      call REBALANCE_TREE (Current)
   end  do

   !! No more elements to insert, so display the contents of the tree.
   call TRAVERSE (Root)

end  program  RED_BLACK

!! Figure 3.14  Algorithm for manipulating a Red-Black binary tree
