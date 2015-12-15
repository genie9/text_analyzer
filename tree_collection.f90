! creating node-type of the tree !
module node_def
  implicit none

  type, public               :: Node
    character(80)            :: word
    integer                  :: iword = 0
    logical                  :: color
    type (Node), pointer     :: left
    type (Node), pointer     :: right
    type (Node), pointer     :: parent
  end  type
 
end module node_def

module  GLOBAL
   use  node_def

   private
   logical, public                 :: Seen_EOF
   logical, parameter, public      :: Red = .true., Black = .false.
   type (Node), pointer, public    :: Root, Current

end  module  GLOBAL

! functions and subroutines to use binary tree: insert, rotate, traversal !
module tree_collection 
   use node_def
   use word_collection
   use GLOBAL

   implicit none
   public :: create_node, insert_in_tree, ROTATE_LEFT, ROTATE_RIGHT, &
             REBALANCE_TREE, TRAVERSE

contains

  ! This procedure receives pointer to an isolated new node, in which all pointers are null, and which
  ! contains a value, ready for insertion in the tree.
  ! The procedure causes the tree to be searched from the root, along a path determined by
  ! the new node's value, until the path terminates in a null pointer. The node is then
  ! inserted in that position, and is colored red. The pointer to it's parent is then
  ! initialized as W. The pointer V indicates the position of the current node in the tree as the
  ! search progresses along a branch.
  subroutine  insert_in_tree(new)
      type (Node), pointer         :: new
      type (Node), pointer         :: V, W
      integer                      :: res

      V => Root                                   ! The initial position of the search.
      nullify (W)                                 ! A pointer to the parent of V.

      ! Find the place in the tree to graft the new node - travel from the root to a node
      ! containing a null pointer.
      do
         if (.not. associated (V) ) then
            exit
         end  if
         W => V
         res = compare(new%word, V%word) 
         if(res  < 0) then                        ! Take the left branch.
            V => V%left
         else if(res > 0) then                    ! Take the right branch.
            V => V%Right
         else                                     ! If same word add count.
            V%iword = V%iword + 1
            return
         end  if
      end  do
      !! We have found a node W whose left or right pointer field is null.

      new%parent => W                             ! Make the new node point at its parent.
      if ( .not. associated(W) ) then             ! There's only one node in the tree.
         Root => new
      else if (new%word < W%word) then            ! Make the parent point at the new node.
         W%left => new                            ! The new node is in the left branch.
      else
         W%Right => new                           ! The new node is in the right branch.
      end  if

end  subroutine  insert_in_tree

  ! This procedure performs a left rotation of a branch of a Red-Black tree, with node <Pivot>
  ! as the pivot.
  subroutine rotate_left(pivot)
      !! INCOMING: Pivot = a pointer to the node about which a branch of the tree is to be rotated.
      type (Node), pointer         :: pivot

      type (Node), pointer         :: Y, X

      X => pivot                                    ! Can't use Pivot directly.  Must use a temporary.

      Y => X%right                                  ! Y is the right child of X.
      X%right => Y%left                             ! Change the left subtree of Y into X's right
                                                    ! subtree.
      if(associated(Y%left) ) then
         Y%left%parent => X                         ! The left sub-node of Y points back at X --
                                                    ! that is, X is now the parent of Y's left subtree.
      end  if
      Y%parent => X%parent                          ! X's parent now becomes the parent of Y.
      if(.not. associated (X%parent) ) then         ! We are at the root node.
         Root => Y
      else if(associated (X, X%parent%left) ) then  ! X is on the left of its parent.
         X%parent%left => Y                         ! The left pointer of X's parent points at Y.
      else                                          ! X is on the right subtree of its parent.
         X%parent%right => Y                        ! The right pointer of X's parent points at Y.
      end  if
      Y%left => X                                   ! Make X a left child of node Y.  (X's left sub-
                                                    ! tree comes with X).
      X%parent => Y                                 ! X's parent is now Y.

  end subroutine rotate_left

  ! This procedure performs a right rotation of a branch of a Red-Black tree, with node <Pivot>
  ! as the pivot.  This procedure is the mirror image of ROTATE_left, with the words Left
  ! and Right interchanged.
  subroutine  rotate_right(pivot)
      !! INCOMING: Pivot = a pointer to the node about which a branch of the tree is to be rotated.
      type (Node), pointer         :: pivot
      type (Node), pointer         :: Y, X

      X => pivot                                    ! Can't use Pivot directly.  Must use a temporary.

      Y => X%left                                   ! Y is the left child of X.
      X%left => Y%right                             ! Change the right subtree of Y into X's left
                                                    ! subtree.
      if(associated(Y%right) ) then
         Y%right%parent => X                        ! The right sub-node of Y points back at X --
      end if                                        ! that is, X is now the parent of Y's right subtree
      
      Y%parent => X%parent                          ! X's parent now becomes the parent of Y.
      if (.not. associated(X%parent) ) then         ! We are at the root node.
         Root => Y
      else if(associated(X, X%parent%right)) then   ! X is on the right of its parent.
         X%parent%right => Y                        ! The right pointer of X's parent points at Y.
      else                                          ! X is on the left of its parent.
         X%parent%left => Y                         ! The left pointer of X's parent points at Y.
      end if
      Y%right => X                                  ! Put X on the left of Y.
      X%parent => Y                                 ! X's parent is now Y.

  end subroutine rotate_right

  ! This procedure rebalances and re-colors the nodes of a tree following an insertion.  
  subroutine  rebalance_tree(dummy_current)
      !! INCOMING: Dummy_Current = a pointer to a (red) node that has been inserted in the tree.
      type (Node), pointer         :: dummy_current
      type (Node), pointer         :: X, Y
      logical                      :: red_uncle
      logical                      :: iterating

      X => dummy_current
      
      do                                             ! This loop is exexuted if...
         iterating = .not. associated(X, Root)       ! ..we are not at root and ...
         if(iterating) then                          ! ..parent is also red and ...
           iterating = X%parent%color .eqv. Red  
         end  if
         if(iterating) then                          ! ..there is a grandparent.
           iterating = associated(X%parent%parent)
         end  if
         if(.not. iterating) then                
           exit
         end  if

         if(associated(X%parent, X%parent%parent%left) ) then
                                                    
            Y => X%parent%parent%right               ! The parent is a left node of X's grandparent
            red_uncle = associated(Y)                ! and the uncle is right node of grandparent
            if (red_uncle) then
               red_uncle = Y%color .eqv. Red
            end  if
                              ! CASE 1 !
            if (red_uncle) then                      ! If uncle, parent and X are all red  
               X%parent%color = Black                ! The parent must be black.
               Y%color = Black                       ! The uncle must be black.
               X%parent%parent%color = Red           ! The grandparent must be red.
               X => X%parent%parent                  ! Move 2 places up the tree, to the grandparent.
                              ! CASE 2 !
            else                                     ! The uncle is black, or is non-existent.
               if (associated (X, X%parent%right) ) then 
                  X => X%parent                      ! Move up the tree.
                  call rotate_left(X)
               end  if
                              ! CASE 3 !
               X%parent%color = Black
               X%parent%parent%color = Red
               call rotate_right(X%parent%parent)
            end  if
         ! This segment is the mirror image of the code for the "then" part,
         ! with the words Right and left interchanged
         else                                        ! The parent is a right node of X's grandparent.
            Y => X%parent%parent%left                ! Get the address of the uncle.
            red_uncle = associated(Y)
            if (red_uncle) then
               red_uncle = Y%color .eqv. Red
            end  if
                              ! CASE 1 !
            if (red_uncle) then                      ! If uncle, parent and X are all red  
               X%parent%color = Black                ! The parent must be black.
               Y%color = Black                       ! The uncle must be black.
               X%parent%parent%color = Red           ! The grandparent must be red.
               X => X%parent%parent                  ! Move 2 places up the tree, to the grandparent.
                              ! CASE 2 !
            else                                     ! The uncle is black, or is non-existent.
               if (associated (X, X%parent%left) ) then 
                  X => X%parent                      ! Move up the tree.
                  call rotate_right(X)
               end  if
                              ! CASE 3 !
               X%parent%color = Black
               X%parent%parent%color = Red
               call rotate_left(X%parent%parent)
            end  if
         end  if
      end  do

      Root%color = Black                              ! Ensure that the root is black.

  end  subroutine  rebalance_tree

  ! This recursive subroutine retrieves and prints the values from the tree in ascending order.
  recursive subroutine traverse(Current)
      type (Node), pointer :: Current

      if(associated(Current%left) ) then             ! Take the left subtree.
         call traverse(Current%left)
      end  if

      record(Current%word)                           ! Retrieve value from tree and record it

      if(associated(Current%right) ) then            ! Take the right subtree.
         call traverse(Current%right)
      end if

  end subroutine traverse
  
  ! This procedure creates a node from given data.
  ! Then it calls the procedure insert_in_tree.
  subroutine create_node()
      character(len= :), allocatable :: new_word

      new_word = to_lower(read_one())             ! The next value to be inserted.
      if(new_word == '') then                     ! We encountered the end of the file.
       Seen_EOF = .true.
       return
      end if
 
      allocate(Current)                           ! Create a node.
      Current%word = new_word
      Current%color = Red                         ! New nodes are always Red.
      nullify(Current%left)
      nullify(Current%right)
      call insert_in_tree(Current)                ! Insert the new node in the tree.

  end subroutine create_node

end  module tree_collection
