! creating node-type of the tree !
module node_def
  implicit none

  type, public               :: Node
    character(80)            :: word
    integer                  :: iword = 1
    logical                  :: color
    type (Node), pointer     :: left
    type (Node), pointer     :: right
    type (Node), pointer     :: parent
  end  type
 
end module node_def

! Global stuff
module  GLOBAL
   use  node_def

   private
   logical, public                 :: Seen_EOF
   logical, parameter, public      :: Red = .true., Black = .false.
   type (Node), pointer, public    :: Root, Current

end  module  GLOBAL

