! creating node-type of the tree !
module node
  implicit none

  type, public                      :: Node
    character(len=:), allocatable   :: word
    integer                         :: appearance = 0
    logical                         :: red = .false., black = .true.
    type (Node), pointer            :: left
    type (Node), pointer            :: right
    type (Node), pointer            :: parent

  end  type
  
end module node

module  GLOBAL
  7    use  node
  8    
  9    private
 10    logical, public                 :: Seen_EOF
 11    logical, parameter, public      :: Red = .true., Black = .false.
 12    type (Node), pointer, public    :: Root, Current
 13 
 14 end  module  GLOBAL

