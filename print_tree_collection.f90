module print_tree_collection
  use node_def
  use tree_collection

  implicit none

  integer,parameter :: MAXBUF=80, GVUNIT=100
  integer           :: depth, maxdepth

contains

  recursive subroutine print_tree(p)
    implicit none

    type (Node), pointer :: p

    if (associated(p)) then
       depth = depth + 1
       if (depth > maxdepth) maxdepth = depth
       if (associated(p%left))  write(GVUNIT,'(a,a,a)') trim(p%word),' -> ',trim(p%left%word)
       if (associated(p%right)) write(GVUNIT,'(a,a,a)') trim(p%word),' -> ',trim(p%right%word)
       call print_tree(p%left)
       call print_tree(p%right)
       depth = depth-1
    end if
  end subroutine print_tree

  subroutine print_preamble()
    write(GVUNIT,'(a)') 'digraph tree {'
  end subroutine print_preamble

  subroutine print_epilogue()
    write(GVUNIT,'(a)') '}'
  end subroutine print_epilogue

end module print_tree_collection
