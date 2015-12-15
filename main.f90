! Main program checks arguments, opens text file and..........!
! calls node creation and rebalancing tree untill end of file.!
! After that traversal is called to compile final data........!
program main
  use node_def
  use GLOBAL
  use tree_collection
  use print_tree_collection
  implicit none

  character(len=80) :: filename
  integer           :: ios1, ios2
!type (node),pointer :: start
  if(command_argument_count() /= 1) then
    print *, '***Usage error: missing filename for data reading'
  end if

  call get_command_argument(1, filename)
  
  open(unit=1, file=filename, iostat=ios1, status='old', action='read')
  if(ios/=0) then
    print '(a,a,3x,i0)', '***Error in opening file: ', trim(filename), ios1
    stop
  end if

  open(unit=2, file=filename//'_analyze', iostat=ios2, status='new', action='write')
  if(ios/=0) then
    print '(a,a,3x,i0)', '***Error in creating file: ', trim(file), ios2
    stop
  end if

  Seen_EOF = .false.

  nullify(Root)
  call create_node()
  Root => Current                  ! Initializing first node to root.
  nullify(Root%parent)             ! The root has no parent.
  Root%color = Black               ! The root is always Black.

  do
    call create_node()             ! Create other nodes.
    if(Seen_EOF) then                             
      exit
    end if
    call rebalance_tree(Current)
  end do
  
  ! printing tree
  depth = 0
  maxdepth = 0
  call print_preamble()
  call print_tree(root)
  call print_epilogue()

  print *, 'ready!!'

end program main

