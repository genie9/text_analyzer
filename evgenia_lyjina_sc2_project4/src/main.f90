! Main program checks arguments, opens text file and..........!
! calls node creation and rebalancing tree untill end of file.!
! After that traversal is called to compile final data........!
program main
  use node_def
  use GLOBAL
  use tree_collection
  use print_tree_collection
  implicit none

  character(len=80) :: filename_in, filename_out
  integer           :: ios1, ios2, ind
  
  if(command_argument_count() /= 1) then
    print *, '***Usage error: missing filename for data reading'
  end if

  call get_command_argument(1, filename_in)
  
  open(unit=1, file=filename_in, iostat=ios1, status='old', action='read')
  if(ios1/=0) then
    print '(a,a,3x,i0)', '***Error in opening file: ', trim(filename_in), ios1
    stop
  end if
  
  ! Modifing the output filename to txt file.
  ! Searching for file identifier and period
  ind = scan(filename_in, '.', .true.)
  ! .. and concatenating components together
  filename_out = trim(filename_in(1:ind-1))//'_analyze.txt'

  open(unit=2, file=filename_out, iostat=ios2, status='new', action='write')
  if(ios2/=0) then
    print '(a,a,3x,i0)', '***Error in creating file: ', trim(filename_out), ios2
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

  ! Generating code for printing the tree
  depth = 0
  maxdepth = 0
  call print_preamble()
  call print_tree(root)
  call print_epilogue()
  print *, 'maximum depth of the tree = ', maxdepth
  
  ! Uncomment line below to PRINT THE TREE
  ! call execute_command_line('dot -Tpng fort.100 > tree.png')

  call traverse(Root)
  print *, 'ready!!'
  print '(a,3x,i0)', ' different words counted ', wordcount
end program main

