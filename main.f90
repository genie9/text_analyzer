program main
  use word_collection
  implicit none

  character(len=80) :: arg, filename
  character(len=:), allocatable :: f_a, f_b
  integer            :: iarg, ios

  call get_command_argument(1, arg)
  read(arg, *) filename
  
  open(unit=1, file=filename, iostat=ios, status='old')
  if(ios/=0) then
    print '(a,a,3x,i0)', '***Error in opening file: ', trim(filename), ios
    stop
  end if

  do
    f_a = read_one()
    f_a = to_lower(f_a)
    if(f_a == '') then
      exit
    end if

    print *, f_a
  end do
  
  print *, 'ready!!'

end program main

