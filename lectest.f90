program main
  implicit none

  character(len=80) :: a, b, arg, filename
  character(len=1)   :: tmp='0'
  character(len=:), allocatable :: f_a, f_b
  integer            :: i, iarg, ios, size
  logical            :: first=.true., last=.false.  

  call get_command_argument(1, arg)
  read(arg, *) filename
  
  open(unit=1, file=filename, iostat=ios, status='old')
  if(ios/=0) then
    print '(a,a,3x,i0)', '***Error in opening file: ', trim(filename), ios
    stop
  end if

! looping through words in string  
  do
    a = ''    
20  do 
      read(1,'(a)', ADVANCE='NO', END=40, EOR=20, IOSTAT=ios) tmp
    
      ! check for reading errors !
      if(ios > 0) then
        print *,'***Error: ', ios
        stop
      end if

      ! if string break exiting reading loop !
      if(iachar(tmp) == 9 .or. iachar(tmp) == 10 .or. iachar(tmp) == 32 .or. iachar(tmp) == 44 &
              .or. iachar(tmp) == 46 ) then
!print *, 'sanan loppu'
        exit
      end if
!print *,'tmp = ',tmp 
      a = trim(a)//trim(tmp)
!print *,'a = ',a 
    end do
  
40  if(ios==-1 .or. ios == -2 ) then
      last = .true.
    end if

    f_a = trim(a)
  
    if(first .eqv. .true.) then
      first = .false.
      f_b = f_a
      cycle 
    end if

    if(llt(f_a, f_b)) then
    print *, 'ensin ',f_a,' sitten ', f_b
  else if(lgt(f_a,f_b)) then
    print *, 'ensin ',f_b,' sitten ', f_a
  else
    print *, f_b,' sama kuin ', f_a
  endif
  
  if(last) then
    exit
  end if

  f_b=f_a
end do 
end program main

