! functions to read one string from file, to lower case and to compare strings !
module word_collection
  use node
  implicit none

contains

  ! picks up single characters from io to a temperament string in loop till end of string !
  ! EOS are comma, period, space, tab or linefeed in ASCII !
  ! result is trimmed temperament string !
  function read_one() result(word)
    character(len=1)              :: tmp='0'
    character(len=80)             :: a
    character(len=:), allocatable :: word
    integer                       :: ios
  
10    a = ''
    do 
      read(1,'(a)', ADVANCE='NO', IOSTAT=ios) tmp
    
      ! check for reading errors !
      if(ios > 0) then
        print *,'***Error: ', ios
        stop
      end if

      ! on string's end exiting reading loop !
      if(iachar(tmp) == 9 .or. iachar(tmp) == 10 .or. iachar(tmp) == 32 .or. iachar(tmp) == 44 &
              .or. iachar(tmp) == 46 .or. ios < 0) then

        if(len_trim(a) == 0 .and. ios /= -1) then
          goto 10
        end if

        exit
      end if
      
      a = trim(a)//trim(tmp)
    end do
     
    word = trim(a)

  end function read_one
  
  ! function recognizes only english and finish leters and turns cap letters to lower case !
  function to_lower (str) result (str_low)
    character(*), Intent(In) :: str
    character(len(str))      :: str_low
    integer :: ic, i

    character(29), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÅ'
    character(29), parameter :: low = 'abcdefghijklmnopqrstuvwxyzäöå'

    str_low = str
    do i = 1, len_trim(str)
        ic = index(cap, str(i:i))
        if (ic > 0) str_low(i:i) = low(ic:ic)
    end do

end function to_lower

  ! compares lexically two strings given as parameters; !
  ! returns -1 if first is "smaller", 0 if same strings, 1 if second is "smaller" ! 
  integer function compare(a, b)
    character(len=80), intent(IN) :: a, b

    if(llt(a, b)) then
      ! print *, 'ensin ',a,' sitten ', b
      compare = (-1)
    else if(lgt(a, b)) then
      ! print *, 'ensin ',b,' sitten ', a
      compare = 1
    else
      ! print *, b,' sama kuin ', a
      compare = 0
    end if
  end function compare

end module word_collection
