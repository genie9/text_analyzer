! functions to read one string from file and to write one to file, to lower case and to compare strings !
module word_collection
  use node_def
  implicit none

contains

  ! Picks up single characters from io to a temperament string in loop till end of string.
  ! EOS are comma, period, exclamation and question marks, space, tab or linefeed in ASCII.
  ! Result is trimmed from temperament string.
  function read_one() result(word)
    character(len=1)              :: tmp='0'
    character(len=80)             :: a
    character(len=:), allocatable :: word
    integer                       :: ios
  
10  a = ''
    do 
      read(1,'(a)', ADVANCE='NO', IOSTAT=ios) tmp
    
      ! Checking for reading errors
      if(ios > 0) then
        print *,'***Error reading from file: ', ios
        stop
      end if

      ! On string's ending character exiting reading loop
      if(iachar(tmp) == 9 .or. iachar(tmp) == 10 .or. iachar(tmp) == 32 .or. iachar(tmp) == 39 .or. iachar(tmp) == 38 &
              .or. (scan(tmp, '!"/()=?|{[]}\,.;:<>+%^') /= 0) .or. ios < 0) then

        ! Avoiding production of an empty strings resulting from consecutive not wanted charachters
        if(len_trim(a) == 0 .and. ios /= -1) then
          goto 10
        end if

        exit
      end if
      
      a = trim(a)//trim(tmp)
    end do
     
    word = trim(a)

  end function read_one
  
  ! Function recognizes only english and finish leters and turns cap letters to lower case
  function to_lower (str) result (str_low)
    character(*), Intent(IN) :: str
    character(len(str))      :: str_low
    integer                  :: ic, i

    character(32), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÅŒÆØ'
    character(32), parameter :: low = 'abcdefghijklmnopqrstuvwxyzäöåœæø'

    str_low = str
    do i = 1, len_trim(str)
        ic = index(cap, str(i:i))
        if (ic > 0) str_low(i:i) = low(ic:ic)
    end do

end function to_lower

  ! Compares lexically two strings given as parameters;
  ! Function returns -1 if first is "smaller", 0 if same strings, 1 if second is "smaller"
  integer function compare(a, b)
    character(*), intent(IN) :: a, b

    if(llt(a, b)) then
      compare = (-1)
    else if(lgt(a, b)) then
      compare = 1
    else
      compare = 0
    end if
  end function compare

  ! Soubroutine to print word and it's count into a file.
  subroutine record(node_p)
    type (Node) :: node_p
    integer     :: ios

    write(2, '(i5,4x,a)', IOSTAT=ios) node_p%iword, trim(node_p%word)
      
    ! Check for writing errors 
    if(ios > 0) then
      print *,'***Error reading from file: ', ios
      stop
    end if
  end subroutine record

end module word_collection
