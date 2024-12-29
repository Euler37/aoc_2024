module aoc_2024
  implicit none
  interface tostring
     module procedure tostring32
     module procedure tostring64
  end interface

  interface gcd
    module procedure gcd_i4
    module procedure gcd_i8
  end interface

  interface lcm
    module procedure lcm_i4
    module procedure lcm_i8
  end interface
contains
   subroutine replace(str,b,c)
      character(len=*),intent(inout)::str
      character,intent(in)::b,c
      integer::i
      do i=1,len_trim(str)
         if(str(i:i)==b) str(i:i)=c
      end do
   end subroutine replace

   logical function isspace(str)result(res)
      character,intent(in)::str
      res=any(ichar(str)==[32,9,10,11,12,13])
   end function isspace
   
   integer function strtol(str,p)result(res)
      character(len=*),intent(in)::str
      integer,intent(inout)::p
      integer::i
      res=0
      do
         if(str(p:p)=="-".and.p+1<=len(str))then
            if(isdigit(str(p+1:p+1)))exit
         end if
         if(isdigit(str(p:p)))exit
         p=p+1
         if(p>len(str))return
      end do
      ! find pos
      i=p
      p=p+1
      do
         if(.not.isdigit(str(p:p)))exit
         p=p+1
         if(p>len(str))exit
      end do
      res=tonum(str(i:p-1))
   end function strtol


   pure function tostring32(value)result(res)
      integer(4),intent(in)      :: value
      character(len=:),allocatable :: res
      integer(4),parameter       :: buffer_len=range(value)+2
      character(len=buffer_len)    :: buffer
      integer(4)                 :: pos,n
      character(len=1),parameter   :: numbers(0:9)=["0","1","2","3","4","5","6","7","8","9"]
      if(value == 0)then
         res = numbers(0)
         return
      end if
      n=abs(value)
      buffer=""
      pos=buffer_len+1
      do while(n > 0)
         pos=pos-1
         buffer(pos:pos)=numbers(mod(n,10))
         n=n/10
      end do
      if(value < 0)then
         pos=pos-1
         buffer(pos:pos)="-"
      end if
      res=buffer(pos:)
   end function tostring32

   pure function tostring64(value)result(res)
      integer(8),intent(in)      :: value
      character(len=:),allocatable :: res
      integer(4),parameter       :: buffer_len=range(value)+2
      character(len=buffer_len)  :: buffer
      integer(4)                 :: pos
      integer(8)                 ::n
      character(len=1),parameter   :: numbers(0:9)=["0","1","2","3","4","5","6","7","8","9"]
      if(value == 0)then
         res = numbers(0)
         return
      end if
      n=abs(value)
      buffer=""
      pos=buffer_len+1
      do while(n > 0)
         pos=pos-1
         buffer(pos:pos)=numbers(mod(n,10))
         n=n/10
      end do
      if(value < 0)then
         pos=pos-1
         buffer(pos:pos)="-"
      end if
      res=buffer(pos:)
   end function tostring64

    function form(format,nums)result(res)
        character(len=*),intent(in)::format
        integer,intent(in)::nums(:)
        character(len=:),allocatable::res
        integer::idx,i
        res=format
        i=0
        do
            idx=index(res,"<>")
            if(idx==0)exit
            i=i+1
            if(i>size(nums))error stop "The size of array 'nums' less than the number of '<>'"
            res=res(:idx-1)//tostring(nums(i))//res(idx+2:)        
        end do
    end function form

   !> description : convert string to integer(4)
   !>
   !> example     : tonum('123') => 123
   !> example     : tonum('-123') => -123
   function tonum(str)result(res)
      character(len=*),intent(in)::str
      integer(4)::res
      character(len=len_trim(adjustl(str)))::str_trim
      integer(4)::i,lens,sign1
      str_trim=trim(adjustl(str))
      lens=len(str_trim)
      res=0
      sign1=merge(-1,1,str_trim(1:1)=="-")
      do i=lens,merge(2,1,sign1==-1),-1
         res=res+(ichar(str_trim(i:i))-ichar("0"))*10**(lens-i)
      end do
      res=res*sign1
   end function tonum


   function getrow(filename) result(res)
      character(len=*),intent(in)::filename
      integer(kind=4)::res
      integer(kind=4)::unit,ios
      character(len=1)::c
      open(newunit=unit,file=filename)
      res=0
      do
         read(unit,"(A)",iostat=ios)c
         if(ios/=0)exit
         res=res+1
      end do
      close(unit)
   end function getrow

   !> description : get row number from string
   !> same as fortran split tokens
   !>
   !> example     : getrownum("abcdefg") => 1
   !> example     : getrownum("abcdefg abc") => 2
   function getcolnum(str) result(res)
      character(len=*),intent(in)::str
      integer(kind=4)::res
      integer(kind=4)::i,j,ios
      character(len=1)::c
      res=1
      do i=1,len_trim(str)
         read(str,*,iostat=ios)(c,j=1,i)
         if(ios/=0)then
            res=i-1
            exit
         end if
      end do
   end function getcolnum

   !> description : check if character is digit
   !>
   !> example     : isdigit('1') => true
   !> example     : isdigit('a') => false
   elemental function isdigit(s)result(res)
      character(len=1),intent(in)::s
      logical(kind=4)::res
      res=s >="0".and. s<="9"
   end function isdigit

   !> description : get time
   !> 
   !> example     : tic = clock()
   !> example     : spend = clock() - tic
   function clock()result(res)
      real(kind=8)::res
      integer(kind=8)::tic
      integer(kind=8)::rate
      call system_clock(tic,count_rate=rate)
      res=real(tic,8)/rate
   end function clock

    elemental integer function lcm_i4(m,n)result(r)
       integer,value::m,n
       r=m/gcd(m,n)*n
    end function lcm_i4
    
    elemental integer(8) function lcm_i8(m,n)result(r)
       integer(8),value::m,n
       r=m/gcd(m,n)*n
    end function lcm_i8

   recursive elemental integer function gcd_i4(m,n)result(r)
      integer,value,intent(in)::m,n
      r=m
      if(n==0)return
      r=gcd_i4(n,mod(m,n))
   end function gcd_i4

  recursive elemental integer(8) function gcd_i8(m,n)result(r)
      integer(8),value,intent(in)::m,n
      r=m
      if(n==0)return
      r=gcd_i8(n,mod(m,n))
   end function gcd_i8

    logical function next_permutation(a)result(found)
        character(len=1),intent(inout)::a(:)
        integer::i,j
        found=.false.
        associate(n=>size(a))
            do i=n-1,1,-1
               if(a(i)<a(i+1))then
                  found=.true.
                  exit
               end if
            end do
            if(.not.found) then
                a=a(n:1:-1)
                return
            end if
            do j=n,i+1,-1
               if(a(i)<a(j))exit
            end do
            a([i,j])=a([j,i])
            a(i+1:n)=a(n:i+1:-1)
        end associate
    end function next_permutation

    integer function partition(arr,low,high)result(res)
       integer,intent(inout)::arr(0:)
       integer,intent(in)::low,high
       integer::i,pivot,j,temp
       i=low-1
       pivot=arr(high)
       do j=low,high-1
          if(arr(j)<pivot)then
             i=i+1
             temp=arr(i)
             arr(i)=arr(j)
             arr(j)=temp
          end if
       end do
       temp=arr(i+1)
       arr(i+1)=arr(high)
       arr(high)=temp
       res=i+1
    end function partition

    recursive subroutine quicksort(arr,first,last)
       integer,intent(inout)::arr(0:)
       integer,intent(in)::first,last
       integer::pivot_idx
       if(first < last)then
          pivot_idx=partition(arr,first,last)
          call quicksort(arr,first,pivot_idx-1)
          call quicksort(arr,pivot_idx+1,last)
       end if
    end subroutine quicksort

    subroutine getfile(filename,str)
       character(len=*),intent(in)::filename
       character(len=:),allocatable::str
       integer::unit,length
       open (newunit=unit, file=filename, status='old', action='read', &
          access='stream', form='unformatted')
       inquire (unit, size=length)
       allocate(character(len=length)::str)
       read(unit)str
       close(unit)
    end subroutine getfile
 end module aoc_2024
