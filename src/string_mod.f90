module string_mod
   implicit none
   type string
      character(len=:),allocatable::str
   contains
      generic::assignment(=)=>string_construct
      procedure,pass::print=>print_string
      procedure,pass::string_construct
      procedure,pass::size=>size_string
      final::final_string
   end type string

   interface split
      module procedure split_str
      module procedure split_string
   end interface

   interface operator(.in.)
      module procedure instrarray
      module procedure instringarray
   end interface

   interface len
      module procedure len_string
   end interface

contains
   integer elemental function len_string(str)result(res)
      type(string),intent(in)::str
      res=len(str%str)
   end function len_string

   logical function instrarray(str,arr)result(res)
      character(len=*),intent(in)::str
      type(string),intent(in)::arr(:)
      integer::i
      res=.false.
      do i=1,size(arr)
         if(arr(i)%str==str)then
            res=.true.
            exit
         end if
      end do
   end function instrarray

   logical function instringarray(str,arr)result(res)
      type(string),intent(in)::str
      type(string),intent(in)::arr(:)
      res=instrarray(str%str,arr)
   end function instringarray

   elemental integer function size_string(this)result(res)
      class(string),intent(in)::this
      res=len(this%str)
   end function size_string

   subroutine string_construct(this,str)
      class(string),intent(inout)::this
      character(len=*),intent(in)::str
      this%str=str
   end subroutine string_construct

   subroutine split_string(str,sep,res,skip)
      type(string),intent(in)::str
      character(len=*),intent(in)::sep
      type(string),allocatable::res(:)
      logical,intent(in)::skip
      call split(str%str,sep,res,skip)
   end subroutine split_string

   subroutine split_str(str,sep,res,skip)
      character(len=*),intent(in)::str
      character(len=*),intent(in)::sep
      logical,intent(in)::skip
      type(string),allocatable,intent(out)::res(:)
      integer::start,end,l,ls,i,num
      start=1
      end=1
      l=len(sep)
      ls=len_trim(str)
      num=0
      do
         start=end
         if(skip)then
            do i=end,ls,l
               if(str(i:i+l-1)/=sep)exit
               start=start+l
            end do
         end if
         end=index(str(start:),sep)+start
         if(end==start)exit
         num=num+1
      end do
      if(ls>=start)num=num+1
      allocate(res(num))
      num=0
      start=1
      end=1
      do
         start=end
         if(skip)then
            do i=end,ls,l
               if(str(i:i+l-1)/=sep)exit
                start=start+l
            end do
         end if
         end=index(str(start:),sep)
         if(end==0)exit
         end=end+start-1
         num=num+1
         res(num)=str(start:end-1)
         end=end+l
      end do
      if(ls>=start)then
         num=num+1
         res(num)=str(start:ls)
      end if
   end subroutine split_str

   impure elemental subroutine print_string(this)
      class(string),intent(in)::this
      write(*,*,delim="quote")this%str
   end subroutine print_string

   elemental subroutine final_string(this)
      type(string),intent(inout)::this
      if(allocated(this%str))deallocate(this%str)
   end subroutine final_string

   function string_view(s,n)result(res)
      character(len=1),intent(in),target::s(*)
      integer,intent(in)::n
      character(len=1),pointer::res(:)
      res(1:n)=>s(1:n)
   end function string_view

   function string_view2d(s,n,m)result(res)
      character(len=1),intent(in),target::s(*)
      integer,intent(in)::n,m
      character(len=1),pointer::res(:,:)
      res(1:n,1:m)=>s(1:n*m)
   end function string_view2d
end module string_mod
