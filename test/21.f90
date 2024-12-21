program main
   use aoc_2024
   use hash_mod
   use string_mod
   implicit none
   type tuple
      sequence
      integer::ix,iy
   end type tuple

   type node
      sequence
      character(len=8)::code
      integer::depth
      integer::lim
   end type node
   character(len=4)::str
   integer(8)::res,res2
   integer::path,i
   type(hashmap)::seen
   type(hashmap)::dd,padd

   open(10,file="data/21.txt")
   call init_pad()
   call seen%init(eq,bit)
   res=0
   res2=0
   do i=1,5
      read(10,"(A)")str
      res=res+shortest(str,2,0)*tonum(str(1:3))
      res2=res2+shortest(str,25,0)*tonum(str(1:3))
   end do
   print*,res
   print*,res2
   call seen%clean()
   call padd%clean()
   call dd%clean()
contains
   recursive integer(8) function shortest(code,lim,depth)result(res)
      character(len=*),intent(in)::code
      integer,intent(in)::lim,depth
      type(node)::key
      logical::first
      integer::x,y,i
      type(tuple)::dxy
      type(tuple)::xy
      character(len=8)::moves(2)
      character,pointer::ptr(:)
      character::c
      res=huge(1_8)
      if(code=="")return
      key=node(code,depth,lim)
      if(key .in. seen)then
         call seen%get(key,res)
         return
      end if
      xy=merge(tuple(2,3),tuple(2,0),depth==0)
      ptr=>string_view(code,len_trim(code))
      res=0
      do i=1,size(ptr)
         call padd%get(ptr(i),dxy)
         moves=moves_set(xy,dxy,merge(3,0,depth==0))
         if(depth==lim)then
            res=res+len_trim(moves(1))
         else
            res=res+min(shortest(moves(1),lim,depth+1),shortest(moves(2),lim,depth+1))
         end if
         xy=dxy
      end do
      call seen%append(key,res)
   end function shortest

   function moves_set(xy,dxy,robs)result(res)
      type(tuple),intent(in)::xy,dxy
      integer,intent(in)::robs
      integer::ax,ay,ix,iy
      character(len=8)::res(2)
      character::c
      ! print*,xy,dxy,robs
      ax=merge(-1,1,dxy%ix<xy%ix)
      ay=merge(-1,1,dxy%iy<xy%iy)
      res=""
      if(xy%ix/=dxy%ix .or. xy%iy /= dxy%iy)then
         ix=xy%ix
         iy=xy%iy
         do while(ix /= dxy%ix)
            if(ix+ax==0 .and. iy==robs)exit
            ix=ix+ax
            call dd%get(tuple(ax,0),c)
            res(1)=trim(res(1))//c
         end do
         do while(iy /= dxy%iy)
            iy=iy+ay
            call dd%get(tuple(0,ay),c)
            res(1)=trim(res(1))//c
         end do
         do while(ix /= dxy%ix)
            ix=ix+ax
            call dd%get(tuple(ax,0),c)
            res(1)=trim(res(1))//c
         end do
         res(1)=trim(res(1))//"a"

         ix=xy%ix
         iy=xy%iy
         do while(iy /= dxy%iy)
            if(ix==0 .and. iy+ay==robs)exit
            iy=iy+ay
            call dd%get(tuple(0,ay),c)
            res(2)=trim(res(2))//c
         end do
         do while(ix /= dxy%ix)
            ix=ix+ax
            call dd%get(tuple(ax,0),c)
            res(2)=trim(res(2))//c
         end do
         do while(iy /= dxy%iy)
            iy=iy+ay
            call dd%get(tuple(0,ay),c)
            res(2)=trim(res(2))//c
         end do
         res(2)=trim(res(2))//"a"
      else
         res(1)="a"
      end if
   end function moves_set

   subroutine eq(a,b)
      class(*),intent(inout)::a
      class(*),intent(in)::b
      select type(a); type is (integer)
      select type(b); type is (integer)
      a=b
      end select; end select 
      select type(a); type is (integer(8))
      select type(b); type is (integer(8))
      a=b
      end select; end select 

      select type(a); type is (character(len=*))
      select type(b); type is (character(len=*))
      a=b
      end select; end select 
      
      select type(a); class is (tuple)
      select type(b); class is (tuple)
      a%ix=b%ix
      a%iy=b%iy
      end select; end select 

      select type(a); class is (node)
      select type(b); class is (node)
      a%code=b%code
      a%depth=b%depth
      a%lim=b%lim
      end select; end select 
   end subroutine eq

   subroutine bit(a,b)
      use iso_c_binding
      class(*),intent(in),target::a
      integer(1),intent(inout),pointer::b(:)
      select type(a)
      type is (integer)          ; call c_f_pointer(c_loc(a),b,[4])
      type is (real)             ; call c_f_pointer(c_loc(a),b,[4])
      type is (character(len=*)) ; call c_f_pointer(c_loc(a),b,[len(a)])
      class is (tuple)           ; call c_f_pointer(c_loc(a%ix),b,[8])
      class is (node)            ; call c_f_pointer(c_loc(a%code),b,[16])
      end select
   end subroutine bit

   subroutine init_pad()
      call dd%init(eq,bit)
      call dd%append(tuple(1 ,0 )  , ">")
      call dd%append(tuple(-1,0 )  , "<")
      call dd%append(tuple(0 ,1 )  , "v")
      call dd%append(tuple(0 ,-1)  , "^")
      call padd%init(eq,bit)
      call padd%append('7',tuple(0,0))
      call padd%append('8',tuple(1,0))
      call padd%append('9',tuple(2,0))
      call padd%append('4',tuple(0,1))
      call padd%append('5',tuple(1,1))
      call padd%append('6',tuple(2,1))
      call padd%append('1',tuple(0,2))
      call padd%append('2',tuple(1,2))
      call padd%append('3',tuple(2,2))
      call padd%append('0',tuple(1,3))
      call padd%append('A',tuple(2,3))
      call padd%append('a',tuple(2,0))
      call padd%append('^',tuple(1,0))
      call padd%append('<',tuple(0,1))
      call padd%append('v',tuple(1,1))
      call padd%append('>',tuple(2,1))
   end subroutine init_pad
end program main
