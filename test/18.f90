program main
   use aoc_2024
   implicit none
   integer,parameter::n=70
   integer::a(-1:n+1,-1:n+1)
   integer::x,y,i
   integer,parameter::move(2,4)=reshape([-1,0,1,0,0,-1,0,1],[2,4])
   integer::flag(-1:n+1,-1:n+1),minlen,ios,j
   type node
      integer::lens
      integer::idx(2)
   end type node
   a=0
   a(-1,-1:n+1)=1
   a(-1:n+1,-1)=1
   a(n+1,-1:n+1)=1
   a(-1:n+1,n+1)=1
   a(n,n)=2
   flag=0
   open(10,file="data/18.txt")
   do i=1,1024
      read(10,*)x,y
      a(x,y)=1
   end do
   call dijkstra([0,0])
   print*,minlen
   j=1024
   do
      j=j+1
      ! print*,j
      read(10,*,iostat=ios)x,y
      if(ios/=0)exit
      a(x,y)=1
      minlen=0
      call dijkstra([0,0])
      if(minlen==0)then
         print*,x,y
         exit
      end if
   end do
contains
   subroutine dijkstra(idx)
      use heap_mod
      integer,intent(in)::idx(2)
      type(heap)::m
      type(node)::x
      integer::i,idy(2)
      call m%init(n*n*4,node(0,[0,0]),eq,cmp,sw)
      call m%insert(node(0,idx))
      flag=0
      outer:do while(m%size>0)
         call m%pop(x)
         if(flag(x%idx(1),x%idx(2))/=0)cycle
         flag(x%idx(1),x%idx(2))=1
         do i=1,4
            idy=x%idx+move(:,i)
            if(flag(idy(1),idy(2))/=0)cycle
            select case(a(idy(1),idy(2)))
            case(1)
            case(0)
               call m%insert(node(x%lens+1,idy))
            case(2)
               minlen=x%lens+1
               exit outer
            end select
         end do
      end do outer
      call m%clean()
   end subroutine dijkstra

   subroutine eq(a,b)
      class(*),intent(inout)::a
      class(*),intent(in)::b
      select type(a); class is (node)
      select type(b); class is (node)
      a%lens=b%lens
      a%idx =b%idx
      end select; end select 
   end subroutine eq

   subroutine sw(a,b)
      class(*),intent(inout)::a,b
      type(node)::tmp
      call eq(tmp,b)
      call eq(b,a)
      call eq(a,tmp)
   end subroutine sw

   logical function cmp(a,b)result(res)
      class(*),intent(in)::a,b
      select type(a); class is (node)
      select type(b); class is (node)
      res=a%lens<b%lens
      end select; end select 
   end function cmp

end program main
