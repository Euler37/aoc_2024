program main
   use aoc_2024
   implicit none
   integer,parameter::n=70
   integer::a(-1:n+1,-1:n+1)
   integer::x,y,i
   integer,parameter::move(2,4)=reshape([-1,0,1,0,0,-1,0,1],[2,4])
   integer,parameter::rotate(2,4)=reshape([3,4,3,4,1,2,1,2],[2,4])
   integer::flag(4,-1:n+1,-1:n+1),minlen,ios,j
   type node
      integer::lens
      integer::idx(2)
      integer::mv
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
      integer::id,idy(2)
      call m%init(n*n*4,node(0,[0,0],0),eq,cmp,sw)
      call m%insert(node(0,idx,1))
      call m%insert(node(0,idx,2))
      call m%insert(node(0,idx,3))
      call m%insert(node(0,idx,4))
      flag=0
      do while(m%size>0)
         call m%pop(x)
         id=flag(x%mv,x%idx(1),x%idx(2))
         if(id/=0)cycle
         flag(x%mv,x%idx(1),x%idx(2))=1
         idy=x%idx+move(:,x%mv)
         select case(a(idy(1),idy(2)))
         case(1)
         case(0)
            call m%insert(node(x%lens+1,idy,x%mv))
            call m%insert(node(x%lens+1,idy,rotate(1,x%mv)))
            call m%insert(node(x%lens+1,idy,rotate(2,x%mv)))
         case(2)
            minlen=x%lens+1
            exit
         end select
      end do
      call m%clean()
   end subroutine dijkstra

   subroutine eq(a,b)
      class(*),intent(inout)::a
      class(*),intent(in)::b
      select type(a); class is (node)
      select type(b); class is (node)
      a%lens=b%lens
      a%idx =b%idx
      a%mv=b%mv
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
