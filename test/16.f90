program main
   use aoc_2024
   implicit none
   character(len=*),parameter::filename="data/16.txt"
   character,allocatable::a(:,:)
   integer::n,i,is
   type node
      integer::lens
      integer::idx(2)
      integer::mv
   end type node
   integer,parameter::move(2,4)=reshape([-1,0,1,0,0,-1,0,1],[2,4])
   !^ 1 |v 2| < 3| >4
   integer,parameter::rotate(2,4)=reshape([3,4,3,4,1,2,1,2],[2,4])
   integer::idx(2),end(2),minlen,lens
   integer,allocatable::flag(:,:,:)
   integer,allocatable::path(:,:)
   n=getrow(filename)
   allocate(a(n,n))
   allocate(flag(4,n,n))
   allocate(path(n,n))
   open(10,file=filename)
   do i=1,n
      read(10,form("(<>A1)",[n]))a(i,:)
   end do
   close(10)
   ! do i=1,n
      ! print"(*(A1))",a(i,:)
   ! end do
   idx=findloc(a,"S")
   call dijkstra(idx)
   print*,minlen
contains
   subroutine dijkstra(idx)
      use heap_mod
      integer,intent(in)::idx(2)
      type(heap)::m
      type(node)::x
      integer::id,idy(2)
      call m%init(n*n*4,node(0,[0,0],0),eq,cmp,sw)
      call m%insert(node(0,idx,4))
      call m%insert(node(1000,idx,1))
      call m%insert(node(1000,idx,2))
      call m%insert(node(2000,idx,3))
      flag=0
      do while(m%size>0)
         call m%pop(x)
         id=flag(x%mv,x%idx(1),x%idx(2))
         if(id/=0)cycle
         flag(x%mv,x%idx(1),x%idx(2))=1
         idy=x%idx+move(:,x%mv)
         select case(a(idy(1),idy(2)))
         case("#")
         case(".")
            call m%insert(node(x%lens+1,idy,x%mv))
            call m%insert(node(x%lens+1001,idy,rotate(1,x%mv)))
            call m%insert(node(x%lens+1001,idy,rotate(2,x%mv)))
         case("E")
            minlen=x%lens+1
            exit
         end select
      end do
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
