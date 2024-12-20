program main
   use aoc_2024
   implicit none
   integer::x,y,i
   integer,parameter::move(2,4)=reshape([-1,0,1,0,0,-1,0,1],[2,4])
   integer,allocatable::flag(:,:)
   character,allocatable::a(:,:)
   character(len=*),parameter::filename="data/20.txt"
   integer::minlen,ios,j,n,minlen0,res
   type node
      integer::lens
      integer::idx(2)
   end type node
   character(len=200)::str
   integer::start(2)
   open(10,file=filename)
   read(10,"(A)")str
   close(10)
   n=len_trim(str)
   allocate(a(n,n))
   allocate(flag(n,n))
   flag=0
   open(10,file=filename)
   do i=1,n
      read(10,form("(<>A1)",[n]))a(i,:)
   end do
   start=findloc(a,"S")
   call dijkstra(start)
   minlen0=minlen
   res=0
   do i=2,n-1
     do j=2,n-1
        if(a(j,i)=="#".and.countnum(j,i)<=2)then
           a(j,i)="."
           call dijkstra(start)
           if(minlen0-minlen>=100) res=res+1
           a(j,i)="#"
        end if
     end do
   end do
   print*,res
contains
   integer function countnum(j,i)result(res)
      integer,intent(in)::i,j
      res=count([a(j+1,i),a(j-1,i),a(j,i+1),a(j,i-1)]=="#")
   end function countnum

   subroutine dijkstra(idx)
      use heap_mod
      integer,intent(in)::idx(2)
      type(heap)::m
      type(node)::x
      integer::id,idy(2),i
      call m%init(n*n,node(0,[0,0]),eq,cmp,sw)
      call m%insert(node(0,idx))
      flag=0
      outer:do while(m%size>0)
         call m%pop(x)
         id=flag(x%idx(1),x%idx(2))
         if(id/=0)cycle
         flag(x%idx(1),x%idx(2))=1
         do i=1,4
            idy=x%idx+move(:,i)
            if(flag(idy(1),idy(2))/=0)cycle
            select case(a(idy(1),idy(2)))
            case("#")
            case(".")
               call m%insert(node(x%lens+1,idy))
            case("E")
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
