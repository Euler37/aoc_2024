program main
   use aoc_2024
   use iso_fortran_env
   implicit none
   character(len=*),parameter::filename="data/15.txt"
   integer,parameter::move(2,4)=reshape([-1,0,1,0,0,-1,0,1],[2,4])
   character(len=100)::line
   integer::n,i,j,ios,idx(2),res
   character,allocatable::a(:,:)
   character,allocatable::b(:,:)
   character::mv
   type point
      integer::idx(2)
      character::c
   end type point
   type(point),allocatable::path(:)
   logical::flag
   logical,allocatable::tag(:,:)

   open(10,file=filename)
   read(10,"(A)")line
   n=len_trim(line)
   allocate(a(0:n-1,0:n-1))
   allocate(b(0:n-1,0:2*n-1))
   allocate(tag(0:n-1,0:2*n-1))
   close(10)
   open(10,file=filename)
   do i=0,n-1
      read(10,form("(<>A1)",[n]))a(i,:)
   end do
   b=""
   do i=0,n-1
      do j=0,n-1
         if(a(i,j)=="O")then
            b(i,2*j)="["
            b(i,2*j+1)="]"
         else if(a(i,j)=="@")then
            b(i,2*j)="@"
            b(i,2*j+1)="."
         else
            b(i,2*j)=a(i,j)
            b(i,2*j+1)=a(i,j)
         end if
      end do
   end do
   idx=findloc(a,"@")-1
   do
      read(10,"(A1)",advance="no",iostat=ios)mv
      if(is_iostat_end(ios))exit
      if(map(mv)==0)cycle
      call update(idx,map(mv))
   end do
   close(10)
   res=0
   do j=0,n-1
      do i=0,n-1
         if(a(i,j)=="O")res=res+100*i+j
      end do
   end do
   print*,res
   open(10,file=filename)
   do i=0,n-1
      read(10,*)
   end do
   idx=findloc(b,"@")-1
   do
      read(10,"(A1)",advance="no",iostat=ios)mv
      if(is_iostat_end(ios))exit
      if(map(mv)==0)cycle
      call update_part2(idx,map(mv))
   end do
   close(10)
   res=0
   do j=0,2*n-1
      do i=0,n-1
         if(b(i,j)=="[")res=res+100*i+j
      end do
   end do
   print*,res

contains
   pure integer function map(mv)result(res)
      character,intent(in)::mv
      select case(mv)
      case("^");res=1
      case("v");res=2
      case("<");res=3
      case(">");res=4
      case default;res=0
      end select
   end function map

   subroutine update(idx,mv)
      integer,intent(inout)::idx(2)
      integer,intent(in)::mv
      integer::idy(2)
      idy=idx
      do
         idy=idy+move(:,mv)
         select case(a(idy(1),idy(2)))
         case("#"); exit
         case(".")
            a(idy(1),idy(2))="O"
            a(idx(1),idx(2))="."
            idx=idx+move(:,mv)
            a(idx(1),idx(2))="@"
            exit
         end select
      end do
   end subroutine update

   subroutine update_part2(idx,mv)
      integer,intent(inout)::idx(2)
      integer,intent(in)::mv
      integer,parameter::move(2,4)=reshape([-1,0,1,0,0,-1,0,1],[2,4])
      integer::i,idy(2),idz(2)
      if(mv>2)then
         idy=idx
         do
            idy=idy+move(:,mv)
            select case(b(idy(1),idy(2)))
            case("#"); exit
            case(".")
               do
                  if(all(idy==idx))exit
                  idz=idy-move(:,mv)
                  b(idy(1),idy(2))=b(idz(1),idz(2))
                  idy=idy-move(:,mv)
               end do
               b(idx(1),idx(2))="."
               idx=idx+move(:,mv)
               b(idx(1),idx(2))="@"
               exit
            end select
         end do
      else
         idy=idx+move(:,mv)
         path=[point::]
         select case(b(idy(1),idy(2)))
         case("[","]")
            flag=.true.
            call dfs(idy,mv)
            if(.not.flag)return
            tag=.true.
            do i=1,size(path)
               idz=path(i)%idx+move(:,mv)
               b(idz(1),idz(2))=path(i)%c
               tag(idz(1),idz(2))=.false.
            end do
            do i=1,size(path)
               idy=path(i)%idx
               if(tag(idy(1),idy(2))) b(idy(1),idy(2))="."
            end do
            b(idx(1),idx(2))="."
            idx=idx+move(:,mv)
            b(idx(1),idx(2))="@"
         case(".")
            b(idx(1),idx(2))="."
            idx=idx+move(:,mv)
            b(idx(1),idx(2))="@"
         end select
      end if
   end subroutine update_part2

   recursive subroutine dfs(idx,mv)
      integer,intent(in)::idx(2)
      integer,intent(in)::mv
      select case(b(idx(1),idx(2)))
      case("#")
         flag=.false.
         return
      case("[")
         path=[path,point(idx,'['),point(idx+[0,1],']')]
         call dfs(idx+move(:,mv),mv)
         call dfs(idx+[0,1]+move(:,mv),mv)
      case("]")
         path=[path,point(idx,']'),point(idx-[0,1],'[')]
         call dfs(idx+move(:,mv),mv)
         call dfs(idx-[0,1]+move(:,mv),mv)
      case("."); return
      end select
   end subroutine dfs
end program main
