program main
   use aoc_2024
   use string_mod
   implicit none
   character(len=*),parameter::filename="data/8.txt"
   character(len=:),allocatable::s
   character(len=1),pointer::ps(:,:)
   integer::n,i,idx(2),j,k
   integer,allocatable::flag(:,:)
   type point
      integer::a(2)
   end type point
   type(point),allocatable::p(:)
   logical,allocatable::antenna(:,:)

   n=getrow(filename)
   allocate(character(len=n*n)::s)
   ps=>string_view2d(s,n,n)
   allocate(flag(n,n),source=0)
   allocate(antenna(n,n),source=.true.)
   open(10,file=filename)
   do i=1,n
      read(10,form("(<>A1)",[n]))ps(i,:)
   end do
   close(10)
   !0 none
   !1 #
   !part 1
   do j=1,n
      do i=1,n
         if(ps(i,j)==".")cycle
         if(.not.antenna(i,j))cycle
         p=[point([i,j])]
         antenna(i,j)=.false.
         do
            idx= findloc(ps,ps(i,j),mask=antenna)
            if(all(idx==0))exit
            p=[p,point(idx)]
            antenna(idx(1),idx(2))=.false.
         end do
         call antinode()
      end do
   end do
   print*,count(flag==1)
   !part 2
   antenna=.true.
   flag=0
   do j=1,n
      do i=1,n
         if(ps(i,j)==".")cycle
         if(.not.antenna(i,j))cycle
         p=[point([i,j])]
         antenna(i,j)=.false.
         do
            idx= findloc(ps,ps(i,j),mask=antenna)
            if(all(idx==0))exit
            p=[p,point(idx)]
            antenna(idx(1),idx(2))=.false.
         end do
         call resonant_antinode()
      end do
   end do
   print*,count(flag==1)
contains
   subroutine antinode()
      integer::m
      integer::i,j
      integer::j1(2)
      m=size(p)
      if(m==1)return
      do i=1,m
         do j=i+1,m
            associate(i1=>p(i)%a,i2=>p(j)%a)
            !j1   i1    i2    j2
            !(i2+j1)/2 = i1
            !(i1+j2)/2 = i2
            j1=i1+(i1-i2)
            if(all(j1<=n.and.j1>=1)) flag(j1(1),j1(2))=1
            j1=i2+(i2-i1)
            if(all(j1<=n.and.j1>=1)) flag(j1(1),j1(2))=1
            end associate
         end do
      end do
   end subroutine antinode
   subroutine resonant_antinode()
      integer::m
      integer::i,j,k
      integer::j1(2)
      m=size(p)
      if(m==1)return
      do i=1,m
         do j=i+1,m
            associate(i1=>p(i)%a,i2=>p(j)%a)
            k=0
            do
               j1=i1+k*(i1-i2)
               if(.not.all(j1<=n.and.j1>=1)) exit
               flag(j1(1),j1(2))=1
               k=k+1
            end do
            k=0
            do
               j1=i2+k*(i2-i1)
               if(.not.all(j1<=n.and.j1>=1)) exit
               flag(j1(1),j1(2))=1
               k=k+1
            end do
            end associate
        end do
     end do
  end subroutine resonant_antinode
end program main
