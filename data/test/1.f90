program main
   use aoc_2024
   implicit none
   integer::row,i,res
   integer,allocatable::a(:),b(:)
   row=getrow("data/1.txt")
   allocate(a(row),b(row))
   open(10,file="data/1.txt")
   do i=1,row
      read(10,*)a(i),b(i)
   end do
   ! part 1
   call quicksort(a,0,row-1)
   call quicksort(b,0,row-1)
   print*,sum(abs(a-b))
   ! part 2
   res=0
   do i=1,row
      res=res+a(i)*count(b==a(i))
   end do
   print*,res
end program main
