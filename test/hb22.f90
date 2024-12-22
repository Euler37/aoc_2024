program main
   use aoc_2024
   use hashbit_mod
   use iso_fortran_env
   implicit none
   integer(8)::res,num
   integer::ios,i,j
   integer::n
   integer,allocatable::diff(:,:)
   integer,allocatable::price(:,:)
   integer::val
   type(hashbit)::hb
   integer,allocatable::h(:,:,:,:)
   n=getrow("data/22.txt")
   allocate(diff(2000,n))
   allocate(price(2000,n))
   allocate(h(-9:9,-9:9,-9:9,-9:9))
   open(10,file="data/22.txt")
   res=0
   do j=1,n
      read(10,*)num
      do i=1,2000
         diff(i,j)=modulo(num,10_8)
         num=next_num(num)
         diff(i,j)=modulo(num,10_8)-diff(i,j)
         price(i,j)=modulo(num,10_8)
      end do
      res=res+num
   end do
   print*,res
   close(10)
   h=0
   do j=1,n
      call hb%init([integer(8)::19,19,19,19])
      do i=1,1996
         associate(k=>diff(i:i+3,j))
            if(hb%get(k+9_8))cycle
            h(k(1),k(2),k(3),k(4))=h(k(1),k(2),k(3),k(4))+price(i+3,j)
            call hb%set(k+9_8)
         end associate
      end do
      call hb%clean()
   end do
   print*,maxval(h)
contains
   integer(8) function next_num(a)result(res)
      integer(8),value::a
      a=modulo(ieor(a,a*64_8),16777216_8)
      a=modulo(ieor(a,a/32_8),16777216_8)
      res=modulo(ieor(a,a*2048_8),16777216_8)
   end function next_num

end program main
