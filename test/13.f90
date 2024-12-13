program main
   use aoc_2024
   implicit none
   character(len=100)::str
   integer::ios
   integer(8)::ab(2,2),xy(2),res,res2,xy2(2)
   open(10,file="data/13.txt")
   res=0
   do
      str=""
      read(10,"(A)")str
      str=str(13:)
      call replace(str,"Y"," "); call replace(str,"+"," ")
      read(str,*)ab(:,1)
      read(10,"(A)")str
      str=str(13:)
      call replace(str,"Y"," "); call replace(str,"+"," ")
      read(str,*)ab(:,2)
      read(10,"(A)")str
      str=str(10:)
      call replace(str,"Y"," "); call replace(str,"="," ")
      read(str,*)xy
      xy2=10000000000000_8+xy
      call solve(ab,xy)
      res =res+sum(xy*[3,1])
      call solve(ab,xy2)
      res2=res2+sum(xy2*[3,1])
      read(10,*,iostat=ios)
      if(ios/=0)exit
   end do
   print*,res
   print*,res2
   close(10)
contains
   subroutine solve(ab,xy)
      integer(8),intent(inout)::ab(:,:),xy(:)
      integer(8)::det,r(2)
      ! X(1) X(2) A = 13478
      ! Y(1) Y(2) B = 4424
      ! if ab      , 1 solve
      ! if singular , inf solve ,find the min
      ! a b    d -b
      ! c d   -c  a
      det=ab(1,1)*ab(2,2)-ab(1,2)*ab(2,1)
      if(det==0_8)error stop "Singular"
      r(1)= ab(2,2)*xy(1)-ab(1,2)*xy(2)
      r(2)=-ab(2,1)*xy(1)+ab(1,1)*xy(2)
      if(all(mod(r,det)==0))then
         xy=r/det
      else
         xy=0
      end if
   end subroutine solve
end program main
