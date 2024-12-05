program main
   use aoc_2024
   implicit none
   integer::n,unit,ios,i,m,res,res2,j
   character(len=*),parameter::filename="data/5.txt"
   character(len=200)::tmp
   integer,allocatable::rules(:,:)
   integer,allocatable::update(:)
   integer::weight(2,100)
   ! get rules
   open(newunit=unit,file=filename)
   n=0
   do
      tmp=""
      read(unit,"(A)",iostat=ios)tmp
      if(tmp==" ")exit
      n=n+1
   end do
   allocate(rules(2,n))
   rewind(unit)
   do i=1,n
      read(unit,"(A)")tmp
      call replace(tmp,"|",",")
      read(tmp,*)rules(:,i)
   end do
   ! pass
   read(unit,*)
   ! 
   res=0
   res2=0
   do
      tmp=""
      read(unit,"(A)",iostat=ios)tmp
      if(ios/=0)exit
      m=getcolnum(tmp)
      allocate(update(m))
      read(tmp,*)update
      if (isright(update))then
         ! part 1
         res=res+update(m/2+1)
      else
         ! part 2 
         ! bubble sort
         do i=1,m-1
            do j=1,m-i
               if(.not.isright(update(j:j+1)))then
                  update(j:j+1)=update(j+1:j:-1)
               end if
            end do
         end do
         res2=res2+update(m/2+1)
      end if
      deallocate(update)
   end do
   close(unit)
   print*,res
   print*,res2
contains
   logical function isright(a)result(res)
      integer,intent(in)::a(:)
      integer::i,r1,r2
      integer::idx(100)
      ! hash 
      idx=0
      do i=1,size(a)
         idx(a(i))=i
      end do
      res=.true.
      do i=1,n
         ! r1=findloc(a,rules(1,i),dim=1)
         ! r2=findloc(a,rules(2,i),dim=1)
         r1=idx(rules(1,i))
         r2=idx(rules(2,i))
         if(r1/=0.and.r2/=0)then
            if(r2 <= r1)then
               res=.false.
               return
            end if
         end if
      end do
   end function isright
end program main
