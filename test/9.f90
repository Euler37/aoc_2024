program main
   use aoc_2024
   implicit none
   integer::n
   integer,target,allocatable::a(:)
   integer,pointer::files(:),space(:)
   integer::ptr,fl,fr,sl,i,j
   integer(8)::res
   character(*),parameter::filename="data/9.txt"
   open (10, file=filename, status='old', action='read',&
      &  access='stream', form='unformatted')
   inquire (10, size=n)
   close(10)
   n=n-1
   allocate(a(n))
   ! part 1
   open(10,file="data/9.txt")
   read(10,form("(<>I1)",[n]))a
   close(10)
   files(0:)=>a(1::2)
   space(0:)=>a(2::2)
   ptr=0
   fl=0
   fr=size(files)-1
   sl=0
   res=0
   outer:do
      do i=1,files(fl)
         res=res+ptr*fl
         ptr=ptr+1
      end do
      files(fl)=0
      fl=fl+1
      if(fl >= size(files) )exit
      if(files(fl)==0)exit
      do i=1,space(sl)
         do
            if(files(fr)==0) then
               fr=fr-1
               if(fr <= -1)exit outer
            else
               files(fr)=files(fr)-1
               res=res+ptr*fr
               ptr=ptr+1
               exit
            end if
         end do
      end do
      sl=sl+1
   end do outer
   print*,res
   ! part 2
   open(10,file="data/9.txt")
   read(10,form("(<>I1)",[n]))a
   close(10)
   block
      type pat
         integer::id,num
      end type pat
      type(pat),allocatable::p(:)
      integer::nid,idx,k,l
      files(0:)=>a(1::2)
      nid=size(files)-1
      p=[pat(0,a(1)),(pat(-1,a(i)),pat(i/2,a(i+1)),i=2,n,2)]
      do i=nid,0,-1
         idx=findloc(p%id,i,dim=1)
         do j=1,size(p)
            if(p(j)%id < 0.and.p(j)%num >= files(i))then
               if(idx < j)exit
               p(idx)=pat(-1,files(i))
               if(p(j)%num-files(i)==0)then
                  p(j)=pat(i,files(i))
               else
                  ! ..... -> iii..
                  p=[p(1:j-1),pat(i,files(i)),pat(-1,p(j)%num-files(i)),p(j+1:)]
               end if
               exit
            end if
         end do
      end do
      res=0
      ptr=0
      do i=1,size(p)
         if(p(i)%id<0)then
            ptr=ptr+p(i)%num
         else
            do j=1,p(i)%num
               res=res+ptr*p(i)%id
               ptr=ptr+1
            end do
         end if
      end do
      print*,res
   end block
end program main
