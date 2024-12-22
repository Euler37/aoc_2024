module hash_mod
   implicit none
   private
   public::hashmap,hash_iter
   abstract interface
      subroutine  equal(a,b)
         class(*),intent(inout)::a
         class(*),intent(in)::b
      end subroutine equal
      subroutine  bitarray(a,b)
         class(*),intent(in),target::a
         integer(1),intent(inout),pointer::b(:)
      end subroutine bitarray
   end interface

   type node
      class(*),allocatable::key
      class(*),allocatable::val
      type(node),pointer::next=>null()
   contains
      procedure::init=>list_init
      procedure::front=>list_addhead
      procedure::remove=>list_remove
      procedure::final_node
      ! final::final_node
   end type node

   integer,parameter::mdim=9997
   type hashmap
      integer::num
      type(node)::a(mdim)
      procedure(equal),nopass,pointer::eq
      procedure(bitarray),nopass,pointer::bit
   contains
      generic::operator(.in.) => in
      procedure,pass::append => hash_append
      procedure,pass::init => hash_init
      procedure,pass::get    => hash_get
      procedure,pass::view    => hash_view
      procedure,pass::remove => hash_remove
      procedure,pass(this)::in => hash_in
      procedure,pass::clean => hash_clean
   end type hashmap

   type hash_iter
      integer::size
      type(node),pointer::pos
      type(hashmap),pointer::h
    contains
          procedure,pass::init => hash_iter_init
          procedure,pass::next => hash_iter_next
   end type hash_iter
contains
   subroutine list_init(this)
      class(node),intent(inout),target::this
      this%next=>this
   end subroutine list_init

   subroutine list_addhead(this,new)
      class(node),intent(inout),target::this
      type(node),intent(inout),pointer::new
      !this ->  a b c 
      !    new 
      new %next =>this%next
      this%next=>new
   end subroutine list_addhead

   subroutine list_remove(this,ptr)
      class(node),intent(inout),target::this
      type(node),intent(inout),pointer::ptr
      !this ->  a b c 
      !     ptr 
      this%next=>ptr%next
      deallocate(ptr%key)
      deallocate(ptr%val)
      nullify(ptr%next)
      deallocate(ptr)
   end subroutine list_remove

   subroutine hash_iter_init(this,h)
      class(hash_iter),intent(inout)::this
      type(hashmap),intent(in),target::h
      this%size=1
      this%h  =>h
      this%pos=>h%a(1)%next
   end subroutine hash_iter_init

   logical function hash_iter_next(this,key,val)result(res)
      class(hash_iter),intent(inout),target::this
      class(*),intent(inout)::key,val
      res=.true.
      do
         if(associated(this%pos,this%h%a(this%size)))then
            this%size=this%size+1
            if(this%size>mdim)then
               res=.false.
               return
            end if
            this%pos=>this%h%a(this%size)%next
         else
            exit
         end if
      end do
      call this%h%eq(key,this%pos%key)
      call this%h%eq(val,this%pos%val)
      this%pos=>this%pos%next
   end function hash_iter_next

   subroutine hash_init(this,eq,bit)
      class(hashmap),intent(inout)::this
      procedure(equal)::eq
      procedure(bitarray)::bit
      integer::i
      this%eq =>eq
      this%bit=>bit
      do i=1,mdim
         call this%a(i)%init()
      end do
   end subroutine hash_init
   

   subroutine final_node(this)
      class(node),intent(inout),target::this
      type(node),pointer::ptr
      ptr=>this%next
      do while(.not.associated(ptr,this))
         call this%remove(ptr)
         ptr=>this%next
      end do
   end subroutine final_node

   subroutine hash_clean(this)
      class(hashmap),intent(inout),target::this
      integer::i
      type(node),pointer::tmp
      do i=1,size(this%a)
         call final_node(this%a(i))
      end do
      this%num=0
   end subroutine hash_clean

   subroutine hash_append(this,key,val)
      class(hashmap),intent(inout),target::this
      class(*),intent(in)::key
      class(*),intent(in)::val
      integer::idx
      type(node),pointer::pt
      integer(1),pointer::pk(:),pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      pt=>this%a(idx)%next
      if(associated(pt,this%a(idx)))this%num=this%num+1
      do while(.not.associated(pt,this%a(idx)))
         call this%bit(pt%key,pb)
         if(eqv(pb,pk))then
            call this%eq(pt%val,val)
            return
         end if
         pt=>pt%next
      end do
      allocate(pt)
      allocate(pt%key,source=key)
      allocate(pt%val,source=val)
      call this%a(idx)%front(pt)
   end subroutine hash_append

   subroutine  hash_get(this,key,val)
      class(hashmap),intent(inout),target::this
      class(*),intent(in)::key
      class(*),intent(inout)::val
      integer::idx
      type(node),pointer::pt
      integer(1),pointer::pk(:),pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      pt=>this%a(idx)%next
      do while(.not.associated(pt,this%a(idx)))
         call this%bit(pt%key,pb)
         if(eqv(pb,pk))then
            call this%eq(val,pt%val)
            exit
         end if
         pt=>pt%next
      end do
   end subroutine hash_get

   ! subroutine hash_view(this,key,val)
      ! class(hashmap),intent(inout),target::this
      ! class(*),intent(in)::key
      ! class(*),pointer::val
      ! integer::idx
      ! type(node),pointer::pt
      ! integer(1),pointer::pk(:),pb(:)
      ! call this%bit(key, pk)
      ! idx=modulo(hash_code(pk),mdim)+1
      ! pt=>this%a(idx)%next
      ! do while(.not.associated(pt,this%a(idx)))
         ! call this%bit(pt%key,pb)
         ! if(eqv(pb,pk))then
            ! val=>pt%val
            ! exit
         ! end if
         ! pt=>pt%next
      ! end do
   ! end subroutine hash_view

   function hash_view(this,key)result(val)
      class(hashmap),intent(inout),target::this
      class(*),intent(in)::key
      class(*),pointer::val
      integer::idx
      type(node),pointer::pt
      integer(1),pointer::pk(:),pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      pt=>this%a(idx)%next
      do while(.not.associated(pt,this%a(idx)))
         call this%bit(pt%key,pb)
         if(eqv(pb,pk))then
            val=>pt%val
            exit
         end if
         pt=>pt%next
      end do
   end function hash_view

   logical function hash_in(key,this) result(val)
      class(hashmap),intent(in),target::this
      class(*),intent(in)::key
      integer::idx
      type(node),pointer::pt
      integer(1),pointer::pk(:),pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      val=.false.
      pt=>this%a(idx)%next
      do while(.not.associated(pt,this%a(idx)))
         call this%bit(pt%key,pb)
         if(eqv(pb,pk))then
            val=.true.
            exit
         end if
         pt=>pt%next
      end do
   end function hash_in

   subroutine hash_remove(this,key)
      class(hashmap),intent(inout),target::this
      class(*),intent(in)::key
      integer::idx
      type(node),pointer::pt,prev
      integer(1),pointer::pk(:),pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      prev=>this%a(idx)
      pt  =>prev%next
      do while(.not.associated(pt,this%a(idx)))
         call this%bit(pt%key,pb)
         if(eqv(pb,pk))then
            call prev%remove(pt)
            exit
         end if
         pt=>pt%next
      end do
   end subroutine hash_remove

   pure logical function eqv(pa,pb)result(res)
      integer(1),intent(in)::pa(:)
      integer(1),intent(in)::pb(:)
      if(size(pa)/=size(pb))then
         res=.false.
      else
         res=all(pa==pb)
      end if
   end function eqv

   pure integer function hash_code(p)result(h)
      integer(1),intent(in)::p(:)
      integer::i
      integer::g
      integer,parameter::pp=999999937
      integer,parameter::prime=100000037
      h=37
      do i=1,size(p)
         h=h+p(i)*prime
         !h=ieor(h,pp)
         h=h*pp
      end do
   end function hash_code

   ! pure integer(8) function hash_code(p)result(h)
     ! integer(1),intent(in)::p(:)
     ! integer::i
     ! integer(8)::g
     ! integer(8),parameter::pp=int( z'cbf29ce484222325',8)
     ! integer(8),parameter::prime =10000000000037_8
     ! integer,parameter::prime=100000037
     ! 1000000000000002493
     ! 100000000000000543_8

     ! h=1
     ! do i=1,size(p)
        ! h=shiftl(h,8)+p(i)*prime
        ! h=h+p(i)*prime
        ! h=ieor(h,pp)
     ! end do
   ! end function hash_code

   ! integer(8) function hash_code(p)result(h)
      ! integer(1)::p(:)
      ! integer::i
      ! integer(8)::g
      ! integer(8),parameter::PP=int(z"F0000000",8)
      ! h=1
      ! do i=1,size(p)
         ! h=shiftl(h,4)+p(i)
         ! g=iand(h,pp)
         ! if(g/=0)h=ieor(h,shiftl(g,24))
         ! h=iand(h,not(g))
      ! end do
   ! end function hash_code
end module hash_mod
