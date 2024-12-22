module dyn_hash_mod
   implicit none
   private
   public::dyn_hashmap,dyn_hash_iter
   abstract interface
      subroutine  equal(a,b)
         class(*),intent(inout)::a
         class(*),intent(in)::b
      end subroutine equal
      subroutine  bitarray(a,b)
         class(*),intent(in),target::a
         integer(1),intent(inout),pointer::b(:)
      end subroutine bitarray
      subroutine  dyn_init(a)
         class(*),intent(inout),allocatable::a
      end subroutine dyn_init
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

   integer,parameter::mdim=997
   type dyn_hashmap
      integer::num
      type(node)::a(mdim)
      procedure(equal),nopass,pointer::eq
      procedure(bitarray),nopass,pointer::bit
      procedure(dyn_init),nopass,pointer::dyn
   contains
      generic::operator(.in.) => in
      procedure,pass::add => hash_add
      procedure,pass::init => hash_init
      procedure,pass::init_dyn => hash_init_dyn
      procedure,pass::view    => hash_view
      procedure,pass::remove => hash_remove
      procedure,pass(this)::in => hash_in
      procedure,pass::clean => hash_clean
   end type dyn_hashmap

   type dyn_hash_iter
      integer::size
      type(node),pointer::pos
      type(dyn_hashmap),pointer::h
    contains
          procedure,pass::init => dyn_hash_iter_init
          procedure,pass::next => dyn_hash_iter_next
          procedure,pass::view => dyn_hash_iter_view
   end type dyn_hash_iter
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
      if(associated(ptr,this))then
         print*,"[ERROR] can't remove head"
         return
      end if
      this%next=>ptr%next
      deallocate(ptr%key)
      deallocate(ptr%val)
      nullify(ptr%next)
      deallocate(ptr)
   end subroutine list_remove

   subroutine dyn_hash_iter_init(this,h)
      class(dyn_hash_iter),intent(inout)::this
      type(dyn_hashmap),intent(in),target::h
      this%size=1
      this%h  =>h
      this%pos=>h%a(1)!%next
   end subroutine dyn_hash_iter_init

   logical function dyn_hash_iter_next(this,key)result(res)
      class(dyn_hash_iter),intent(inout),target::this
      class(*),intent(inout)::key
      !class(*),pointer::val
      this%pos=>this%pos%next
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
      !call this%h%eq(val,this%pos%val)
      !val=>this%pos%val
   end function dyn_hash_iter_next

   function dyn_hash_iter_view(this)result(val)
      class(dyn_hash_iter),intent(inout),target::this
      class(*),pointer::val
      val=>this%pos%val
   end function dyn_hash_iter_view

   subroutine hash_init(this,eq,bit,dyn)
      class(dyn_hashmap),intent(inout)::this
      procedure(equal)::eq
      procedure(bitarray)::bit
      procedure(dyn_init)::dyn
      integer::i
      this%eq =>eq
      this%bit=>bit
      this%dyn=>dyn
      do i=1,mdim
         call this%a(i)%init()
      end do
   end subroutine hash_init

   subroutine hash_init_dyn(this,ptr)
      class(dyn_hashmap),intent(inout)::this
      class(*),allocatable::ptr
      call this%dyn(ptr)
   end subroutine hash_init_dyn
   

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
      class(dyn_hashmap),intent(inout),target::this
      integer::i
      do i=1,mdim
         call final_node(this%a(i))
      end do
      this%num=0
   end subroutine hash_clean

   subroutine hash_add(this,key)
      class(dyn_hashmap),intent(inout),target::this
      class(*),intent(in)::key
      integer::idx
      type(node),pointer::pt
      integer(1),pointer::pk(:),pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      pt=>this%a(idx)%next
      !if(associated(pt,this%a(idx)))this%num=this%num+1
      do while(.not.associated(pt,this%a(idx)))
         call this%bit(pt%key,pb)
         if(eqv(pb,pk))then
            print*,"[ERROR]"
            deallocate(pt%val)
            call this%dyn(pt%val)
            return
         end if
         pt=>pt%next
      end do
      allocate(pt)
      allocate(pt%key,source=key)
      call this%dyn(pt%val)
      call this%a(idx)%front(pt)
   end subroutine hash_add

   subroutine hash_view(this,key,val)!result(val)
      class(dyn_hashmap),intent(inout),target::this
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
   end subroutine hash_view

   logical function hash_in(key,this) result(val)
      class(dyn_hashmap),intent(in),target::this
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
      class(dyn_hashmap),intent(inout),target::this
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
end module dyn_hash_mod
