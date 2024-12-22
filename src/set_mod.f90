module set_mod
   implicit none
   private
   public::set,set_iter
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
      type(node),pointer::next=>null()
   contains
      procedure::init=>list_init
      procedure::front=>list_addhead
      procedure::remove=>list_remove
      procedure::final_node
      ! final::final_node
   end type node

   integer,parameter::mdim=997
   type set
      integer::num
      type(node)::a(mdim)
      procedure(equal),nopass,pointer::eq
      procedure(bitarray),nopass,pointer::bit
   contains
      generic::operator(.in.) => in
      procedure,pass::append => set_append
      procedure,pass::init => set_init
      procedure,pass::remove => set_remove
      procedure,pass(this)::in => set_in
      procedure,pass::clean => set_clean
   end type set

   type set_iter
      integer::size
      type(node),pointer::pos
      type(set),pointer::h
    contains
          procedure,pass::init => set_iter_init
          procedure,pass::next => set_iter_next
   end type set_iter
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
      nullify(ptr%next)
      deallocate(ptr)
   end subroutine list_remove

   subroutine set_iter_init(this,h)
      class(set_iter),intent(inout)::this
      type(set),intent(in),target::h
      this%size=1
      this%h  =>h
      this%pos=>h%a(1)%next
   end subroutine set_iter_init

   logical function set_iter_next(this,key)result(res)
      class(set_iter),intent(inout),target::this
      class(*),intent(inout)::key
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
      this%pos=>this%pos%next
   end function set_iter_next

   subroutine set_init(this,eq,bit)
      class(set),intent(inout)::this
      procedure(equal)::eq
      procedure(bitarray)::bit
      integer::i
      this%eq =>eq
      this%bit=>bit
      do i=1,mdim
         call this%a(i)%init()
      end do
   end subroutine set_init
   
   subroutine final_node(this)
      class(node),intent(inout),target::this
      type(node),pointer::ptr
      ptr=>this%next
      do while(.not.associated(ptr,this))
         call this%remove(ptr)
         ptr=>this%next
      end do
   end subroutine final_node

   subroutine set_clean(this)
      class(set),intent(inout),target::this
      integer::i
      type(node),pointer::tmp
      do i=1,size(this%a)
         call final_node(this%a(i))
      end do
      this%num=0
   end subroutine set_clean

   subroutine set_append(this,key)
      class(set),intent(inout),target::this
      class(*),intent(in)::key
      integer::idx
      type(node),pointer::pt
      integer(1),pointer::pk(:),pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      pt=>this%a(idx)%next
      if(associated(pt,this%a(idx)))this%num=this%num+1
      do while(.not.associated(pt,this%a(idx)))
         call this%bit(pt%key,pb)
         if(eqv(pb,pk)) return
         pt=>pt%next
      end do
      allocate(pt)
      allocate(pt%key,source=key)
      call this%a(idx)%front(pt)
   end subroutine set_append

   logical function set_in(key,this) result(val)
      class(set),intent(in),target::this
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
   end function set_in

   subroutine set_remove(this,key)
      class(set),intent(inout),target::this
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
   end subroutine set_remove

   logical function eqv(pa,pb)result(res)
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
         ! h=ieor(h,pp)
         h=h*pp
      end do
   end function hash_code


   !integer(8) function hash_code(p)result(h)
   !   integer(1)::p(:)
   !   integer::i
   !   integer(8)::g
   !   integer(8),parameter::pp=int( z'cbf29ce484222325',8)
   !   integer(8),parameter::prime = int( z'100000001b3', 8 )
   !   h=1
   !   do i=1,size(p)
   !      h=h+p(i)*prime
   !      h=ieor(h,pp)
   !   end do
   !end function hash_code

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
end module set_mod
