

module list_mod
   ! linux kernal list
   implicit none
   private
   public::list,list_iter
   type list
      class(*),allocatable::val
      type(list),pointer::next=>null()
      type(list),pointer::prev=>null()
   contains
     procedure::init =>list_init
     procedure::append=>add_tail
     procedure::front =>add_head
     procedure::remove=>list_remove
     procedure::view =>list_view
     procedure::clean=>list_final
     procedure::empty=>list_empty
   end type list

   type list_iter
        type(list),pointer::head
        type(list),pointer::iter
    contains
        procedure::next=>iter_next
        procedure::prev=>iter_prev
        procedure::init=>iter_init
        procedure::view=>iter_view
   end  type list_iter
contains
    subroutine iter_init(this,l)
        class(list_iter),intent(inout)::this
        type(list),intent(inout),target::l
        this%head=>l
        this%iter=>l
    end subroutine iter_init

    logical function iter_next(this)result(res)
        class(list_iter),intent(inout),target::this
        this%iter=>this%iter%next
        res=.not.associated(this%iter,this%head)
    end function iter_next

    logical function iter_prev(this)result(res)
        class(list_iter),intent(inout),target::this
        this%iter=>this%iter%prev
        res=.not.associated(this%iter,this%head)
    end function iter_prev

   function iter_view(this)result(res)
        class(list_iter),intent(inout),target::this
        class(*),pointer::res
        res=>this%iter%val  
   end function iter_view

   subroutine list_init(this)
      class(list),intent(inout),target::this
      this%next=>this
      this%prev=>this
   end subroutine list_init

   subroutine add_kernal(new,prev,next)
      type(list),intent(inout),target::new
      type(list),intent(inout),target::prev
      type(list),intent(inout),target::next
      next%prev=>new
      new %next=>next
      new %prev=>prev
      prev%next=>new
   end subroutine add_kernal

   subroutine add_tail(head,val)
      class(list),intent(inout),target::head
      class(*),intent(in)::val
      type(list),pointer::new
      allocate(new);allocate(new%val,source=val)
      call add_kernal(new,head%prev,head)
   end subroutine add_tail

   subroutine add_head(head,val)
      class(list),intent(inout),target::head
      class(*),intent(in)::val
      type(list),pointer::new
      allocate(new);allocate(new%val,source=val)
      call add_kernal(new,head,head%next)
   end subroutine add_head

   subroutine list_remove(head,ptr)
        class(list),intent(inout),target::head
        type(list),pointer::ptr
        if(associated(ptr,head))then
            print*,"[ERROR] can't remove head"
            return
        end if
        ptr%prev%next=>ptr%next
        ptr%next%prev=>ptr%prev
        deallocate(ptr%val)
        nullify(ptr%next)
        nullify(ptr%prev)
        deallocate(ptr)
   end subroutine list_remove

    subroutine list_final(head)
        class(list),intent(inout),target::head
        type(list),pointer::ptr
        ptr=>head%next
        do while(.not.associated(ptr,head))
            call head%remove(ptr)
            ptr=>head%next
        end do
    end subroutine list_final

    logical function list_empty(head)result(res)
      class(list),intent(inout),target::head
      res=associated(head%next,head)
    end function list_empty


   function list_view(head,ptr)result(res)
        class(list),intent(inout),target::head
        type(list),pointer::ptr
        class(*),pointer::res
        res=>ptr%val
   end function list_view
end module list_mod
