! Singly linked list in Fortran
module slinked_list_m

    implicit none
    private

    public :: linked_list_t

    type node_t
        private
        type(node_t), pointer :: next => null()
        class(*), allocatable :: item
    contains
        procedure :: clear => node_t_clear
    end type node_t

    type linked_list_t
        integer, private :: num_nodes = 0
        type(node_t), pointer :: head => null()
        type(node_t), pointer :: tail => null()
    contains
        procedure :: push => linked_list_t_push
        procedure :: pop => linked_list_t_pop
        procedure :: insert => linked_list_t_insert
        procedure :: remove => linked_list_t_remove
        generic :: get => &
            linked_list_t_get_generic, &
            linked_list_t_user_get
        procedure, private :: &
            linked_list_t_get_generic, &
            linked_list_t_user_get
        procedure :: replace => linked_list_t_replace
        procedure :: size => linked_list_t_size
        procedure :: clear => linked_list_t_clear
    end type linked_list_t

    abstract interface
        subroutine get_value(this_item, return_item)
            class(*), intent(in) :: this_item
            class(*), intent(out) :: return_item
        end subroutine get_value
    end interface

contains

    pure function init_node(new_item) result(new_node)
        type(node_t) new_node
        class(*), intent(in) :: new_item
        allocate (new_node%item, source=new_item)
    end function init_node

    pure subroutine node_t_clear(self)
        class(node_t), intent(inout) :: self
        if (allocated(self%item)) deallocate (self%item)
        nullify (self%next)
    end subroutine node_t_clear

    pure subroutine node_t_clear_all(self)
        class(node_t), intent(inout) :: self
        type(node_t), pointer :: curr_node
        type(node_t), pointer :: next_node
        curr_node = self
        next_node => self%next
        do
            call curr_node%clear()
            deallocate (curr_node)

            if (.not. associated(next_node)) exit
            curr_node => next_node
            next_node => next_node%next
        end do
    end subroutine node_t_clear_all

    pure subroutine linked_list_t_push(self, item)
        class(linked_list_t), intent(inout) :: self
        class(*), intent(in) :: item
        if (associated(self%tail)) then
            allocate (self%tail%next, source=init_node(item))
            self%tail => self%tail%next
        else
            allocate (self%head, source=init_node(item))
            self%tail => self%head
        end if
        self%num_nodes = self%num_nodes + 1
    end subroutine linked_list_t_push

    pure subroutine linked_list_t_pop(self)
        class(linked_list_t), intent(inout) :: self
        if (associated(self%tail)) then
            call self%remove(self%num_nodes)
        else
            return
        end if
    end subroutine linked_list_t_pop

    pure subroutine linked_list_t_insert(self, item, node_index)
        class(linked_list_t), intent(inout) :: self
        class(*), intent(in) :: item
        integer, intent(in) :: node_index
        type(node_t), pointer :: curr_node, next_node
        integer index
        index = node_index - 1
        if (index >= self%num_nodes) then   ! insert at end
            call self%push(item)
        elseif (index < 1) then             ! insert at beginning
            curr_node => self%head
            allocate (self%head, source=init_node(item))
            self%head%next => curr_node
            nullify (curr_node)
        else                                ! insert in target index
            curr_node => self%head
            do while (index > 1)
                index = index - 1
                curr_node => curr_node%next
            end do
            next_node => curr_node%next
            allocate (curr_node%next, source=init_node(item))
            curr_node%next%next => next_node
            self%num_nodes = self%num_nodes + 1
            nullify (next_node, curr_node)
        end if
    end subroutine linked_list_t_insert

    pure subroutine linked_list_t_remove(self, node_index)
        class(linked_list_t), intent(inout) :: self
        integer, intent(in) :: node_index
        type(node_t), pointer :: curr_node, temp_node
        integer index
        if (node_index < 1 .or. node_index > self%num_nodes) return
        curr_node => self%head
        index = 1
        if (node_index == 1) then
            self%head => self%head%next
            call curr_node%clear()
            deallocate (curr_node)
            self%num_nodes = self%num_nodes - 1
            return
        end if
        do while (associated(curr_node))
            if (index == node_index - 1) then
                temp_node => curr_node
            elseif (index == node_index) then
                temp_node%next => curr_node%next
                call curr_node%clear()
                deallocate (curr_node)
                self%num_nodes = self%num_nodes - 1
                return
            end if
            curr_node => curr_node%next
            index = index + 1
        end do
    end subroutine linked_list_t_remove

    impure subroutine linked_list_t_get_generic(self, node_index, return_item)
        class(linked_list_t), intent(inout) :: self
        integer, intent(in) :: node_index
        class(*), pointer, intent(out) :: return_item
        type(node_t), pointer :: curr_node
        integer index
        if (node_index < 1 .or. node_index > self%num_nodes) then
            nullify (return_item)
            return
        end if
        curr_node => self%head
        index = 1
        do while (associated(curr_node))
            if (index == node_index) then
                return_item => curr_node%item
                nullify (curr_node)
                return
            end if
            curr_node => curr_node%next
            index = index + 1
        end do
        nullify (curr_node)
        nullify (return_item)
    end subroutine linked_list_t_get_generic

    impure subroutine linked_list_t_user_get(self, node_index, func, return_item)
        class(linked_list_t), intent(inout) :: self
        integer, intent(in) :: node_index
        procedure(get_value) :: func
        class(*), intent(out) :: return_item
        type(node_t), pointer :: curr_node
        integer index
        if (node_index < 1 .or. node_index > self%num_nodes) return
        curr_node => self%head
        index = 1
        do while (associated(curr_node))
            if (index == node_index) then
                call func(curr_node%item, return_item)
                nullify (curr_node)
                return
            end if
            curr_node => curr_node%next
            index = index + 1
        end do
        nullify (curr_node)
    end subroutine linked_list_t_user_get

    pure subroutine linked_list_t_replace(self, item, node_index)
        class(linked_list_t), intent(inout) :: self
        class(*), intent(in) :: item
        integer, intent(in) :: node_index
        type(node_t), pointer :: curr_node
        integer index
        if (node_index < 1 .or. node_index > self%num_nodes) return
        index = node_index
        curr_node => self%head
        do while (index > 1)
            index = index - 1
            curr_node => curr_node%next
        end do
        curr_node%item = item
    end subroutine linked_list_t_replace

    pure function linked_list_t_size(self) result(size)
        class(linked_list_t), intent(in) :: self
        integer size
        size = self%num_nodes
    end function linked_list_t_size

    pure subroutine linked_list_t_clear(self)
        class(linked_list_t), intent(inout) :: self
        type(node_t), pointer :: curr_node
        do while (self%num_nodes > 0)
            curr_node => self%head
            if (associated(curr_node%next)) self%head => curr_node%next
            call curr_node%clear()
            deallocate (curr_node)
            self%num_nodes = self%num_nodes - 1
        end do
        nullify (self%head, self%tail)
    end subroutine linked_list_t_clear

end module slinked_list_m
