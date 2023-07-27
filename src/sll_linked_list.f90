!> Singly linked list
module sll_linked_list

    use sll_node, only: node, node_init, node_finalizer, node_storage, node_replace
    implicit none

    private
    public :: sll, sll_finalizer, sll_storage

    !> singly linked list
    type sll
        integer, private :: len = 0, cap = 0  !! length and capacity of the list
        type(node), pointer :: head => null()  !! head of the list
        type(node), pointer :: tail => null()  !! tail of the list
    contains
        procedure :: push_back, empty, size
    end type sll

contains

    !> sll push_back
    subroutine push_back(self, data)
        class(sll), intent(inout) :: self  !! sll object to be modified
        class(*), intent(in) :: data  !! data to be stored

        if (self%cap == 0 .or. self%len >= self%cap) then  ! allocate new memory
            if (associated(self%tail)) then
                allocate (self%tail%next, source=node_init(data))
                self%tail => self%tail%next
                self%len = self%len + 1
            else
                allocate (self%head, source=node_init(data))
                self%tail => self%head
                self%len = 1
            end if
        else  ! fill existing memory, replace data
            if (associated(self%tail)) then
                call node_replace(self%tail%next, data)
                self%tail => self%tail%next
                self%len = self%len + 1
            else
                call node_replace(self%head, data)
                self%tail => self%head
                self%len = 1
            end if
        end if

    end subroutine push_back

    !> sll empty
    pure subroutine empty(self)
        class(sll), intent(inout) :: self  !! sll object to be modified

        if (associated(self%tail)) nullify (self%tail)
        self%cap = max(self%cap, self%len)
        self%len = 0

    end subroutine empty

    !> sll size
    integer pure function size(self, capacity)
        class(sll), intent(in) :: self  !! sll object to be measured
        logical, intent(in), optional :: capacity  !! whether to return capacity or length
        logical :: capacity_

        if (present(capacity)) then
            capacity_ = capacity
        else
            capacity_ = .false.
        end if

        if (capacity_) then
            if (self%cap == 0) then
                size = self%len
            else
                size = self%cap
            end if
        else
            size = self%len
        end if

    end function size

    !> sll finalizer
    subroutine sll_finalizer(self)
        class(sll), intent(inout), target :: self  !! sll object to be finalized
        type(node), pointer :: current

        if (associated(self%head)) then
            current => self%head
            do while (associated(current))
                self%head => current%next
                call node_finalizer(current)
                deallocate (current)
                current => self%head
            end do
        end if

    end subroutine sll_finalizer

    !> sll storage
    integer function sll_storage(self)
        type(sll), intent(in), target :: self  !! sll object to be measured
        type(node), pointer :: current

        sll_storage = storage_size(self%head)*2
        if (associated(self%head)) then
            current => self%head
            do while (associated(current))
                sll_storage = sll_storage + node_storage(current)
                current => current%next
            end do
        end if

    end function sll_storage

end module sll_linked_list
