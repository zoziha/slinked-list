!> Singly linked list
module sll_linked_list

    use sll_node, only: node, node_init, node_finalizer, node_storage
    implicit none

    private
    public :: sll, sll_finalizer, sll_storage

    !> singly linked list
    type sll
        type(node), pointer :: head => null()  !! head of the list
        type(node), pointer :: tail => null()  !! tail of the list
    contains
        procedure :: push_back
    end type sll

contains

    !> sll push_back
    pure subroutine push_back(self, data)
        class(sll), intent(inout) :: self  !! sll object to be modified
        class(*), intent(in) :: data  !! data to be stored

        if (associated(self%tail)) then
            allocate (self%tail%next, source=node_init(data))
            self%tail => self%tail%next
        else
            allocate (self%head, source=node_init(data))
            self%tail => self%head
        end if

    end subroutine push_back

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
