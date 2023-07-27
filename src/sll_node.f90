!> node of singly linked list
module sll_node

    implicit none

    private
    public :: node, node_finalizer, node_init, node_storage

    !> node type
    type node
        class(*), allocatable :: data  !! data
        type(node), pointer :: next => null()  !! next node
    end type node

contains

    !> initialize node
    type(node) pure function node_init(data)
        class(*), intent(in) :: data  !! data to store

        allocate (node_init%data, source=data)

    end function node_init

    !> finalize node
    subroutine node_finalizer(self)
        type(node), intent(inout) :: self  !! node to finalize

        if (allocated(self%data)) deallocate (self%data)
        if (associated(self%next)) nullify (self%next)

    end subroutine node_finalizer

    !> node storage
    pure integer function node_storage(self)
        type(node), intent(in) :: self  !! node to calculate storage

        node_storage = storage_size(self%data) + storage_size(self%next)

    end function node_storage

end module sll_node
