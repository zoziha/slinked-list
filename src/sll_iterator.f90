!> Singly linked list iterator
module sll_iterator

    use sll_node, only: node
    use sll_linked_list, only: sll
    implicit none

    private
    public :: iterator, iterator_finalizer, iterator_storage

    !> singly linked list iterator
    type iterator
        private
        type(node), pointer :: current => null()  !! current node
    contains
        procedure :: next
    end type iterator

    !> constructor
    interface iterator
        module procedure :: iterator_init
    end interface iterator

contains

    !> initialize iterator
    type(iterator) function iterator_init(list)
        type(sll), intent(in), target :: list  !! singly linked list to iterate

        iterator_init%current => list%head

    end function iterator_init

    !> finalize iterator
    pure subroutine iterator_finalizer(self)
        type(iterator), intent(inout) :: self  !! iterator to finalize

        self%current => null()

    end subroutine iterator_finalizer

    !> get current node and advance iterator
    logical function next(self, data)
        class(iterator), intent(inout) :: self  !! iterator to advance
        class(*), pointer, intent(out) :: data  !! data stored in current node (if any)

        next = associated(self%current)
        if (next) then
            data => self%current%data
            self%current => self%current%next
        end if

    end function next

    !> iterator storage
    pure integer function iterator_storage(self)
        type(iterator), intent(in) :: self  !! iterator to get storage size

        iterator_storage = storage_size(self%current)

    end function iterator_storage

end module sll_iterator