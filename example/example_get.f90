program main

    use sll_module, only: sll, iterator, sll_storage, sll_finalizer, iterator_finalizer
    implicit none
    type(sll) :: list  !! singly linked list
    type(iterator) :: iter  !! iterator for sll
    class(*), pointer :: ptr  !! generic pointer

    call list%push_back(1.0)
    call list%push_back(2)

    iter = iterator(list)
    do while (iter%next(ptr))

        select type (ptr)
        type is (real)
            print *, "v: ", ptr
        type is (integer)
            print *, "v: ", ptr
        end select

    end do

    print *, "storage size: ", sll_storage(list) + storage_size(list), " bits."
    call sll_finalizer(list)  ! free memory
    call iterator_finalizer(iter)  ! free memory

end program main
!> v:    1.00000000    
!> v:            2
!> storage size:         1600  bits.
