program main

    use sll_module, only: sll, iterator, sll_storage, sll_finalizer, iterator_finalizer
    use display_module, only: display
    implicit none
    type(sll) :: list  !! singly linked list
    type(iterator) :: iter  !! iterator for sll
    class(*), pointer :: ptr  !! generic pointer

    call list%push_back(1.0)
    call list%push_back(2)
    call display(list%size(), "size:", inline=.true.)
    call display(list%size(.true.), "capacity:", inline=.true.)

    call list%empty()
    call list%push_back(3.0)
    call display(list%size(), "size:", inline=.true.)
    call display(list%size(.true.), "capacity:", inline=.true.)

    if (.not. list%is_empty()) then

        iter = iterator(list)
        do while (iter%next(ptr))

            select type (ptr)
            type is (real)
                call display(ptr, "v:", inline=.true.)
            type is (integer)
                call display(ptr, "v:", inline=.true.)
            end select

        end do

    end if

    call display(sll_storage(list) + storage_size(list), "storage size (bit):", inline=.true.)
    call sll_finalizer(list)  ! free memory
    call iterator_finalizer(iter)  ! free memory

end program main
!> [scalar] size: 2
!> [scalar] capacity: 2
!> [scalar] size: 1
!> [scalar] capacity: 2
!> [scalar] v:  3.000E+00
!> [scalar] storage size (bit): 1664
