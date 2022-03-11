program main

    use slinked_list_m, only: linked_list_t
    use, intrinsic :: iso_fortran_env, only: ik => int32, rk => real32
    implicit none
    class(*), pointer :: ll_ptr

    type(linked_list_t) :: list
    print *, list%size()

    call list%push(1.0_rk)
    call list%push(2_ik)

    call list%get(1, ll_ptr)

    select type (ll_ptr)
    type is (real(rk))
        print *, ll_ptr
    end select

    print *, list%size()

end program main
