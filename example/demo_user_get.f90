program main

    use slinked_list_m, only: linked_list_t
    use, intrinsic :: iso_fortran_env, only: ik => int32, rk => real32
    implicit none
    real(rk) :: x
    integer(ik) :: n

    type(linked_list_t) :: list
    print *, list%size()

    call list%push(1.0_rk)
    call list%push(2_ik)

    call list%get(1, get_value, x)
    call list%get(2, get_value, n)

    print *, x, n, list%size()

contains

    subroutine get_value(this_item, return_item)
        class(*), intent(in) :: this_item
        class(*), intent(out) :: return_item

        select type (this_item)
        type is (real(rk))
            select type (return_item)
            type is (real(rk))
                return_item = this_item
            class default
                print *, '*<ERROR>* get_value: type mismatch'
            end select
        type is (integer(ik))
            select type (return_item)
            type is (integer(ik))
                return_item = this_item
            class default
                print *, '*<ERROR>* get_value: type mismatch'
            end select
        class default
            print *, "*<ERROR>* get_value: invalid type"
        end select

    end subroutine get_value

end program main
