program main

    use slinked_list_m, only: linked_list_t
    type(linked_list_t) list
    class(*), pointer :: val

    call list%push(1.0)
    call list%push(2.0)
    call list%insert(3.0, 2)
    print *, list%size()
    call list%get(2, val)

    select type (val)
    type is (real)
        print *, val
    end select
    
    call list%clear()
    print *, list%size()

end program main
