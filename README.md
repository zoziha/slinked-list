# Singly Linked List

![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)
[![license](https://img.shields.io/badge/License-MIT-pink)](LICENSE)

Simple generic singly linked list module for in-memory storage of small amounts of data.

*Suggestions and code contributions are welcome.*

## Usage

Only FPM is supported, other build systems can copy source files directly,
and `ifort/ifx` and `gfortran` compilers are tested.

To use `sliked-list` within your `fpm` project, add the following lines to your `fpm.toml` file:

```toml
[dependencies]
sliked-list = { git="https://github.com/zoziha/sliked-list" }
```

## Example

```sh
> fpm run --example --all  # run the example
```

```fortran
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
```

Note: Due to the type conversion of `class(*)`, `sll` is not efficient, so it is only used for storing small datasets.

## Link

- [zoziha/dlinked_list](https://gitee.com/zoziha/dlinked_list)
- [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)
- [浅谈单链表与双链表的区别](https://blog.csdn.net/kangxidagege/article/details/80211225)
