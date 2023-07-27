# Singly Linked List

![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)
[![license](https://img.shields.io/badge/License-MIT-pink)](LICENSE)

Simple generic singly linked list module for in-memory storage of small amounts of data. `sll` provides the ability to reuse linked list space to avoid the time-consuming duplication of memory allocation and destruction in certain application scenarios.

*Suggestions and code contributions are welcome.*

## Usage

Only FPM is supported, other build systems can copy source files directly,
and `ifort/ifx` and `gfortran` compilers are tested.

To use `slinked-list` within your `fpm` project, add the following lines to your `fpm.toml` file:

```toml
[dependencies]
slinked-list = { git="https://github.com/zoziha/slinked-list" }
```

## Example

```sh
> fpm run --example --all  # run the example
```

```fortran
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
```

Note: Due to the type conversion of `class(*)`, `sll` is not efficient, so it is only used for storing small datasets.

## Link

- [zoziha/dlinked_list](https://gitee.com/zoziha/dlinked_list)
- [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)
- [浅谈单链表与双链表的区别](https://blog.csdn.net/kangxidagege/article/details/80211225)
