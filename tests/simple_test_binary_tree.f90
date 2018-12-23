program simple_test_binary_tree
  use binary_tree
  implicit none

  block
    type(binary_tree_t) :: tree
    integer, allocatable :: vals(:)

    print*, "--------------------"
    print*, " Binary tree"

    call tree%add(4)
    call tree%add(5)
    call tree%add(3)
    call tree%add(2)
    call tree%add(1)

    if (associated(tree%root)) then
      print*, tree%root%val
      print*, tree%root%left%val
      print*, tree%root%right%val
    end if

    write(*, '(a)', advance='no') "print_tree: "
    call tree%print()

    write(*, '(a)', advance='no') "get_values_tree: "
    vals = tree%values()
    print*, vals
  end block

end program simple_test_binary_tree
