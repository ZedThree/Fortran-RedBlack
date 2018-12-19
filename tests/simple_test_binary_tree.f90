program simple_test_binary_tree
  use binary_tree
  implicit none

  block
    type(binary_tree_t) :: tree
    integer, allocatable :: vals(:)

    print*, "--------------------"
    print*, " Binary tree"

    call tree_add(tree, 4)
    call tree_add(tree, 5)
    call tree_add(tree, 3)
    call tree_add(tree, 2)
    call tree_add(tree, 1)

    if (associated(tree%root)) then
      print*, tree%root%val
      print*, tree%root%left%val
      print*, tree%root%right%val
    end if

    write(*, '(a)', advance='no') "print_tree: "
    call print_tree(tree)

    write(*, '(a)', advance='no') "get_values_tree: "
    vals = get_values_tree(tree)
    print*, vals
  end block

end program simple_test_binary_tree
