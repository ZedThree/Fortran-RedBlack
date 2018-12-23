program simple_test_redblack_tree
  use redblack_tree
  implicit none

  block
    type(redblack_tree_t) :: tree
    integer, allocatable :: vals(:)

    print*, "--------------------"
    print*, " Redblack tree"

    call tree%add(0)
    call pretty_print_tree(tree)
    print*, ""
    call tree%add(1)
    call pretty_print_tree(tree)
    print*, ""
    call tree%add(2)
    call pretty_print_tree(tree)
    print*, ""
    call tree%add(3)
    call pretty_print_tree(tree)
    print*, ""
    call tree%add(4)
    call pretty_print_tree(tree)
    print*, ""
    call tree%add(5)
    call pretty_print_tree(tree)
    print*, ""
    call tree%add(6)
    call pretty_print_tree(tree)
    print*, ""
    call tree%add(7)
    call pretty_print_tree(tree)
    print*, ""
    call tree%add(8)
    call pretty_print_tree(tree)
    print*, ""

    write(*, '(a)', advance='no') "print_tree: "
    call tree%print()

    write(*, '(a)', advance='no') "get_values_tree: "
    vals = tree%values()
    print*, vals

  end block

end program simple_test_redblack_tree
