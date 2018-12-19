program simple_test_redblack_tree
  use redblack_tree
  implicit none

  block
    type(redblack_tree_t) :: tree
    integer, allocatable :: vals(:)

    print*, "--------------------"
    print*, " Redblack tree"

    call tree_add(tree, 0)
    call pretty_print_tree(tree)
    print*, ""
    call tree_add(tree, 1)
    call pretty_print_tree(tree)
    print*, ""
    call tree_add(tree, 2)
    call pretty_print_tree(tree)
    print*, ""
    call tree_add(tree, 3)
    call pretty_print_tree(tree)
    print*, ""
    call tree_add(tree, 4)
    call pretty_print_tree(tree)
    print*, ""
    call tree_add(tree, 5)
    call pretty_print_tree(tree)
    print*, ""
    call tree_add(tree, 6)
    call pretty_print_tree(tree)
    print*, ""
    call tree_add(tree, 7)
    call pretty_print_tree(tree)
    print*, ""
    call tree_add(tree, 8)
    call pretty_print_tree(tree)
    print*, ""

    write(*, '(a)', advance='no') "print_tree: "
    call print_tree(tree)

    write(*, '(a)', advance='no') "get_values_tree: "
    vals = get_values_tree(tree)
    print*, vals

  end block

end program simple_test_redblack_tree
