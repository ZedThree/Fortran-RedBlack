program simple_test
  use binary_tree
  implicit none

  block
    type(binary_tree_t) :: tree

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
  end block

end program simple_test
