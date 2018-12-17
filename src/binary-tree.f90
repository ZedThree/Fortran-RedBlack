module binary_tree

  implicit none

  type :: node_t
    type(node_t), pointer :: left => null()
    type(node_t), pointer :: right => null()

    integer :: val
  end type node_t

  type :: binary_tree_t
    type(node), pointer :: root => null()
  end type binary_tree_t

contains

  subroutine tree_add(this, val)
    class(binary_tree_t), intent(inout) :: this
    integer, intent(in) :: val

    if (.not. associated(this%root)) then
      allocate(this%root)
      this%root%val = val
    else
      call tree_add_at_node(this%root, val)
    end if
  end subroutine tree_add

  recursive subroutine tree_add_at_node(node, val)
    class(node_t), intent(inout) :: node
    integer, intent(in) :: val

    if (val < node%val) then
      if (associated(node%left)) then
        call tree_add_at_node(node%left, val)
      else
        allocate(node%left)
        node%left%val = val
      end if
    else
      if (associated(node%right)) then
        call tree_add_at_node(node%right, val)
      else
        allocate(node%right)
        node%right%val = val
      end if
    end if
  end subroutine tree_add_at_node

  function find(this, val)
    class(binary_tree_t), intent(in) :: this
    integer, intent(in) :: val
    type(node_t), pointer :: find

    find => null()

    if (.not. associated(this%root)) then
      return
    end if

    find => find_at_node(this%root, val)
  end function find

  recursive function find_at_node(node, val) result(found)
    type(node_t), pointer :: node
    integer, intent(in) :: val
    type(node_t), pointer :: found

    found => null()

    if (val == node%val) then
      found => node
    elseif (val < node%val) then
      if (.not. associated(node%left)) then
        return
      end if

      found => find_at_node(node%left, val)
    else
      if (.not. associated(node%right)) then
        return
      end if

      found => find_at_node(node%right, val)
    end if
  end function find_at_node

  subroutine delete_tree(this)
    type(binary_tree_t), intent(inout) :: this

    if (associated(this%root)) then
      call delete_node(this%root)
      deallocate(this%root)
    end if

  end subroutine delete_tree

  recursive subroutine delete_node(node)
    type(node_t), intent(inout) :: node

    if (associated(node%left)) then
      call delete_node(node%left)
      deallocate(node%left)
    end if

    if (associated(node%right)) then
      call delete_node(node%right)
      deallocate(node%right)
    end if
  end subroutine delete_node

end module binary_tree

    
