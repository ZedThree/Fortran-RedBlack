module binary_tree

  implicit none

  type :: binary_node_t
    type(binary_node_t), pointer :: left => null()
    type(binary_node_t), pointer :: right => null()

    integer :: val
  end type binary_node_t

  type :: binary_tree_t
    type(binary_node_t), pointer :: root => null()
    integer :: size = 0
  contains
    final :: delete_tree
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

    this%size = this%size + 1
  end subroutine tree_add

  recursive subroutine tree_add_at_node(node, val)
    class(binary_node_t), intent(inout) :: node
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
    type(binary_node_t), pointer :: find

    find => null()

    if (.not. associated(this%root)) then
      return
    end if

    find => find_at_node(this%root, val)
  end function find

  recursive function find_at_node(node, val) result(found)
    type(binary_node_t), pointer :: node
    integer, intent(in) :: val
    type(binary_node_t), pointer :: found

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
    type(binary_node_t), intent(inout) :: node

    if (associated(node%left)) then
      call delete_node(node%left)
      deallocate(node%left)
    end if

    if (associated(node%right)) then
      call delete_node(node%right)
      deallocate(node%right)
    end if
  end subroutine delete_node

  subroutine print_tree(this)
    class(binary_tree_t), intent(in) :: this

    if (.not. associated(this%root)) then
      return
    end if

    call print_node(this%root)
    print*, ""
  end subroutine print_tree

  recursive subroutine print_node(node)
    type(binary_node_t), intent(in) :: node

    if (associated(node%left)) then
      call print_node(node%left)
    end if

    write(*, '(i0, a)', advance='no') node%val, " "

    if (associated(node%right)) then
      call print_node(node%right)
    end if
  end subroutine print_node

  function get_values_tree(this) result(vals)
    class(binary_tree_t), intent(in) :: this
    integer, allocatable :: vals(:)

    integer :: index

    allocate(vals(this%size))

    if (.not. associated(this%root)) then
      return
    end if

    index = 0
    call get_values_node(this%root, vals, index)
  end function get_values_tree

  recursive subroutine get_values_node(node, vals, index)
    type(binary_node_t), intent(in) :: node
    integer, allocatable, intent(inout) :: vals(:)
    integer, intent(inout) :: index

    if (associated(node%left)) then
      call get_values_node(node%left, vals, index)
    end if

    index = index + 1
    vals(index) = node%val

    if (associated(node%right)) then
      call get_values_node(node%right, vals, index)
    end if
  end subroutine get_values_node

  recursive function node_min_value(node) result(min)
    type(binary_node_t), intent(in) :: node
    integer :: min

    if (.not. associated(node%left)) then
      min = node%val
    else
      min = node_min_value(node%left)
    end if
  end function node_min_value

  function tree_remove(this, val) result(removed)
    class(binary_tree_t), intent(inout) :: this
    integer, intent(in) :: val
    logical :: removed

    type(binary_node_t), pointer :: removed_node => null()

    if (.not. associated(this%root)) then
      removed = .false.
      return
    end if

    if (this%root%val == val) then
      block
        type(binary_node_t), target :: dummy_root
        type(binary_node_t), pointer :: dummy_root_p

        dummy_root_p => dummy_root
        dummy_root_p%left => this%root
        dummy_root_p%right => null()
        removed_node => node_remove(this%root, dummy_root_p, val)

        this%root => dummy_root_p%left
      end block
    else
      removed_node => node_remove(this%root, null(), val)
    end if

    if (associated(removed_node)) then
      deallocate(removed_node)
      removed_node => null()
      removed = .true.
      this%size = this%size - 1
    else
      removed = .false.
    end if

  end function tree_remove

  recursive function node_remove(this, parent, val) &
       & result(removed_node)
    type(binary_node_t), pointer :: this
    type(binary_node_t), pointer :: parent
    integer :: val

    type(binary_node_t), pointer :: removed_node

    removed_node => null()

    if (val < this%val) then
      if (.not. associated(this%left)) then
        return
      end if
      removed_node => node_remove(this%left, this, val)
    else if (val > this%val) then
      if (.not. associated(this%right)) then
        return
      end if
      removed_node => node_remove(this%right, this, val)
    else
      if (associated(this%left) .and. associated(this%right)) then
        this%val = node_min_value(this%right)
        removed_node => node_remove(this%right, this, this%val)
      else if (associated(parent%left, target=this)) then

        removed_node => this

        if (associated(this%left)) then
          parent%left => this%left
        else
          parent%left => this%right
        end if
      else if (associated(parent%right, target=this)) then

        removed_node => this

        if (associated(this%left)) then
          parent%right => this%left
        else
          parent%right => this%right
        end if
      end if
    end if

  end function node_remove

end module binary_tree
