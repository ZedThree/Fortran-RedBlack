module redblack_tree

  implicit none

  type :: redblack_node_t
    type(redblack_node_t), pointer :: left => null()
    type(redblack_node_t), pointer :: right => null()
    logical :: red = .true.

    integer :: val
  end type redblack_node_t

  type :: redblack_tree_t
    type(redblack_node_t), pointer :: root => null()
    integer :: size = 0
  contains
    final :: delete_tree
  end type redblack_tree_t

contains

  function is_red(root)
    type(redblack_node_t), pointer :: root
    logical :: is_red

    is_red = .false.
    if (associated(root)) is_red = root%red
  end function is_red

  function single_rotate(root, is_left) result(old)
    type(redblack_node_t), pointer :: root
    logical, intent(in) :: is_left
    type(redblack_node_t), pointer :: old

    if (is_left) then
      old => root%right
      root%right => old%left
      old%left => root
    else
      old => root%left
      root%left => old%right
      old%right => root
    end if

    root%red = .true.
    old%red = .false.

  end function single_rotate

  function double_rotate(root, is_left) result(old)
    type(redblack_node_t), pointer :: root
    logical, intent(in) :: is_left
    type(redblack_node_t), pointer :: old

    if (is_left) then
      root%right => single_rotate(root%right, .false.)
    else
      root%left => single_rotate(root%left, .true.)
    end if

    old => single_rotate(root, is_left)

  end function double_rotate

  recursive function redblack_assert(root) result(black_count)
    type(redblack_node_t), pointer :: root
    integer :: black_count

    integer :: left_hand, right_hand

    black_count = 0
    left_hand = 0
    right_hand = 0

    if (.not. associated(root)) then
      black_count = 1
      return
    end if

    ! Consecutive red links
    if (is_red(root)) then
      if (is_red(root%left) .or. is_red(root%right)) then
        print*, "Red violation"
        return
      end if
    end if

    left_hand = redblack_assert(root%left)
    right_hand = redblack_assert(root%right)

    ! Invalid binary search tree
    if (associated(root%left)) then
      if (root%left%val >= root%val) then
        print*, "Binary tree violation"
        return
      end if
    end if
    if (associated(root%right)) then
      if (root%right%val <= root%val) then
        print*, "Binary tree violation"
        return
      end if
    end if

    ! Black height mismatch
    if (left_hand /= 0 .and. right_hand /= 0&
         & .and. left_hand /= right_hand) then
      print*, "Black violation"
      return
    end if

    ! Only count black lines
    if (left_hand /= 0 .and. right_hand /= 0) then
      if (is_red(root)) then
        black_count = left_hand
      else
        black_count = left_hand + 1
      end if
    end if

  end function redblack_assert

  subroutine tree_add(this, val)
    class(redblack_tree_t), intent(inout) :: this
    integer, intent(in) :: val

    call tree_add_at_node(this%root, val)
    this%root%red = .false.

    this%size = this%size + 1
  end subroutine tree_add

  recursive subroutine tree_add_at_node(node, val)
    type(redblack_node_t), pointer :: node
    integer, intent(in) :: val

    type(redblack_node_t), pointer :: node_dir, node_antidir

    if (.not. associated(node)) then
      allocate(node)
      node%val = val
      node%red = .true.
      return
    end if

    if (val < node%val) then
      call tree_add_at_node(node%left, val)
      node_dir => node%left
      node_antidir => node%right
    else
      call tree_add_at_node(node%right, val)
      node_dir => node%right
      node_antidir => node%left
    end if

    ! Check for violations
    if (is_red(node_dir)) then
      if (is_red(node_antidir)) then
        node%red = .true.
        node%left%red = .false.
        node%right%red = .false.
      else
        if (val < node%val) then
          if (is_red(node_dir%left)) then
            node => single_rotate(node, is_left=.false.)
          else if (is_red(node_dir%right)) then
            node => single_rotate(node, is_left=.false.)
          end if
        else
          if (is_red(node_dir%right)) then
            node => single_rotate(node, is_left=.true.)
          else if (is_red(node_dir%right)) then
            node => double_rotate(node, is_left=.true.)
          end if
        end if
      end if
    end if

  end subroutine tree_add_at_node

  function find(this, val)
    class(redblack_tree_t), intent(in) :: this
    integer, intent(in) :: val
    type(redblack_node_t), pointer :: find

    find => null()

    if (.not. associated(this%root)) then
      return
    end if

    find => find_at_node(this%root, val)
  end function find

  recursive function find_at_node(node, val) result(found)
    type(redblack_node_t), pointer :: node
    integer, intent(in) :: val
    type(redblack_node_t), pointer :: found

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
    type(redblack_tree_t), intent(inout) :: this

    if (associated(this%root)) then
      call delete_node(this%root)
      deallocate(this%root)
    end if

  end subroutine delete_tree

  recursive subroutine delete_node(node)
    type(redblack_node_t), intent(inout) :: node

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
    class(redblack_tree_t), intent(in) :: this

    if (.not. associated(this%root)) then
      return
    end if

    call print_node(this%root)
    print*, ""
  end subroutine print_tree

  recursive subroutine print_node(node)
    type(redblack_node_t), intent(in) :: node

    if (associated(node%left)) then
      call print_node(node%left)
    end if

    write(*, '(i0, a)', advance='no') node%val, " "

    if (associated(node%right)) then
      call print_node(node%right)
    end if
  end subroutine print_node

  subroutine pretty_print_tree(this)
    class(redblack_tree_t), intent(in) :: this

    if (.not. associated(this%root)) then
      return
    end if

    call pretty_print_node(this%root, 1)
  end subroutine pretty_print_tree

  recursive subroutine pretty_print_node(node, depth)
    type(redblack_node_t), intent(in) :: node
    integer, intent(in) :: depth

    character(len=5) :: red_or_black
    character(len=22) :: depth_format

    if (associated(node%right)) then
      call pretty_print_node(node%right, depth + 1)
    end if

    if (node%red) then
      red_or_black = "red"
    else
      red_or_black = "black"
    end if

    write(depth_format, '(a, i0, a)') "(", depth,&
         & '("  "), i0, a, a, a)'

    write(*, trim(depth_format)) node%val, " (", trim(red_or_black), ") "

    if (associated(node%left)) then
      call pretty_print_node(node%left, depth + 1)
    end if

  end subroutine pretty_print_node

  function get_values_tree(this) result(vals)
    class(redblack_tree_t), intent(in) :: this
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
    type(redblack_node_t), intent(in) :: node
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
    type(redblack_node_t), intent(in) :: node
    integer :: min

    if (.not. associated(node%left)) then
      min = node%val
    else
      min = node_min_value(node%left)
    end if
  end function node_min_value

  function tree_remove(this, val) result(removed)
    class(redblack_tree_t), intent(inout) :: this
    integer, intent(in) :: val
    logical :: removed

    type(redblack_node_t), pointer :: removed_node => null()

    if (.not. associated(this%root)) then
      removed = .false.
      return
    end if

    if (this%root%val == val) then
      block
        type(redblack_node_t), target :: dummy_root
        type(redblack_node_t), pointer :: dummy_root_p

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
    type(redblack_node_t), pointer :: this
    type(redblack_node_t), pointer :: parent
    integer :: val

    type(redblack_node_t), pointer :: removed_node

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

end module redblack_tree
