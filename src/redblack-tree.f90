module redblack_tree

  implicit none

  private
  public :: redblack_node_t, redblack_tree_t
  public :: is_red, redblack_assert
  public :: single_rotate, double_rotate
  public :: pretty_print_tree
  public :: RED, BLACK
  public :: RED_VIOLATION, BLACK_VIOLATION, TREE_VIOLATION

  logical, parameter :: RED = .true.
  logical, parameter :: BLACK = .false.

  type :: redblack_node_t
    type(redblack_node_t), pointer :: left => null()
    type(redblack_node_t), pointer :: right => null()
    logical :: colour = RED

    integer :: val
  end type redblack_node_t

  type :: redblack_tree_t
    type(redblack_node_t), pointer :: root => null()
    integer :: size = 0
  contains
    procedure :: add => tree_add
    procedure :: find => tree_find
    procedure :: print => tree_print
    procedure :: remove => tree_remove
    procedure :: values => tree_get_values
    procedure :: copy => tree_copy
    generic :: assignment(=) => copy
    final :: delete_tree
  end type redblack_tree_t

  integer, parameter :: RED_VIOLATION = -1
  integer, parameter :: BLACK_VIOLATION = -2
  integer, parameter :: TREE_VIOLATION = -3

contains

  function is_red(node)
    !! Returns true if node has been allocated and is red
    type(redblack_node_t), pointer :: node !! The node to check
    logical :: is_red

    is_red = .false.
    if (associated(node)) is_red = (node%colour .eqv. RED)
  end function is_red

  function single_rotate(root, is_left) result(new_root)
    !! Rotate subtree at root and recolour
    type(redblack_node_t), pointer :: root
        !! The start of the subtree
    logical, intent(in) :: is_left
        !! If true, rotate to left; otherwise to the right
    type(redblack_node_t), pointer :: new_root
        !! The new root of the subtree

    if (is_left) then
      new_root => root%right
      root%right => new_root%left
      new_root%left => root
    else
      new_root => root%left
      root%left => new_root%right
      new_root%right => root
    end if

    root%colour = RED
    new_root%colour = BLACK

  end function single_rotate

  function double_rotate(root, is_left) result(new_root)
    !! Rotate twice: first about a child, then at root
    type(redblack_node_t), pointer :: root
        !! The start of the subtree
    logical, intent(in) :: is_left
        !! If true, rotate to left; otherwise to the right
    type(redblack_node_t), pointer :: new_root
        !! The new root of the subtree

    if (is_left) then
      root%right => single_rotate(root%right, .false.)
    else
      root%left => single_rotate(root%left, .true.)
    end if

    new_root => single_rotate(root, is_left)

  end function double_rotate

  function redblack_assert_error_message(error) result(message)
    !! Convert the error code from redblack_assert to a readable
    !! message
    integer, intent(in) :: error !! The error code from redblack_assert
    character(len=:), allocatable :: message

    if (error >= 0) then
      message = "No error"
      return
    end if

    select case(error)
    case(RED_VIOLATION)
      message = "Red violation"
    case(BLACK_VIOLATION)
      message = "Black violation"
    case(TREE_VIOLATION)
      message = "Binary tree violation"
    case default
      message = "Unknown error!"
    end select
  end function redblack_assert_error_message

  function tree_redblack_assert(this) result(black_count)
    !! Recurse down the tree checking the red-black properties all
    !! hold
    class(redblack_tree_t), intent(in) :: this
        !! The tree to check
    integer :: black_count
        !! The black level of the subtree or a negative value if one
        !! of the properties has been violated

    black_count = redblack_assert(this%root)
  end function tree_redblack_assert

  recursive function redblack_assert(root) result(black_count)
    !! Recurse down the subtree checking the red-black properties all
    !! hold
    type(redblack_node_t), pointer :: root
        !! Root of the subtree
    integer :: black_count
        !! The black level of the subtree or a negative value if one
        !! of the properties has been violated

    integer :: left_hand, right_hand

    black_count = 0
    left_hand = 0
    right_hand = 0

    if (.not. associated(root)) then
      ! Leaves are all black
      black_count = 1
      return
    end if

    ! Consecutive red links
    if (is_red(root)) then
      if (is_red(root%left) .or. is_red(root%right)) then
        black_count = RED_VIOLATION
        return
      end if
    end if

    left_hand = redblack_assert(root%left)
    right_hand = redblack_assert(root%right)

    ! Invalid binary search tree
    if (associated(root%left)) then
      if (root%left%val >= root%val) then
        black_count = TREE_VIOLATION
        return
      end if
    end if
    if (associated(root%right)) then
      if (root%right%val <= root%val) then
        black_count = TREE_VIOLATION
        return
      end if
    end if

    ! Black height mismatch
    if (left_hand /= 0 .and. right_hand /= 0&
         & .and. left_hand /= right_hand) then
      black_count = BLACK_VIOLATION
      return
    end if

    ! All good, count black lines
    if (left_hand > 0 .and. right_hand > 0) then
      if (is_red(root)) then
        black_count = left_hand
      else
        black_count = left_hand + 1
      end if
    end if

  end function redblack_assert

  subroutine tree_add(this, val)
    !! Add a value to the tree
    class(redblack_tree_t), intent(inout) :: this
    integer, intent(in) :: val

    call tree_add_at_node(this%root, val)
    this%root%colour = BLACK

    this%size = this%size + 1
  end subroutine tree_add

  recursive subroutine tree_add_at_node(node, val)
    !! Add a value to the subtree
    type(redblack_node_t), pointer :: node
    integer, intent(in) :: val

    type(redblack_node_t), pointer :: node_dir, node_antidir
    type(redblack_node_t), pointer :: node_dir_dir, node_dir_antidir
    logical :: is_left

    if (.not. associated(node)) then
      allocate(node)
      node%val = val
      node%colour = RED
      return
    end if

    if (val == node%val) return

    is_left = (val < node%val)

    if (is_left) then
      call tree_add_at_node(node%left, val)
      node_dir => node%left
      node_antidir => node%right
      node_dir_dir => node_dir%left
      node_dir_antidir => node_dir%right
    else
      call tree_add_at_node(node%right, val)
      node_dir => node%right
      node_antidir => node%left
      node_dir_dir => node_dir%right
      node_dir_antidir => node_dir%left
    end if

    ! Check for violations
    if (is_red(node_dir)) then
      if (is_red(node_antidir)) then
        ! Simple case, red siblings

        if (is_red(node_dir_dir) .or. is_red(node_dir_antidir)) then
          node%colour = RED
          node%left%colour = BLACK
          node%right%colour = BLACK
        end if

      else
        ! More complicated violations

        if (is_red(node_dir_dir)) then
          node => single_rotate(node, .not. is_left)
        else if (is_red(node_dir_antidir)) then
          node => double_rotate(node, .not. is_left)
        end if

      end if
    end if

  end subroutine tree_add_at_node

  function tree_find(this, val)
    class(redblack_tree_t), intent(in) :: this
    integer, intent(in) :: val
    type(redblack_node_t), pointer :: tree_find

    tree_find => null()

    if (.not. associated(this%root)) then
      return
    end if

    tree_find => find_at_node(this%root, val)
  end function tree_find

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

  subroutine tree_print(this)
    class(redblack_tree_t), intent(in) :: this

    if (.not. associated(this%root)) then
      return
    end if

    call node_print(this%root)
    print*, ""
  end subroutine tree_print

  recursive subroutine node_print(node)
    type(redblack_node_t), intent(in) :: node

    if (associated(node%left)) then
      call node_print(node%left)
    end if

    write(*, '(i0, a)', advance='no') node%val, " "

    if (associated(node%right)) then
      call node_print(node%right)
    end if
  end subroutine node_print

  subroutine pretty_print_tree(this)
    class(redblack_tree_t), intent(in) :: this

    if (.not. associated(this%root)) then
      return
    end if

    print*, ""
    print*, "------------------------------"
    call pretty_print_node(this%root, 1)
    print*, "------------------------------"
  end subroutine pretty_print_tree

  recursive subroutine pretty_print_node(node, depth)
    type(redblack_node_t), intent(in) :: node
    integer, intent(in) :: depth

    character(len=5) :: red_or_black

    integer :: i

    if (associated(node%right)) then
      call pretty_print_node(node%right, depth + 1)
    end if

    if (node%colour .eqv. RED) then
      red_or_black = "red"
    else
      red_or_black = "black"
    end if

    do i = 0, depth
      write(*, '(a)', advance='no') " "
    end do

    write(*, '(i0, a, a, a)') node%val, " (", trim(red_or_black), ") "

    if (associated(node%left)) then
      call pretty_print_node(node%left, depth + 1)
    end if

  end subroutine pretty_print_node

  function tree_get_values(this) result(vals)
    class(redblack_tree_t), intent(in) :: this
    integer, allocatable :: vals(:)

    integer :: index

    allocate(vals(this%size))

    if (.not. associated(this%root)) then
      return
    end if

    index = 0
    call node_get_values(this%root, vals, index)
  end function tree_get_values

  recursive subroutine node_get_values(node, vals, index)
    type(redblack_node_t), intent(in) :: node
    integer, allocatable, intent(inout) :: vals(:)
    integer, intent(inout) :: index

    if (associated(node%left)) then
      call node_get_values(node%left, vals, index)
    end if

    index = index + 1
    vals(index) = node%val

    if (associated(node%right)) then
      call node_get_values(node%right, vals, index)
    end if
  end subroutine node_get_values

  function tree_remove(this, val) result(removed)
    class(redblack_tree_t), intent(inout) :: this
    integer, intent(in) :: val
    logical :: removed
    logical :: done

    type(redblack_node_t), pointer :: removed_node => null()

    integer :: data

    if (.not. associated(this%root)) then
      removed = .false.
      return
    end if

    data = val
    done = .false.
    this%root => node_remove(this%root, removed_node, data, done)

    if (associated(removed_node)) then
      deallocate(removed_node)
      removed_node => null()
      removed = .true.
      this%size = this%size - 1
    else
      removed = .false.
    end if

  end function tree_remove

  recursive function node_remove(root, removed_node, val, done) &
       & result(new_root)
    type(redblack_node_t), pointer :: root, new_root
    integer, intent(inout) :: val
    logical, intent(inout) :: done

    type(redblack_node_t), pointer :: removed_node
    type(redblack_node_t), pointer :: heir
    logical :: is_left

    removed_node => null()

    if (.not. associated(root)) then
      done = .true.
      new_root => root
      return
    end if

    if (root%val == val) then
      if (.not. associated(root%left) &
          .or. .not. associated(root%right)) then
        if (associated(root%left)) then
          new_root => root%left
        else
          new_root => root%right
        end if

        if (is_red(root)) then
          done = .true.
        else if (is_red(new_root)) then
          new_root%colour = BLACK
          done = .true.
        end if

        removed_node => root
        return
      else
        heir => root%left

        do while (associated(heir%right))
          heir => heir%right
        end do

        root%val = heir%val
        val = heir%val
      end if
    end if

    is_left = root%val < val

    if (is_left) then
      root%right => node_remove(root%right, removed_node, val, done)
    else
      root%left => node_remove(root%left, removed_node, val, done)
    end if

    if (.not. done) then
      root => node_remove_balance(root, is_left, done)
    end if

    new_root => root

  end function node_remove

  function node_remove_balance(root, is_left, done) &
       result(new_root)
    type(redblack_node_t), pointer :: root
    logical, intent(in) :: is_left
    logical, intent(inout) :: done

    type(redblack_node_t), pointer :: new_root

    type(redblack_node_t), pointer :: parent, sibling, temp
    logical :: save_colour, is_new_root

    parent => root
    if (is_left) then
      sibling => root%right
    else
      sibling => root%left
    end if

    ! Reduce red sibling case to black sibling case
    if (is_red(sibling)) then
      root => single_rotate(root, is_left)

      if (is_left) then
        sibling => parent%right
      else
        sibling => parent%left
      end if
    end if

    if (associated(sibling)) then
      ! Black sibling cases
      if (.not. is_red(sibling%left) &
           .and. .not. is_red(sibling%right)) then
        if (is_red(parent)) then
          done = .true.
        end if

        parent%colour = BLACK
        sibling%colour = RED
      else
        save_colour = parent%colour
        is_new_root = associated(root, target=parent)

        if (is_left) then
          temp => sibling%right
        else
          temp => sibling%left
        end if

        if (is_red(temp)) then
          parent => single_rotate(parent, is_left)
        else
          parent => double_rotate(parent, is_left)
        end if

        parent%colour = save_colour
        parent%left%colour = BLACK
        parent%right%colour = BLACK

        if (is_new_root) then
          root => parent
        else
          if (is_left) then
            root%left => parent
          else
            root%right => parent
          end if
        end if

        done = .true.
      end if
    end if

    new_root => root

  end function node_remove_balance

  subroutine tree_copy(lhs, rhs)
    class(redblack_tree_t), intent(inout) :: lhs
    class(redblack_tree_t), intent(in) :: rhs

    ! Probably don't need this, but not clear if gfortran finalises
    ! lhs of assignment yet
    call delete_tree(lhs)

    lhs%size = rhs%size
    lhs%root => node_copy(rhs%root)
  end subroutine tree_copy

  recursive function node_copy(rhs) result(lhs)
    type(redblack_node_t), pointer :: lhs
    type(redblack_node_t), pointer :: rhs

    if (.not. associated(rhs)) then
      lhs => null()
      return
    end if

    allocate(lhs, source=rhs)

    lhs%val = rhs%val

    lhs%left => node_copy(rhs%left)
    lhs%right => node_copy(rhs%right)
  end function node_copy

end module redblack_tree
