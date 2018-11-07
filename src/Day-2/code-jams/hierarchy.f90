module child_type
  implicit none
  private
  public :: child

  type child
    private
    integer :: age_
  contains
    procedure :: set_age
  end type

contains
  subroutine set_age(this,age)
    class(parent), intent(out) :: this
    this%age_ = age
  end subroutine
end module

module parent_type
  use child_type, only : child
  implicit none
  private
  public :: parent

  type, extends(child) :: parent
    private
    type(child), allocatable :: children
  end type

end module
