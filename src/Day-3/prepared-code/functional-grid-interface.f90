module functional_grid_interface
  use structured_grid_interface, only : structured_grid
  implicit none

  private
  public :: functional_grid

  type, extends(structured_grid) :: functional_grid
  end type
end module
