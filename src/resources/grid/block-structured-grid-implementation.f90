submodule(block_structured_grid_interface) implement_block_structured_grid
  !! author: Damian Rouson
  !! date: 10/29/2018
  !!
  !! Define the grid block structure and global domain
  implicit none

contains

  module procedure input_from_file
    integer :: nx, ny, nz, blocks_unit
    real :: x_lbound, x_ubound, y_lbound, y_ubound, z_lbound, z_ubound

    namelist/num_blocks/ nx, ny, nz
    namelist/global_domain/ x_lbound, x_ubound, y_lbound, y_ubound, z_lbound, z_ubound

    open(newunit=blocks_unit,file=file_name,status="old")
    read(unit=blocks_unit,nml=num_blocks)
    read(unit=blocks_unit,nml=global_domain)
    this%global_block_shape_ = [nx,ny,nz]
    close(blocks_unit)

  end procedure

  module procedure partition
    associate(num_global_blocks=>product(this%global_block_shape_))
    associate(me=>this_image(), ni=>num_images())
    associate(quotient=>ni/num_global_blocks, remainder=>mod(ni,num_global_blocks))
    associate(my_first => sum([ (quotient + overflow(i,ni), i=1,me-1)])
    associate(my_last  => my_first + quotient + merge(1,0,me<=remainder))
      allocate(this%vertices(my_first:my_last))

    end associate; end associate; end associate; end associate
  contains
    pure function overflow(image,numImages) result(extra)
      integer, intent(in) :: image, numImages
      extra = merge
    end function
  end procedure

end submodule
