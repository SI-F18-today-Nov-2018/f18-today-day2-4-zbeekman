submodule(partition_interface) partition_impmenetation
   implicit none

   integer, parameter :: space_dimension=3

contains

  module procedure input_from_file
    integer file_unit, stat
    integer :: n(space_dimension), integer
    integer, parameter :: reader=1, success=0

    integer nx, ny, nz

    namelist/grid_resolution/ nx, ny, nz

    associate(me=>this_image())
    if (me==reader) then
      open(newunit=file_unit,file=file_name,status='old',iostat=stat)
      call assert(stat==success,"file opened successfully")

      read(unit=file_unit,nml=grid_resolution,iostat=stat)
      call assert(stat==success,"file read successfully")
    end if
    end associate

    n=[nx,ny,nz]
    call co_broadcast(n,source_image=reader)
    nx=n(1); ny=n(2); nz=n(3)

    this%global_grid_shape_=n
    close(unit=file_unit)
  end procedure

  module procedure allocate_my_partition

    integer image, remainder
    associate( me => this_image() )
    associate( ni => num_images() )
    associate( num_grids => this%global_grid_shape_(1))

    call assert( ni<=num_grids, "enough grids to distribute to images")

    associate( quotient => num_grids/ni )
    associate( my_first => 1 + sum([(quotient+overflow(image,remainder),image=1,me-1)]) )
    associate( my_last => my_first + quotient + overflow(me,remainder) - 1 )

      allocate( this%x(my_first:my_last,this%global_grid_shape_(2),this%global_grid_shape_(3),space_dimension) )

    end associate; end associate; end associate; end associate; end associate; end associate

  contains

    pure function overflow(image,remainder) result(filler)
      integer, intent(in) :: image,remainder
      integer :: filler
      filler = merge(1,0,image<=remainder)
    end function

  end procedure

end submodule
