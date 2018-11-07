submodule(structured_grid_interface) structured_grid_implementation
  use assertion_interface, only : assert, assertions
  implicit none

contains

    module procedure get_grid_resolution

      integer, parameter :: success=0, reader=1
      integer nx, ny, nz, file_unit, stat
      integer n(space_dimension)
      namelist/grid_resolution/ nx, ny, nz

      if (this_image()==reader) then
        open(newunit=file_unit,file=file_name,iostat=stat)
        call assert(stat==success,"file opened sucessfully")

        read(unit=file_unit,nml=grid_resolution,iostat=stat)
        call assert(stat==success,"file read sucessfully")

        resolution = [nx,ny,nz]

      end if

      call co_broadcast(resolution,source_image=reader)

    end procedure

    module procedure allocate_my_partition

       integer alloc_result, image
       integer, parameter :: success=0

      associate(ni=>num_images(), me=>this_image())
      associate(nx=>(resolution(1)),ny=>(resolution(2)),nz=>(resolution(3)))

       if (assertions) then
         !! Requirements
         call assert(nx>=ni,"enough planes to distribute")
       end if

        associate(num_ny_planes=>(nx))
        associate(quotient=>nx/num_images())
        associate(remainder=>mod(nx,num_images()))

        associate(my_first=>1+sum([(quotient+overflow(image,remainder),image=1,me-1)]))
        associate(my_last=>my_first+quotient+overflow(me,remainder)-1)

          allocate(this%x(my_first:my_last,ny,nz,space_dimension)[*],stat=alloc_result)

        end associate; end associate; end associate; end associate; end associate; end associate

        if (assertions) then
          !! Assurances
          call assert(alloc_result==success,"coarray allocation succeeded")
        end if
      end associate

    contains
      pure function overflow(image_number,remainder) result(extra)
        integer, intent(in) :: image_number, remainder
        integer :: extra
        extra = merge(1,0,image_number<=remainder)
      end function

    end procedure

    module procedure my_partition_size
      my_num_points = size(this%x)
    end procedure

end submodule
