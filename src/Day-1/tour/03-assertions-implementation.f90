submodule(assertions_interface) assertions_implementation
  implicit none

contains
    module procedure assert
      if (.not. assertion) then
        block
          character(len=:), allocatable :: error_message
          integer, parameter :: max_digits=12, max_data_length=1024
          character(len=max_digits) my_image
          character(len=max_data_length) my_data
          write(my_image,*) this_image()
          error_message = description // " failed on image " // adjustl(trim(my_image))
          if (present(diagnostic_data))  then
            select type(diagnostic_data)
              type is(integer)
                write(my_data,*) diagnostic_data
                error_message = error_message // "with diagnostic data " // adjustl(trim(my_data))
              class default
                error_message = error_message // "with unknown diagnostic data type"
            end select
          end if
          error stop error_message
         end block
      end if
    end procedure

end submodule assertions_implementation
