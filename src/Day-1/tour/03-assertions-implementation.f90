submodule(assertions_interface) assertions_implementation
  !! author: Damian Rouson
  !! date: 11/4/2018
  !!
  !! Implement support for runtime checking of assertions expected to always be true in normal execution.
  !! To compile, execute
  !!
  !!    caf -c 03-assertions-implementation.f90
  !!
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
              type is(character(len=*))
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
