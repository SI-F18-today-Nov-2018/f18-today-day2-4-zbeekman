submodule(emulated_collectives_interface) emulated_collectives_implementation
  implicit none

contains


    subroutine emulated_co_broadcast(a,source_image)
      integer, intent(inout) :: a
      integer, intent(in) :: source_image

      integer, allocatable :: a_coarray[:]

      allocate(a_coarray[*],source=a)

      if (source_image /= 1) a_coarray[1] = a_coarray[source_image]

      associate(me=>this_image(),ni=>num_images())

        associate(parent=>me/2)
          if (parent>0) then
            sync images(parent)
            a = a_coarray[parent]
          end if
        end associate

        associate(even_child=>2*me)
          if (even_child<=ni) sync images(even_child)
        end associate

        associate(odd_child=>2*me+1)
          if (odd_child<=ni) sync images(odd_child)
        end associate

      end associate
    end subroutine

    module procedure emulated_co_sum
      integer, allocatable :: a_coarray[:]
      allocate(a_coarray[*])
      a_coarray = a
      associate(me=>this_image(),ni=>num_images())

        associate(even_child=>2*me)
          if (even_child<=ni) then
            sync images(even_child)
            a_coarray = a_coarray + a_coarray[even_child]
          end if
        end associate

        associate(odd_child=>2*me+1)
          if (odd_child<=ni) then
            sync images(odd_child)
            a_coarray = a_coarray + a_coarray[odd_child]
          end if
        end associate

        associate(parent=>me/2)
          if (parent>0) sync images(parent)
        end associate

        call emulated_co_broadcast(a_coarray,source_image=1)
      end associate
      a = a_coarray
    end procedure
end submodule
