submodule(emulated_collectives_interface) emulated_collectives_implementation
  implicit none

contains
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

        call co_broadcast(a_coarray,source_image=1)
      end associate
      a = a_coarray
    end procedure
end submodule
