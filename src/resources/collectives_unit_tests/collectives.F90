! Copyright (c) 2012-2014, Sourcery, Inc.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!     * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!     * Redistributions in binary form must reproduce the above copyright
!       notice, this list of conditions and the following disclaimer in the
!       documentation and/or other materials provided with the distribution.
!     * Neither the name of Sourcery, Inc., nor the
!       names of any other contributors may be used to endorse or promote products
!       derived from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL SOURCERY, INC., BE LIABLE 
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

! This program performs a parallel calculation of pi using a collective sum that
! has been manually coded to have the same interface as the Fortran 2015 co_sum
module collectives_module 
  use iso_fortran_env, only : real64,int32 
  implicit none               

  ! Generic interface to integer and real binary-tree collective sums 
  interface co_sum_binary_tree
     procedure co_sum_binary_tree_int32,co_sum_binary_tree_real64
  end interface

  ! Generic interface to integer and real collective broadcasts
  interface co_broadcast_binary_tree
     procedure co_broadcast_binary_tree_int32,co_broadcast_binary_tree_real64
  end interface

  ! Generic interface to integer and real recursive-doubling collective sums
  interface co_sum_recursive_doubling
     procedure co_sum_recursive_doubling_int32,co_sum_recursive_doubling_real64
  end interface

#ifdef TAU
  interface
   pure subroutine tau_pure_start(x)
     character(len=*), intent(in):: x
   end subroutine tau_pure_start 
   pure subroutine tau_pure_stop(x)
     character(len=*), intent(in):: x
   end subroutine tau_pure_stop 
  end interface
#endif

contains

  ! Copy the local value of image "source_image" to all other images
  subroutine co_broadcast_binary_tree_int32(a,source_image)
    integer(int32), intent(inout) :: a[*]
    integer(int32), intent(in) :: source_image
    integer(int32) :: k
    k = this_image()    
    associate(parent=>k/2)
      if (parent>0) then
        sync images(parent)
        a=a[parent]
      else
        a=a[source_image]
      end if
    end associate
    associate(even_child=>2*k,odd_child=>2*k+1)
      if (even_child<=num_images()) sync images(even_child)
      if (odd_child<=num_images()) sync images(odd_child)
    end associate
  end subroutine

  ! Replace each image's local element with the global sum across all elements
  subroutine co_sum_binary_tree_int32(a)
    integer(int32), intent(inout) :: a[*]
    integer(int32) :: k
    k = this_image()    
    associate(even_child=>2*k)
      if (even_child<=num_images()) then 
        sync images(even_child)
        a=a+a[even_child]
      end if
    end associate
    associate(odd_child=>2*k+1)
      if (odd_child<=num_images()) then
        sync images(odd_child)
        a=a+a[odd_child]
      end if
    end associate
    associate(parent=>k/2)
      if (parent>0) then
        sync images(parent)
      end if
    end associate
    sync all
    call co_broadcast_binary_tree(a,source_image=1_int32)
  end subroutine

  ! Copy the local value of image "source_image" to all other images
  subroutine co_broadcast_binary_tree_real64(a,source_image)
    real(real64), intent(inout) :: a[*]
    integer(int32), intent(in) :: source_image
    integer(int32) :: k
    k = this_image()    
    associate(parent=>k/2)
      if (parent>0) then
        sync images(parent)
        a=a[parent]
      else
        a=a[source_image]
      end if
    end associate
    associate(even_child=>2*k,odd_child=>2*k+1)
      if (even_child<=num_images()) sync images(even_child)
      if (odd_child<=num_images()) sync images(odd_child)
    end associate
  end subroutine

  ! Replace each image's local element with the global sum across all elements
  subroutine co_sum_recursive_doubling_real64(a)
    real(real64), intent(inout) :: a[*]
    integer(int32) :: me,n,contributor,calculator
    me = this_image()    
    n = num_images() 
    do while(n>1 .and. me<=n)
      contributor=me+n/2
      if (contributor<=n) then 
        sync images(contributor)
        a=a+a[contributor]
      end if
      calculator = me-n/2
      if (calculator>0) sync images(calculator)
      n=n/2
    end do
    sync all
    call co_broadcast_binary_tree(a,source_image=1_int32)
  end subroutine

  ! Replace each image's local element with the global sum across all elements
  subroutine co_sum_recursive_doubling_int32(a)
    integer(int32), intent(inout) :: a[*]
    integer(int32) :: me,n,contributor,calculator
    me = this_image()    
    n = num_images() 
    do while(n>1 .and. me<=n)
      contributor=me+n/2
      if (contributor<=n) then 
        sync images(contributor)
        a=a+a[contributor]
      end if
      calculator = me-n/2
      if (calculator>0) sync images(calculator)
      n=n/2
    end do
    sync all
    call co_broadcast_binary_tree(a,source_image=1_int32)
  end subroutine

  ! Replace each image's local element with the global sum across all elements
  subroutine co_sum_binary_tree_real64(a)
    real(real64), intent(inout) :: a[*]
    integer(int32) :: k
    k = this_image()    
    associate(even_child=>2*k)
      if (even_child<=num_images()) then 
        sync images(even_child)
        a=a+a[even_child]
      end if
    end associate
    associate(odd_child=>2*k+1)
      if (odd_child<=num_images()) then
        sync images(odd_child)
        a=a+a[odd_child]
      end if
    end associate
    associate(parent=>k/2)
      if (parent>0) then
        sync images(parent)
      end if
    end associate
    sync all
    call co_broadcast_binary_tree(a,source_image=1_int32)
  end subroutine

end module
