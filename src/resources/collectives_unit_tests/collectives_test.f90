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

! Unit tests for co_broadcast_binary_tree, co_sum_binary_tree, and co_sum_recursive_doubling
program main
  use iso_fortran_env, only : real64,int32,error_unit ! 64-bit arithmetic
  use collectives_module, only : co_broadcast_binary_tree,co_sum_binary_tree,co_sum_recursive_doubling
  implicit none               
  integer(int32) :: me[*]

  ! Store the executing image number
  me=this_image()

  ! Verify broadcasting of integer data from image 1
  int32_co_broadcast_binary_tree: block 
    integer(int32), save :: integer_received[*]
    integer(int32), parameter :: integer_sent=12345 ! Integer test message
    if (me==1) integer_received=integer_sent
    call co_broadcast_binary_tree(integer_received,source_image=1)
    if (integer_received/=integer_sent) then
      write(error_unit,*) "Incorrect co_broadcast_binary_tree (",integer_received,") on image",me
      error stop "Test failed." ! Halt all images
    end if
  end block int32_co_broadcast_binary_tree

  ! Verify broadcasting of real data from image 1
  real64_co_broadcast_binary_tree: block 
    real(real64), save :: real_received[*]
    real(real64), parameter :: real_sent=2.7182818459045_real64 ! Real test message
    if (me==1) real_received=real_sent
    call co_broadcast_binary_tree(real_received,source_image=1)
    if (real_received/=real_sent) then
      write(error_unit,*) "Incorrect co_broadcast_binary_tree (",real_received,") on image",me
      error stop "Test failed." ! Halt all images
    end if
  end block real64_co_broadcast_binary_tree

  ! Verify recursive-doubling collective sum of integer data by tallying image numbers
  int32_co_sum_recursive_doubling: block 
    integer(int32) :: i
    integer(int32), save :: image_number_tally[*]
    image_number_tally=me
    call co_sum_recursive_doubling(image_number_tally)
    if (image_number_tally/=sum([(i,i=1,num_images())])) then
      write(error_unit,"(2(a,i2))") "Wrong result (",image_number_tally,") on image",image_number_tally
      error stop "Test failed." ! Halt all images
    end if
    print *, "Test passed on image ",me
  end block int32_co_sum_recursive_doubling

  ! Verify binary-tree collective sum by calculuating pi
  real64_co_sum_binary_tree: block 
    real(real64), parameter :: four=4._real64,one=1._real64,half=0.5_real64
    real(real64), save :: pi[*],pi_local
    integer(int32) :: i,points_per_image
    integer(int32), parameter :: resolution=1024_int32 ! Number of points used in pi calculation
    ! Partition the calculation evenly across all images
    if (mod(resolution,num_images())/=0) error stop "number of images doesn't evenly divide into number of points"
    points_per_image=resolution/num_images()
    associate(n=>resolution,my_first=>points_per_image*(me-1)+1,my_last=>points_per_image*me)
      pi_local = sum([ (four/(one+((i-half)/n)**2),i=my_first,my_last) ])/n
    end associate
    pi=pi_local
    sync all
    ! Replace pi on each image with the sum of the pi contributions from all images
    call co_sum_binary_tree(pi)
    associate (pi_ref=>acos(-1._real64),allowable_fractional_error=>0.000001_real64)
      if (abs((pi-pi_ref)/pi_ref)>allowable_fractional_error) then
        write(error_unit,*) "Inaccurate pi (",pi,") result on image ",me
        error stop
      end if
    end associate
  end block real64_co_sum_binary_tree

  ! Verify binary-tree collective sum by calculuating pi
  real64_co_sum_recursive_doubling: block 
    real(real64), parameter :: four=4._real64,one=1._real64,half=0.5_real64
    real(real64), save :: pi[*],pi_local
    integer(int32) :: i,points_per_image
    integer(int32), parameter :: resolution=1024_int32 ! Number of points used in pi calculation
    ! Partition the calculation evenly across all images
    if (mod(resolution,num_images())/=0) error stop "number of images doesn't evenly divide into number of points"
    points_per_image=resolution/num_images()
    associate(n=>resolution,my_first=>points_per_image*(me-1)+1,my_last=>points_per_image*me)
      pi_local = sum([ (four/(one+((i-half)/n)**2),i=my_first,my_last) ])/n
    end associate
    pi=pi_local
    sync all
    ! Replace pi on each image with the sum of the pi contributions from all images
    call co_sum_recursive_doubling(pi)
    associate (pi_ref=>acos(-1._real64),allowable_fractional_error=>0.000001_real64)
      if (abs((pi-pi_ref)/pi_ref)>allowable_fractional_error) then
        write(error_unit,*) "Inaccurate pi (",pi,") result on image ",me
        error stop
      end if
    end associate
  end block real64_co_sum_recursive_doubling 

end program
