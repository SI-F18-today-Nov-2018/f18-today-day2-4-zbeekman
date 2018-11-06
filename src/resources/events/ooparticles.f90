module expeller_module
  implicit none
  type expeller
    integer, allocatable :: expelled(:) ! particle indices this image expels
  end type  
end module expeller_module

! Exchange particles across images using Fortran 2015 events and coarrays
program main 
  use expeller_module, only : expeller
  implicit none        
  real, allocatable :: x(:)[:]           ! coarray of 1D particle positions 
  integer, allocatable :: expelled(:)[:] ! particle indices this image expels
  integer :: me,images,mine,i            ! declare local variables
  ! # particles, exchange buffer size, expelled-particle indicial spacing 
  integer, parameter :: num_particles=1024,buffer=256,step=8  
  type(expeller) :: bin[*]
  me = this_image()                      ! my image number                  
  images = num_images()                  ! total number of executing images
  ! Require even spread of particles and of expelled-particle indices 
  if (mod(num_particles,images)/=0 .or. mod(num_particles,step)/=0) &
    error stop "not evenly divisible"
  mine = num_particles/images            ! number of particles this image owns
  allocate(x(mine+buffer)[*],source=0.)  ! start particles at the origin
  ! Set expelled-particle indices, spaced every "step-th" particle for testing
  allocate(expelled(mine/step)[*],source=[(i,i=1,mine,step)]) 
  allocate(bin%expelled(mine/step),source=[(i,i=1,mine,step)]) 
  exchange_particles: block
    use iso_fortran_env, only : event_type
    integer, allocatable :: keepers(:),source_indices(:)
    integer :: destination,source,source_size !src/dest image #'s, # expelled
    type(event_type), save :: particles_ready[*],particles_transferred[*]
    destination=merge(me+1,1,me<num_images()) ! set downstream destination 
    event post(particles_ready[destination])  ! signal my destination image
    keepers = [(merge(i,0,.not.any(i==expelled)),i=1,mine)] 
    keepers = pack(keepers,keepers/=0)     ! eliminate expelled indices
    source  = merge(me-1,images,me>1)      ! set upstream source
    event wait(particles_ready)            ! await signal from source
    source_indices = [expelled(:)[source]] ! get source's expelled-index list
    source_size = size(source_indices)     ! count source's expelled particles
    if (mine+source_size>size(x)) error stop "i'm full" ! global halt
    x(mine+1:mine+source_size)= x([source_indices])[source] ! copy from source 
    event post(particles_transferred[source]) ! signal source image
    event wait(particles_transferred)         ! await destination's signal
    ! Overwrite my particles with my keepers & my source's expelled particles
    x(1:size(keepers)+source_size) = [x(keepers),x(mine+1:mine+source_size)] 
    mine = size(keepers) + source_size        ! adjust my particle count
  end block exchange_particles
end program
