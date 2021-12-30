    program demo_floor
    implicit none
    real :: x = 63.29
    real :: y = -63.59
        print *, x, floor(x)
        print *, y, floor(y)
       ! elemental
       print *,floor([ &
       &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
       &  0.0,   &
       &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])
    end program demo_floor
