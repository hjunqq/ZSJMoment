    !  TestRot.f90
    !
    !  FUNCTIONS:
    !  TestRot - Entry point of console application.
    !

    !****************************************************************************
    !
    !  PROGRAM: TestRot
    !
    !  PURPOSE:  Entry point for the console application.
    !
    !****************************************************************************

    program TestRot
    implicit none

    ! Variables

    real(8)::theta_x,theta_y,theta_z,tcos,tsin
    real(8)::normal(3),rx(3,3),ry(3,3),rz(3,3),r(3,3)

    normal = (/1.0,1.0,1.0/)
    normal = normal / norm2(normal)

    !row--x
    theta_x = atan2(normal(3),normal(2))
    tcos = cos(theta_x)
    tsin = sin(theta_x)
    rx =reshape((/1.0d0, 0.0d0, 0.0d0, &
                  0.0d0,  tcos,  tsin, &
                  0.0d0, -tsin,  tcos/),(/3,3/))

    !pitch--y
    theta_y = atan2(normal(3),normal(1))
    tcos = cos(theta_y)
    tsin = sin(theta_y)
    ry =reshape((/1.0d0, 0.0d0, 0.0d0, &
                  0.0d0,  tcos,  tsin, &
                  0.0d0, -tsin,  tcos/),(/3,3/))
    !yaw--z
    theta_z = atan2(normal(2),normal(1))
    tcos = cos(theta_z)
    tsin = sin(theta_z)
    rz =reshape((/1.0d0, 0.0d0, 0.0d0, &
                0.0d0,  tcos,  tsin, &
                0.0d0, -tsin,  tcos/),(/3,3/))
    
    r = matmul(rx,matmul(ry,rz))
    
    write(*,100)normal
    write(*,100)matmul(r,(/0.0,0.0,1.0/))
    write(*,101)r


100 format('V:',X,3F10.7)
101 format('M:',X,3F10.7) 
    end program TestRot

