    module funcs
    use datatype
    implicit none
    interface operator (.x.)
    module procedure cross_product
    end interface
    contains
    function lowcase(s) result(t)
    ! return string 's' in lowercase
    character(*), intent(in)	::s
    character(len(s))	::t
    integer	::i,diff
    t = s; diff = 65-97
    do i=1,len(t)
        if(ichar(t(i:i))>65.and.ichar(t(i:i))<=90)then
            ! if uppercase, make lowercase
            t(i:i) = char(ichar(t(i:i))-diff)
        endif
    enddo
    end function

    function pointtoplane(point,normal,center) result(t)
    real(8)::point(3),center(3),normal(3)
    real(8)::t
    t =dot_product(point-center,normal)
    if(t/=0)then
        t = t/abs(t)
    endif
    end function


    ! is==0 parallel is==2 lie in the plane is ==1 intersection
    function intersection(lnormal,lpoint,normal,center,t) result(is)
    real(8)::lnormal(3),lpoint(3),normal(3),center(3)
    real(8)::t(3)
    integer::is

    real(8)::d,n,s



    d = dot_product(lnormal,normal)
    n = - dot_product(normal,lpoint - center)
    if(abs(d)<small)then
        if(n==0)then
            is = 2
            return
        else
            is = 0
            return
        endif
    endif

    s = n/d
    if(s<0 .or. s>1)then
        is = 0
        return
    endif

    is = 1
    t = s*lnormal+lpoint
    end function


    function cross_product(a, b)
    real(8), dimension(3) :: cross_product
    real(8), dimension(3), intent(in) :: a, b

    cross_product(1) = a(2)*b(3) - a(3)*b(2)
    cross_product(2) = a(3)*b(1) - a(1)*b(3)
    cross_product(3) = a(1)*b(2) - b(1)*a(2)
    end function cross_product

    function normal_plane(p1,p2,p3)
    real(8), dimension(3),intent(in):: p1,p2,p3
    real(8), dimension(3)::v1,v2,normal_plane
    v1 = p2- p1
    v2 = p3-p1
    normal_plane = cross_product(v1,v2)
    normal_plane = normal_plane / norm2(normal_plane)
    end function normal_plane

    function rot_by_direct(direct,normal,axis)
    real(8), dimension(3),intent(in):: direct,normal
    real(8) :: theta_plane
    real(8),dimension(3,3)::rot_by_direct
    integer::axis,xdir,ydir,zdir,perm(3)

    if(axis==1)then
        xdir = 2
        ydir = 3
    elseif(axis==2)then
        xdir = 1
        ydir = 3
    elseif(axis==3)then
        xdir = 1
        ydir = 2
    endif
    zdir = axis

    if(abs(abs(normal(axis))-1.0)<0.001)then
        !如果是面方向与轴向相同，则绕x轴旋转
        theta_plane = atan2(normal(zdir),normal(ydir))
        theta_plane = -theta_plane

        rot_by_direct(xdir,xdir) = 1.
        rot_by_direct(xdir,ydir) = 0.
        rot_by_direct(xdir,zdir) = 0.

        rot_by_direct(ydir,xdir) = 0.
        rot_by_direct(ydir,ydir) = cos(theta_plane)
        rot_by_direct(ydir,zdir) = -sin(theta_plane)

        rot_by_direct(zdir,xdir) = 0.
        rot_by_direct(zdir,ydir) = sin(theta_plane)
        rot_by_direct(zdir,zdir) = cos(theta_plane)
        return
    endif

    theta_plane = atan2(direct(ydir),direct(xdir))
    if(theta_plane<0)theta_plane=theta_plane + 2.*pi
    theta_plane = - theta_plane

    rot_by_direct(xdir,xdir) = cos(theta_plane)
    rot_by_direct(xdir,ydir) = -sin(theta_plane)
    rot_by_direct(xdir,zdir) = 0.

    rot_by_direct(ydir,xdir) = sin(theta_plane)
    rot_by_direct(ydir,ydir) = cos(theta_plane)
    rot_by_direct(ydir,zdir) = 0.

    rot_by_direct(zdir,xdir) = 0.
    rot_by_direct(zdir,ydir) = 0.
    rot_by_direct(zdir,zdir) = 1.



    end function rot_by_direct

    function rot_plane(normal,axis)
    real(8), dimension(3),intent(in):: normal
    real(8) :: theta_plane
    real(8),dimension(3,3)::rot_plane
    integer::axis,xdir,ydir,zdir,perm(3)

    if(axis==1)then
        xdir = 2
        ydir = 3
    elseif(axis==2)then
        xdir = 1
        ydir = 3
    elseif(axis==3)then
        xdir = 1
        ydir = 2
    endif
    zdir = axis

    if(abs(normal(axis)-1.0)<small)then
        !如果是面方向与轴向相同，则绕x轴旋转
        theta_plane = atan2(normal(zdir),normal(ydir))
        theta_plane = -theta_plane

        rot_plane(xdir,xdir) = 1.
        rot_plane(xdir,ydir) = 0.
        rot_plane(xdir,zdir) = 0.

        rot_plane(ydir,xdir) = 0.
        rot_plane(ydir,ydir) = cos(theta_plane)
        rot_plane(ydir,zdir) = sin(theta_plane)

        rot_plane(zdir,xdir) = 0.
        rot_plane(zdir,ydir) = -sin(theta_plane)
        rot_plane(zdir,zdir) = cos(theta_plane)
        return
    endif

    theta_plane = atan2(normal(ydir),normal(xdir))
    if(theta_plane<0)theta_plane=theta_plane + 2.*pi
    theta_plane = theta_plane - pi/2.
    theta_plane = -theta_plane

    rot_plane(xdir,xdir) = cos(theta_plane)
    rot_plane(xdir,ydir) = sin(theta_plane)
    rot_plane(xdir,zdir) = 0.

    rot_plane(ydir,xdir) = -sin(theta_plane)
    rot_plane(ydir,ydir) = cos(theta_plane)
    rot_plane(ydir,zdir) = 0.

    rot_plane(zdir,xdir) = 0.
    rot_plane(zdir,ydir) = 0.
    rot_plane(zdir,zdir) = 1.

    end function rot_plane

    function RotMatrix(r,axis)
    real(8) r(3)
    real(8) RotMatrix(3,3)
    integer axis

    integer	idimn
    real(8)	unitvec(3),dotvec(3)
    real(8)	c,s,x,y,z

    r(axis) = 0
    !x,y,z 旋转轴
    x = 0;y = 0;z = 0
    if(axis==1)then
        c = r(2)
        s = r(3)
        x = 1
    elseif(axis ==2)then
        c = r(1)
        s = -r(3)
        y = 1
    elseif(axis ==3)then
        c = r(1)
        s = r(2)
        z = 1
    endif


    RotMatrix(1,1)=c+(1-c)*x*x
    RotMatrix(1,2)=(1-c)*x*y+s*z
    RotMatrix(1,3)=(1-c)*x*z-s*y
    RotMatrix(2,1)=(1-c)*x*y-s*z
    RotMatrix(2,2)=c+(1-c)*y*y
    RotMatrix(2,3)=(1-c)*y*z+s*x
    RotMatrix(3,1)=(1-c)*z*x+s*y
    RotMatrix(3,2)=(1-c)*z*y-s*x
    RotMatrix(3,3)=c+(1-c)*z*z
    !r = matmul(RotMatrix,r)
    end function RotMatrix

    function inv(a)
    use lapack95
    integer::ipiv(3)
    real(8),dimension(3,3)::a,inv
    inv = a
    call getrf(inv,ipiv)
    call getri(inv,ipiv)
    end function inv

    function solve(a,b)
    use lapack95
    real(8)::a(3,3),b(3),solve(3)
    call gesv(a,b)
    solve = b
    end function solve
    end module