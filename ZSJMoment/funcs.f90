    module funcs
    use datatype
    implicit none

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
    end module