    !  zsjmoment.f90
    !
    !  functions:
    !  zsjmoment - entry point of console application.
    !

    !****************************************************************************
    !
    !  program: zsjmoment
    !
    !  purpose:  entry point for the console application.
    !
    !****************************************************************************

    program zsjmoment

    use datatype
    use funcs


    implicit none

    ! variables

    ! body of zsjmoment

    inpunit = 10
    open(inpunit,file='inp')
    read(inpunit,*)text
    read(inpunit,*)fpath
    read(inpunit,*)text
    read(inpunit,*)pcenter
    read(inpunit,*)text
    read(inpunit,*)angular
    read(inpunit,*)text
    read(inpunit,*)restype


    mshunit=1
    resunit=2
    cutunit=3
    chkunit=4
    open(chkunit,file=fpath(1:len_trim(fpath))//".cut.chk")

    !读取网格信息
    open(mshunit,file=fpath(1:len_trim(fpath))//".flavia.msh")

    call readmsh

    close(mshunit)

    !切面

    open(cutunit,file=fpath(1:len_trim(fpath))//".cut")

    call readcut

    close(cutunit)

    call defelemedge

    call cutplane

    open(resunit,file=fpath(1:len_trim(fpath))//".flavia.res")

    call resprocess


    close(resunit)

    contains
    !>读取网格信息
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! 输出 group、node、elem 全局变量，存储网格信息
    subroutine readmsh
    character(150)   ::orgline,text
    integer         ::idxi,idxj,igroup,icoor,ielem,idim

    ismeshgroup = .false.
    isoldformat = .true.
    ngroup=0
    ncoor=0
    nelem=0
    ngroup=0
    !将坐标和单元的头尾节点指针置空
    nullify(grouphead)
    nullify(grouplast)
    nullify(coorhead)
    nullify(coorlast)
    nullify(elemhead)
    nullify(elemlast)
    do
        !读取一行数据
        read(mshunit,'(a150)',end=100)orgline
        orgline = adjustl(orgline)                            !for old format
        read(orgline(1:index(orgline,' ')),'(a20)')text
        !取数据的第一个单词
        select case(lowcase(trim(text)))
        case('#')
            print *,"read a comment"
            isoldformat = .false.
        case('group')
            print *,"this result is in group"
            ismeshgroup = .true.
            isoldformat = .false.
        case('end')
            print *,"end group"
            goto 100
        case('mesh')
            !开始读取单元组节点信息
            ngroup=ngroup+1
            !为单元组分配内存
            allocate(pgroup)
            pgroup%index=ngroup

            if(.not.isoldformat)then
                idxi=index(orgline,'mesh')+len("mesh")+2          !去掉双引号，所以要多加一个字符
                idxj=index(orgline,'dimension')-1-2                !去掉双引号，需要多减一个字符
                read(orgline(idxi:idxj),"(a70)")pgroup%groupname

                idxi=index(orgline,'dimension')+len("dimension")
                idxj=index(orgline,'elemtype')-1
                read(orgline(idxi:idxj),"(i3)")pgroup%dim

                idxi=index(orgline,'elemtype')+len("elemtype")
                idxj=index(orgline,'nnode')-1
                read(orgline(idxi:idxj),"(a70)")pgroup%elemtype

                idxi=index(orgline,'nnode')+len("nnode")
                idxj=len_trim(orgline)
                read(orgline(idxi:idxj),"(i3)")pgroup%nnode
            else
                write(pgroup%groupname,*)ngroup

                idxi=index(orgline,'dimension')+len("dimension")
                idxj=index(orgline,'elemtype')-1
                read(orgline(idxi:idxj),*)pgroup%dim

                idxi=index(orgline,'elemtype')+len("elemtype")
                idxj=index(orgline,'nnode')-1
                read(orgline(idxi:idxj),*)pgroup%elemtype

                idxi=index(orgline,'nnode')+len("nnode")
                idxj=len_trim(orgline)
                read(orgline(idxi:idxj),*)pgroup%nnode

            endif



            write(*,*)"**************"
            write(*,*)"mesh          ",pgroup%groupname(1:len_trim(pgroup%groupname))
            write(*,*)"dimension     ",pgroup%dim
            write(*,*)"elemtype      ",pgroup%elemtype(1:len_trim(pgroup%elemtype))
            write(*,*)"nnode         ",pgroup%nnode

            !将读取到的单元组放到链表的结尾
            pgroup%next=>null()
            if(associated(grouplast))then
                grouplast%next=>pgroup
                grouplast=>pgroup
            else
                grouphead=>pgroup
                grouplast=>pgroup
            endif
        case("coordinates")
            print *,"begine to read coordinates"
            do
                read(mshunit,'(a150)',end=100)orgline
                if(isoldformat)orgline = adjustl(orgline)
                read(orgline(1:index(orgline,' ')),'(a20)')text
                select case(lowcase(trim(text)))
                case("end")
                    exit
                    case default
                    ncoor=ncoor+1
                    allocate(pcoor)
                    allocate(pcoor%val(pgroup%dim))
                    read(orgline,*)pcoor%index,pcoor%val
                    if(ncoor/=pcoor%index)stop 'coor error!'
                    pcoor%next=>null()
                    if(associated(coorlast))then
                        coorlast%next=>pcoor
                        coorlast=>pcoor
                    else
                        coorhead=>pcoor
                        coorlast=>pcoor
                    endif
                end select
                pgroup.ncoor=ncoor
            enddo
        case("elements")
            print *,"begine to read elements"
            ielem=0
            do
                read(mshunit,'(a150)',end=100)orgline
                if(isoldformat)orgline = adjustl(orgline)
                read(orgline(1:index(orgline,' ')),'(a20)')text
                select case(lowcase(trim(text)))
                case("end")
                    exit
                    case default
                    nelem=nelem+1
                    ielem=ielem+1
                    allocate(pelem)
                    allocate(pelem%node(pgroup%nnode))
                    read(orgline,*)pelem%index,pelem%node,pelem%group
                    !if(nelem/=pelem.index)stop 'elem error!'
                    pelem%next=>null()
                    if(associated(elemlast))then
                        elemlast%next=>pelem
                        elemlast=>pelem
                    else
                        elemhead=>pelem
                        elemlast=>pelem
                    endif
                end select
                pgroup%nelem=ielem
            enddo
            !为单元组节点中的单元列表分配内存，然后将单元列表指针指向单元信息地址
            allocate(pgroup%elem(ielem))
            pelem=>elemhead
            ielem=0
            do while(associated(pelem))
                if(pelem%group .eq. pgroup%index)then
                    ielem=ielem+1
                    pgroup%elem(ielem)=pelem
                endif
                pelem=>pelem%next
            enddo
            print *,"there're ",ielem,"elem in this group"
        end select
    enddo

    !将链表转换成动态数组格式，以方便以后读取
    !为单元组分配内存
100 allocate(group(ngroup))
    !从链表的头开始依次将值赋予数组
    pgroup=>grouphead
    igroup=0
    do while(associated(pgroup))
        igroup=igroup+1
        group(igroup)%dummy => pgroup
        pgroup=>pgroup%next
    enddo
    allocate(coor(ncoor))
    pcoor=>coorhead
    icoor=0
    do while(associated(pcoor))
        icoor=icoor+1
        coor(icoor)=pcoor
        pcoor=>pcoor%next
    enddo
    allocate(elem(nelem))
    pelem=>elemhead
    ielem=0
    do while(associated(pelem))
        ielem=ielem+1
        elem(ielem)=pelem
        pelem=>pelem%next
    enddo
    ndim = 0
    do igroup = 1,ngroup
        ndim = max(ndim,group(igroup)%dummy%dim)
    enddo
    end subroutine


    !>读取切面信息
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine readcut
    implicit none

    character(150)  ::orgline,text
    integer         ::i

    nullify(cuthead)
    nullify(cutlast)

    read(cutunit,*)text
    read(cutunit,*)ncut

    allocate(cut(ncut))

    do i=1,ncut

        read(cutunit,*)text

        read(cutunit,*)cut(i)%index,cut(i)%dir,cut(i)%cuttype,cut(i)%interval

        allocate(cut(i)%appeargroup(ngroup))

        read(cutunit,*)cut(i)%appeargroup
        read(cutunit,*)cut(i)%spoint,cut(i)%epoint

        write(*,*)"cuttype       ",cut(i)%cuttype
        write(*,*)"interval      ",cut(i)%interval
        write(*,*)

    enddo


    endsubroutine readcut

    !> lineinfo
    subroutine defelemedge
    implicit none
    integer::ntype
    real(8)::cnst3,s,t,st
    integer::idim,igaus

    ntype = 1
    allocate(elemlibs(ntype))
    elemlibs(1)%index = 9
    elemlibs(1)%nline = 12
    elemlibs(1)%ndim = 2
    allocate(elemlibs(1)%line(12))

    elemlibs(1)%line(1)%index = 1
    elemlibs(1)%line(1)%node(1) = 1
    elemlibs(1)%line(1)%node(2) = 5

    elemlibs(1)%line(2)%index = 1
    elemlibs(1)%line(2)%node(1) = 2
    elemlibs(1)%line(2)%node(2) = 6

    elemlibs(1)%line(3)%index = 1
    elemlibs(1)%line(3)%node(1) = 3
    elemlibs(1)%line(3)%node(2) = 7

    elemlibs(1)%line(4)%index = 1
    elemlibs(1)%line(4)%node(1) = 4
    elemlibs(1)%line(4)%node(2) = 8

    elemlibs(1)%line(5)%index = 1
    elemlibs(1)%line(5)%node(1) = 1
    elemlibs(1)%line(5)%node(2) = 2

    elemlibs(1)%line(6)%index = 1
    elemlibs(1)%line(6)%node(1) = 5
    elemlibs(1)%line(6)%node(2) = 6

    elemlibs(1)%line(7)%index = 1
    elemlibs(1)%line(7)%node(1) = 8
    elemlibs(1)%line(7)%node(2) = 7

    elemlibs(1)%line(8)%index = 1
    elemlibs(1)%line(8)%node(1) = 4
    elemlibs(1)%line(8)%node(2) = 3

    elemlibs(1)%line(9)%index = 1
    elemlibs(1)%line(9)%node(1) = 1
    elemlibs(1)%line(9)%node(2) = 4

    elemlibs(1)%line(10)%index = 1
    elemlibs(1)%line(10)%node(1) = 2
    elemlibs(1)%line(10)%node(2) = 3

    elemlibs(1)%line(11)%index = 1
    elemlibs(1)%line(11)%node(1) = 6
    elemlibs(1)%line(11)%node(2) = 7

    elemlibs(1)%line(12)%index = 1
    elemlibs(1)%line(12)%node(1) = 5
    elemlibs(1)%line(12)%node(2) = 8


    ! Gause Point

    allocate(elemlibs(1)%posgp(2,4))
    allocate(elemlibs(1)%weigp(4))

    elemlibs(1)%ngaus = 4

    cnst3 = 1./3.**0.5
    elemlibs(1)%posgp(1,1)= -cnst3
    elemlibs(1)%posgp(2,1)= -cnst3
    elemlibs(1)%posgp(1,2)=  cnst3
    elemlibs(1)%posgp(2,2)= -cnst3
    elemlibs(1)%posgp(1,3)=  cnst3
    elemlibs(1)%posgp(2,3)=  cnst3
    elemlibs(1)%posgp(1,4)= -cnst3
    elemlibs(1)%posgp(2,4)=  cnst3

    elemlibs(1)%weigp(1)=1.00
    elemlibs(1)%weigp(2)=1.00
    elemlibs(1)%weigp(3)=1.00
    elemlibs(1)%weigp(4)=1.00

    ! Shape Fun
    allocate(elemlibs(1)%shapefun(4,4))
    allocate(elemlibs(1)%deriv(4,4,4))

    do idim = 1, 2
        do igaus = 1, 4


            s = elemlibs(1)%posgp(1,igaus)
            t = elemlibs(1)%posgp(2,igaus)

            st = s*t
            elemlibs(1)%shapefun(1,igaus) = (1-t-s+st)*0.25
            elemlibs(1)%shapefun(2,igaus) = (1-t+s-st)*0.25
            elemlibs(1)%shapefun(3,igaus) = (1+t+s+st)*0.25
            elemlibs(1)%shapefun(4,igaus) = (1+t-s-st)*0.25
            elemlibs(1)%deriv(1,1,igaus) = (-1+t)*0.25
            elemlibs(1)%deriv(1,2,igaus) = (+1-t)*0.25
            elemlibs(1)%deriv(1,3,igaus) = (+1+t)*0.25
            elemlibs(1)%deriv(1,4,igaus) = (-1-t)*0.25
            elemlibs(1)%deriv(2,1,igaus) = (-1+s)*0.25
            elemlibs(1)%deriv(2,2,igaus) = (-1-s)*0.25
            elemlibs(1)%deriv(2,3,igaus) = (+1+s)*0.25
            elemlibs(1)%deriv(2,4,igaus) = (+1-s)*0.25
        enddo
    enddo


    endsubroutine defelemedge

    !>切面
    subroutine cutplane()
    implicit none

    integer         ::i,j,k
    integer         :: igroup,isec,nodeidx,inode,dir,nfaceelem
    integer         ::ielem, jelem, jnode, iline, tnode,iface
    integer         ::nodei,nodej
    type(coorinfo)      ::coori,coorj

    integer,allocatable::elemcross(:)

    real(8)::planenormal(3), centcoord(3), ispoint(3), axisvec(3), secvec(3), elemcenter(3),evec(3)

    integer::is,xdir,ydir

    integer::distance

    real::secvalue,theta, etheta


    write(*,*)"in cut plane..."

    allocate(pelem)

    allocate(pelemlib)


    axisvec = (/0.0,sqrt(1-angular**2),angular/)


    nwcoor = ncoor
    nwelem = nelem


    do i=1,ncut





        isec = 0
        iface = 0
        dir = cut(i)%dir


        nullify(facehead)
        nullify(facelast)




        do
            isec = isec +1

            nullify(coorhead)
            nullify(coorlast)
            nullify(elemhead)
            nullify(elemlast)

            !初始化单元
            !do ielem = 1,nelem
            !    elem(ielem)%ifcross=0
            !enddo

            do igroup = 1,ngroup
                do ielem=1,group(igroup)%dummy%nelem
                    pelem => group(igroup)%dummy%elem(ielem)
                    pelem%ifcross=0
                enddo
            enddo


            ! find intersection element
            !如果cuttype==1，沿y轴切面，那么planenormal为面法向，centcoord为面中心点
            !面中心点的算法应该是在起始点和终止点中间插值
            !如果cuttype==2，沿环向切面，那么planenormal为轴向与环向的叉乘，centcoor为面中心点
            !此时interval为弧度，表示切割的角度
            if(cut(i)%cuttype==1)then

                centcoord = cut(i)%interval*isec / norm2(cut(i)%spoint - cut(i)%epoint) * (cut(i)%epoint - cut(i)%spoint) + cut(i)%spoint(dir)
                secvalue = centcoord(dir)
                planenormal = axisvec

            elseif(cut(i)%cuttype==2)then
                centcoord = cut(i)%spoint
                secvalue = centcoord(dir)

                theta = cut(i)%interval*(isec-1)
                if(dir==2)then
                    xdir=1
                    ydir=3
                endif
                secvec(xdir) = cos(theta)
                secvec(ydir) = sin(theta)
                planenormal = axisvec .x. secvec
            endif




            nfaceelem = 0


            allocate(pcoor)
            do igroup = 1, ngroup
                if(cut(i)%appeargroup(igroup)==0)cycle
                do ielem = 1, group(igroup)%dummy%nelem
                    pelem => group(igroup)%dummy%elem(ielem)

                    if(.not.allocated(pelem%cross))then
                        allocate(pelem%cross(group(igroup)%dummy%nnode))
                    endif

                    pelem%cross=0

                    distance = 0
                    elemcenter = 0.

                    do inode = 1,group(igroup)%dummy%nnode
                        nodeidx = pelem%node(inode)
                        pcoor = coor(nodeidx)
                        elemcenter = elemcenter + pcoor%val
                        pelem%cross(inode) = pointtoplane(pcoor%val,planenormal,centcoord)
                        distance = distance + pelem%cross(inode)
                    enddo



                    if(abs(distance)/=group(igroup)%dummy%nnode)then

                        if(cut(i)%cuttype==2)then
                            elemcenter = elemcenter / group(igroup)%dummy%nnode
                            evec = elemcenter-centcoord

                            etheta = atan2(evec(ydir),evec(xdir))

                            if(etheta<0)etheta = etheta + 2*pi

                            if(abs(etheta-theta)>pi/6)cycle
                        endif

                        pelem%ifcross=1
                        !write(chkunit,*)pelem%index
                        nfaceelem = nfaceelem + 1
                    endif
                enddo
            enddo

            !print*,nfaceelem
            !!write(chkunit,*)nfaceelem,"-------------------------"
            if(cut(i)%cuttype==1)then
                if(secvalue>cut(i)%epoint(dir))then
                    exit
                endif
            else
                if(theta>2*PI)then
                    exit
                endif
            endif

            if(nfaceelem==0)cycle

            allocate(pface)
            iface = iface +1
            pface%index = iface
            pface%dir = dir
            pface%cpoint(dir) = secvalue
            pface%theta = theta
            pface%cuttype = cut(i)%cuttype

            ! generate intersection node

            jelem = 0
            tnode = 0
            do igroup = 1, ngroup
                if(cut(i)%appeargroup(igroup)==0)cycle
                do ielem = 1, group(igroup)%dummy%nelem
                    pelem => group(igroup)%dummy%elem(ielem)
                    if(pelem%ifcross == 1)then
                        jelem = jelem +1
                        jnode = 0

                        pelemlib = elemlibs(1)

                        do iline = 1, pelemlib%nline

                            nodei = pelemlib%line(iline)%node(1)
                            nodej = pelemlib%line(iline)%node(2)

                            coori = coor(pelem%node(nodei))
                            coorj = coor(pelem%node(nodej))


                            is = intersection(coorj%val-coori%val,coori%val,planenormal,centcoord,ispoint)
                            select case(is)
                            case(0)
                                !不相交，没关系
                            case(1)
                                !相交
                                jnode = jnode +1
                                allocate(pcoor)
                                pcoor%val=ispoint
                                pcoor%index = jnode + tnode
                                pcoor%relate=(/coori%index,coorj%index/)
                                pcoor%next=>null()


                                if(associated(coorlast))then
                                    coorlast%next=>pcoor
                                    coorlast=>pcoor
                                else
                                    coorhead=>pcoor
                                    coorlast=>pcoor
                                endif
                            case(2)
                                !在面上,两个点都需要添加到列表中
                                jnode = jnode + 1
                                allocate(pcoor)
                                pcoor%val = coori%val
                                pcoor%index = jnode + tnode
                                pcoor%relate = (/coori%index,coorj%index/)
                                pcoor%next =>null()
                                if(associated(coorlast))then
                                    coorlast%next => pcoor
                                    coorlast=>pcoor
                                else
                                    coorhead => pcoor
                                    coorlast =>pcoor
                                endif

                                jnode = jnode + 1
                                allocate(pcoor)
                                pcoor%val = coorj%val
                                pcoor%index = jnode + tnode
                                pcoor%relate = (/coori%index,coorj%index/)
                                pcoor%next =>null()
                                if(associated(coorlast))then
                                    coorlast%next => pcoor
                                    coorlast=>pcoor
                                else
                                    coorhead => pcoor
                                    coorlast =>pcoor
                                endif

                            end select
                            !if(is==1)then
                            !    !print '(2I5,3(F7.3,","),x,3(F7.3,","),x,3(F7.3,","))',coori.index,coorj.index,coori.val,coorj.val,ispoint
                            !    jnode = jnode +1
                            !    allocate(pcoor)
                            !    pcoor%val=ispoint
                            !    pcoor%index = jnode + tnode
                            !    pcoor%relate=(/coori%index,coorj%index/)
                            !    pcoor%next=>null()
                            !
                            !
                            !    if(associated(coorlast))then
                            !        coorlast%next=>pcoor
                            !        coorlast=>pcoor
                            !    else
                            !        coorhead=>pcoor
                            !        coorlast=>pcoor
                            !    endif
                            !
                            !endif


                        enddo

                        tnode = tnode + jnode
                        allocate(piselem)
                        allocate(piselem%node(jnode))
                        piselem%index = jelem
                        piselem%dir = dir
                        piselem%node = (/(k,k=(tnode-jnode+1),tnode)/)

                        piselem%next=>null()

                        if(associated(elemlast))then
                            elemlast%next => piselem
                            elemlast=>piselem
                        else
                            elemhead => piselem
                            elemlast => piselem
                        endif

                    endif
                enddo
            enddo

            ! 删除重复点
            call deldupnode(coorhead,elemhead)


            pcoor=>coorhead
            tnode = 0
            do while(associated(pcoor))
                tnode = tnode +1
                pcoor=>pcoor%next
            enddo

            allocate(pface%coor(tnode))
            pface%nnode = tnode


            pcoor=>coorhead
            tnode = 0
            do while(associated(pcoor))
                tnode = tnode +1
                pface%coor(tnode)%dummy => pcoor
                pcoor=>pcoor%next
            enddo

            piselem => elemhead
            ielem = 0
            do while(associated(piselem))
                ielem = ielem + 1
                piselem => piselem%next
            enddo

            pface%nelem = ielem
            allocate(pface%elem(ielem))
            piselem => elemhead
            ielem = 0
            do while(associated(piselem))
                ielem = ielem +1
                pface%elem(ielem)%dummy => piselem
                piselem => piselem%next
            enddo

            call resortvertix(pface)

            call getcentroid(pface)


            if(associated(facelast))then
                facelast%next => pface
                facelast => pface
            else
                facehead=>pface
                facelast=>pface
            endif



            print *,iface, secvalue,ielem,inode

        enddo

        allocate(cut(i)%face(isec))
        pface=>facehead
        iface = 0
        do while(associated(pface))
            iface = iface +1
            cut(i)%face(iface) = pface
            pface => pface%next
        enddo

        cut(i)%nface = iface


    enddo



    end subroutine cutplane


    subroutine deldupnode(coorhead,elemhead)
    implicit none
    type(coorinfo),pointer::picoor,pjcoor,coorhead,pkcoor

    type(eleminfo),pointer::elemhead,pielem,pjelem
    integer::tnode,inode,nnode,telem
    integer::iindex,jindex
    integer,allocatable::nodemap(:),reorder(:),inodemap(:),jnodemap(:)
    real(8)::dist

    tnode = 0
    picoor=>coorhead
    do while(associated(picoor))
        tnode = tnode +1
        picoor=>picoor%next
    enddo

    allocate(nodemap(tnode))
    allocate(reorder(tnode))
    nodemap = 0
    picoor=>coorhead
    do while(associated(picoor))
        iindex = picoor%index
        nodemap(iindex)=iindex
        picoor=>picoor%next
    enddo

    picoor=>coorhead
    do while (associated(picoor))
        iindex = picoor%index
        if(nodemap(iindex)/=iindex)then
            picoor=>picoor%next
            cycle
        endif
        pjcoor=>picoor%next
        do while(associated(pjcoor))

            jindex = pjcoor%index

            if(nodemap(jindex)/=jindex)then
                pjcoor=>pjcoor%next
                cycle
            endif

            dist = NORM2(picoor%val-pjcoor%val)
            if(dist<small)then

                pjcoor%dupnode = picoor%index

                nodemap(jindex) = iindex

            endif
            pjcoor=>pjcoor%next
        enddo
        picoor=>picoor%next
    enddo

    nnode = 0
    do inode = 1, tnode
        if(nodemap(inode)==inode)then
            nnode = nnode +1
            reorder(inode) = nnode
        endif
    enddo

    !单元重排，并删除重复单元
    pielem=>elemhead
    do while(associated(pielem))
        !write(chkunit,"(10I10)")pielem%index,pielem%node
        pielem%node = reorder(nodemap(pielem%node))
        !write(chkunit,"(10I10)")pielem%index,pielem%node
        pielem=>pielem%next
    enddo

    !删除重复单元
    pielem => elemhead
    telem = 0
    allocate(inodemap(tnode))
    allocate(jnodemap(tnode))
    do while(associated(pielem))
        inodemap = 0
        inodemap(pielem%node) = 1

        pjelem => pielem%next
        do while(associated(pjelem))
            jnodemap = 0
            jnodemap(pjelem%node) = 1

            if(sum(abs(inodemap-jnodemap))==0)then
                pielem%next=>pjelem%next
                deallocate(pjelem)
                pjelem=>pielem%next
                cycle
            endif

            pjelem=>pjelem%next
        enddo


        pielem=>pielem%next
    enddo


    picoor=>coorhead

    do while(associated(picoor))

        pjcoor=>picoor%next
        if(.not.associated(pjcoor))then
            picoor=>picoor%next
            cycle
        endif
        if(pjcoor%dupnode/=0)then

            picoor%next=>pjcoor%next

            deallocate(pjcoor)

            cycle

        endif

        picoor=>picoor%next
    enddo

    picoor => coorhead

    nodemap = 0
    tnode = 0
    do while(associated(picoor))
        tnode = tnode + 1
        nodemap(tnode) = tnode + nwcoor
        !write(chkunit,"(10I10)"),picoor%index , tnode + nwcoor
        picoor%index = tnode + nwcoor
        picoor=>picoor%next
    enddo
    nwcoor = nwcoor + tnode
    write(*,100) nwcoor




    pielem=>elemhead
    telem = 0
    do while(associated(pielem))
        telem = telem +1
        !write(chkunit,100)pielem%index,pielem%node
        pielem%node = nodemap(pielem%node)
        !write(chkunit,100)pielem%index,pielem%node
        pielem%index = telem + nwelem
        pielem=>pielem%next
    enddo



    nwelem = nwelem + telem

100 format(20I10)

    end subroutine deldupnode

    subroutine resortvertix(pface)
    implicit none
    type(faceinfo),pointer::pface

    type(coorinfopointer),pointer::fcoor(:)
    type(eleminfopointer),pointer::felem(:)



    integer::ielem,ncelem,nccoor,nenode
    integer::inode,jnode,knode,mnode,inodeidx,jnodeidx,idim,igaus
    integer::xdir,ydir

    integer::ordered(20),nodetag(20),enodemap(20),is
    real(8)::p1(3),p2(3),p3(3),v(3),theta(20),elcod(3,4),normal(3),rot(3,3),center(3),projp1(3),p(3,4),direct(3)

    fcoor=>pface%coor
    felem=>pface%elem

    ncelem = ubound(felem,1)
    nccoor = ubound(fcoor,1)

    do ielem = 1,ncelem
        pelem => felem(ielem)%dummy
        nenode = ubound(pelem%node,1)
        enodemap = 0
        enodemap(1:nenode) = pelem%node

        ! 删除重复节点
        nodetag = -1
        do inode = 1,nenode
            nodetag(inode) = 0
        enddo
        do inode = 1, nenode -1
            do jnode = inode+1, nenode
                if(pelem%node(inode)==pelem%node(jnode))then
                    nodetag(jnode)=1
                endif
            enddo
        enddo

        knode = count(nodetag==0)
        pelem%enodes = knode

        !找三个点，计算面法向
        knode = 0
        do inode =1, nenode
            if(nodetag(inode)==0)then
                knode = knode +1
                p(:,knode) = fcoor(pelem%node(inode)-nwcoor+nccoor)%dummy%val
            endif
        enddo
        !write(chkunit,"(4(3F10.7,/))")p
        normal = normal_plane(p(:,1),p(:,2),p(:,3))
        center = sum(p,2)/knode
        direct = center

        rot = rot_by_direct(direct,normal,pelem%dir)

        if(pelem%dir==2)then
            xdir=1
            ydir=3
        endif


        ordered = 0

        ordered(1) = pelem%node(1)
        nodetag(1) = 1

        do inode = 1,nenode-1
            inodeidx = ordered(inode)-nwcoor+nccoor
            p1 = fcoor(inodeidx)%dummy%val
            theta = 0.
            do jnode = 1,nenode
                if(nodetag(jnode)==1)cycle
                jnodeidx = pelem%node(jnode)-nwcoor+nccoor
                p2 = fcoor(jnodeidx)%dummy%val
                v = p2-p1
                v = matmul(rot,v)
                !is = intersection(normal,v,normal,center,v)
                theta(jnode) = atan2(v(2),v(1))
            enddo
            if(all(abs(theta)<pi/2))then
                theta=theta+2*pi
            else
                where(theta<0)
                    theta=theta+2*pi
                endwhere
            endif
            mnode = minloc(theta,1,mask=nodetag==0)
            ordered(inode+1) = pelem%node(mnode)
            nodetag(mnode) = 1
        enddo

        pelem%node = ordered


        ! Calculate Jacobi

        !首先计算单元法向
        !p1 = fcoor(ordered(1)-nwcoor+nccoor)%dummy%val
        !p2 = fcoor(ordered(2)-nwcoor+nccoor)%dummy%val
        !p3 = fcoor(ordered(3)-nwcoor+nccoor)%dummy%val
        !normal = normal_plane(p1,p2,p3)
        do inode =1, nenode
            p(:,knode) = fcoor(pelem%node(inode)-nwcoor+nccoor)%dummy%val
        enddo
        normal = normal_plane(p(:,1),p(:,2),p(:,3))


        pelem%normal = normal
        pelem%rot = rot_by_direct(direct,normal,pelem%dir)

        !write(chkunit,*)"debug-----------------"
        !write(chkunit,"(3F10.7)")normal
        !write(chkunit,"(3(3F10.7,/))")rot
        !write(chkunit,*)"--------origin---------"
        do inode = 1,pelem%enodes
            inodeidx = ordered(inode)-nwcoor+nccoor
            p1 = fcoor(inodeidx)%dummy%val
            !write(chkunit,100)inode,p1
        enddo
        !write(chkunit,*)"--------trans---------"
        !计算局部坐标
        do inode = 1,pelem%enodes
            inodeidx = ordered(inode)-nwcoor+nccoor
            p1 = fcoor(inodeidx)%dummy%val
            !projp1 = matmul(rot,p1)
            !write(chkunit,100)inode,projp1
            elcod(:,inode) = p1
        enddo
        write(chkunit,*)"rot:",ielem
        write(chkunit,"(3(3F10.7,/))")rot
        write(chkunit,*)"befor:",ielem
        write(chkunit,102)elcod
        elcod = matmul(rot,elcod)
        write(chkunit,*)"after:",ielem
        write(chkunit,102)elcod
        !normal = normal_plane(elcod(:,1),elcod(:,2),elcod(:,3))
        !if(abs(normal(1)-1)<small)then
        !    elcod(1,:) = elcod(2,:)
        !    elcod(2,:) = elcod(3,:)
        !elseif(abs(normal(2)-1)<small)then
        !    elcod(2,:) = elcod(3,:)
        !endif
        !write(chkunit,*)"--------proje---------"

        !center = p1
        !计算投影点坐标
        !do inode = 1,pelem%enodes
        !    inodeidx = ordered(inode)-nwcoor+nccoor
        !    p1 = fcoor(inodeidx)%dummy%val
        !    is = intersection(normal,p1,normal,center,projp1)
        !    write(chkunit,100)inode,projp1
        !    elcod(:,inode) = projp1
        !enddo

        !do inode =1, pelem%enodes
        !    inodeidx = ordered(inode)-nwcoor+nccoor
        !    p1 = fcoor(inodeidx)%dummy%val
        !    elcod(1,inode) = p1(xdir)
        !    elcod(2,inode) = p1(ydir)
        !enddo

        allocate(pelem%cartd(2,4,4))
        allocate(pelem%djacb(4))
        allocate(pelem%gpcod(2,4))

        do igaus = 1,elemlibs(1)%ngaus
            call jacob(elcod,elemlibs(1)%deriv(:,:,igaus),pelem%cartd(:,:,igaus),pelem%djacb(igaus))
            do idim = 1,elemlibs(1)%ndim
                pelem%gpcod(idim,igaus) = dot_product(elcod(idim,:),elemlibs(1)%shapefun(:,igaus))
            enddo
        enddo

    enddo
100 format(I10,10F10.7)
101 format(10F10.7)
102 format(4(3F10.7,/))
    endsubroutine resortvertix


    subroutine getcentroid(pface)
    implicit none
    type(faceinfo),pointer::pface
    type(eleminfo),pointer::pelem

    type(coorinfopointer),allocatable::fcoor(:)
    type(eleminfopointer),allocatable::felem(:)


    integer::ielem,jelem,telem,igaus,jgaus,tgaus,xdir,ydir

    real(8),allocatable::shapefun(:),center(:)
    real(8)::djacb,area,tarea,normal(3),theta,rot(3,3),fcenter(3),gcenter(3),direct(3)

    fcoor = pface%coor
    felem = pface%elem

    allocate(shapefun(4))
    allocate(center(2))
    tarea = 0.0
    center = 0.0

    normal = 0.0

    do ielem = 1, pface%nelem

        pelem => felem(ielem)%dummy

        area = 0.0
        do igaus = 1, 4
            center = center + pelem%djacb(igaus)*pelem%gpcod(:,igaus)
            area = area + pelem%djacb(igaus)
        enddo
        tarea = tarea + area
        normal = normal + pelem%normal
    enddo

    center = center / tarea
    
    pface%lcenter = center
    
    normal = normal / tarea
    normal = normal / norm2(normal)
    direct = fcoor(1)%dummy%val
    !计算实际坐标

    rot = rot_by_direct(direct,normal,pface%dir)
    !write(chkunit,*)"--------rot---------"
    !write(chkunit,"(3F10.7)")pface%theta,tarea
    !write(chkunit,"(3F10.7)")center
    !write(chkunit,"(3F10.7)")normal
    !write(chkunit,"(3F10.7)")
    !write(chkunit,"(3(3F10.7,/))")rot
    fcenter = 0.
    fcenter(1:2) = center
    gcenter = solve(rot,fcenter)
    !write(chkunit,*)"--------gcenter---------"
    gcenter(pface%dir) = pface%cpoint(pface%dir)
    pface%cpoint = gcenter
    !write(chkunit,"(3F10.7)")pface%cpoint
    !write(chkunit,"(3F10.7)")

    endsubroutine getcentroid

    subroutine jacob(elcod,deriv,cartd,djacb)
    implicit none
    real(8)::cartd(:,:),deriv(:,:),elcod(:,:),xjacm(2,2),xjaci(2,2)
    real(8)::djacb
    integer::idim,jdim,inode

    do idim = 1,2
        do jdim = 1,2
            xjacm(idim,jdim) = 0.0
            do inode = 1,4
                xjacm(idim,jdim) = xjacm(idim,jdim) + deriv(idim,inode)*elcod(jdim,inode)
            enddo
        enddo
    enddo

    djacb = xjacm(1,1)*xjacm(2,2)-xjacm(1,2)*xjacm(2,1)
    xjaci(1,1) =  xjacm(2,2)/djacb
    xjaci(2,2) =  xjacm(1,1)/djacb
    xjaci(1,2) = -xjacm(1,2)/djacb
    xjaci(2,1) = -xjacm(2,1)/djacb

    do idim = 1,2
        do inode = 1, 4
            cartd(idim,inode) = 0.0
            do jdim = 1, 2
                cartd(idim,inode) = cartd(idim,inode) + xjaci(idim,jdim)*deriv(jdim,inode)
            enddo
        enddo
    enddo

    endsubroutine jacob

    !>读取并输出结果信息
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine resprocess
    use gidpost

    character(300)   ::orgline,text,groupname
    integer          ::idxi,idxj,ires,icomp,icoor,istep,idx,ielem,igroup,eidx,icut,iface,tcoor,telem,tlcoor,tlelem
    real            ::curtime
    real,allocatable::time(:)

    integer         ::intval(1),linenode(2)
    type(gid_file) :: fdm, fdr
    real(8):: sxx,syy,szz,sxy,syz,sxz, x,y,z
    type(gid_elementtype):: etype
    type(resinfo)::res
    real:: stress_mat(3,3)



    !写网格文件
    select case(trim(restype))
    case('binary')
        fdr = gid_fopenpostresultfile(fpath(1:len_trim(fpath))//'.post.res',gid_postbinary)
        fdm = fdr
    case('ascii')
        fdm = gid_fopenpostresultfile(fpath(1:len_trim(fpath))//'.post.msh',gid_postascii)
        fdr = gid_fopenpostresultfile(fpath(1:len_trim(fpath))//'.post.res',gid_postascii)
    case('hdf5')
        fdr = gid_fopenpostresultfile(fpath(1:len_trim(fpath))//'.post.res',gid_posthdf5)
        fdm = fdr
    end select

    do igroup = 1, ngroup
        pgroup=>group(igroup)%dummy
        select case(lowcase(trim(pgroup.elemtype)))
        case ('point')
            etype = gid_point
        case ('linear')
            etype = gid_linear
        case ('triangle')
            etype = gid_triangle
        case ('quadrilateral')
            etype = gid_quadrilateral
        case ('tetrahedra')
            etype = gid_tetrahedra
        case ('hexahedra')
            etype = gid_hexahedra
        case ('prism')
            etype = gid_prism
        case ('piramid')
            etype = gid_piramid
        case ('sphere')
            etype = gid_sphere
        case ('circle')
            etype = gid_circle
        end select

        call gid_fbeginmesh(fdm,pgroup.groupname,gid_3d,etype,pgroup.nnode)



        call gid_fbegincoordinates(fdm)

        if(igroup==1)then
            do icoor =1, ncoor
                call gid_fwritecoordinates(fdm,icoor,coor(icoor)%val(1),coor(icoor)%val(2),coor(icoor)%val(3))
            enddo
        endif
        call gid_fendcoordinates(fdm)

        call gid_fbeginelements(fdm)

        do ielem = 1,pgroup.nelem
            call gid_fwriteelement(fdm,pgroup.elem(ielem).index,pgroup.elem(ielem).node)
        enddo
        call gid_fendelements(fdm)

        call gid_fendmesh(fdm)

    enddo

    tcoor = ncoor
    telem = nelem
    do icut = 1, ncut

        do iface = 1,cut(icut)%nface
            etype = gid_quadrilateral
            write(groupname,"('Cut_',I4,'_',I4)")icut,iface
            call gid_fbeginmesh(fdm,groupname,gid_3d,etype,4)

            call gid_fbegincoordinates(fdm)
            do icoor =1, cut(icut)%face(iface)%nnode
                tcoor = tcoor + 1
                pcoor => cut(icut)%face(iface)%coor(icoor)%dummy
                call gid_fwritecoordinates(fdm,tcoor,pcoor%val(1),pcoor%val(2),pcoor%val(3))
            enddo
            call gid_fendcoordinates(fdm)

            call gid_fbeginelements(fdm)
            do ielem = 1,cut(icut)%face(iface)%nelem
                telem = telem +1
                pelem = cut(icut)%face(iface)%elem(ielem)%dummy
                call gid_fwriteelement(fdm,pelem.index,pelem.node)
            enddo
            call gid_fendelements(fdm)
            call gid_fendmesh(fdm)
        enddo
    enddo

    tlcoor = tcoor
    tlelem = telem
    do icut = 1, ncut

        etype = gid_linear
        write(groupname,"('CutLine_',I4)")icut
        call gid_fbeginmesh(fdm,groupname,gid_3d,etype,2)
        call gid_fbegincoordinates(fdm)

        do iface = 1, cut(icut)%nface
            tlcoor = tlcoor + 1
            cut(icut)%face(iface)%cpidx = tlcoor
            call gid_fwritecoordinates(fdm,tlcoor,cut(icut)%face(iface)%cpoint(1),cut(icut)%face(iface)%cpoint(2),cut(icut)%face(iface)%cpoint(3))
        enddo
        call gid_fendcoordinates(fdm)


        call gid_fbeginelements(fdm)
        do iface = 1,cut(icut)%nface - 1
            tlelem = tlelem + 1
            linenode(1) = cut(icut)%face(iface)%cpidx
            linenode(2) = cut(icut)%face(iface+1)%cpidx
            call gid_fwriteelement(fdm,tlelem,linenode)
        enddo
        tlelem = tlelem +1
        linenode(1) = cut(icut)%face(iface)%cpidx
        linenode(2) = cut(icut)%face(1)%cpidx
        call gid_fwriteelement(fdm,tlelem,linenode)
        call gid_fendelements(fdm)
        call gid_fendmesh(fdm)
    enddo

    nres = 0
    nullify(reshead)
    nullify(reslast)



    do
        read(resunit,'(a300)',end=99)orgline
        read(orgline(1:index(orgline,' ')),'(a20)')text
        select case(lowcase(trim(text)))
        case("ongroup")
            print *, "group result"
        case("end")
            goto 99
        case("result")
            nres=nres+1
            allocate(pres)
            pres.index=nres
            pres.next=>null()
            idxi=index(orgline,'"')+1
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(a150)')orgline
            idxi=index(orgline,'"')-1
            read(orgline(1:idxi),'(a70)')pres.resname

            idxi=index(orgline,'"')+3
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(a150)')orgline
            idxi=index(orgline,'"')-1
            read(orgline(1:idxi),'(a70)')pres.ananame

            idxi=index(orgline,'"')+2
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(a150)')orgline
            read(orgline,*)pres.timeana

            idxi=index(orgline,' ')
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(a150)')orgline
            read(orgline,*)pres.restype

            if (pres.restype .eq. 'scalar')then
                pres.nval = 1
            elseif (pres.restype .eq. 'vector')then
                pres.nval = 4
            elseif(pres.restype .eq. 'matrix')then
                pres.nval = 6
            endif
            if(associated(reslast))then
                reslast.next=>pres
                reslast=>pres
            else
                reshead=>pres
                reslast=>pres
            endif
            write(*,*)"resultname           ",pres.resname(1:len_trim(pres.resname))
            write(*,*)"analysisname         ",pres.ananame(1:len_trim(pres.ananame))
            write(*,*)"timeanalysis         ",pres.timeana
            write(*,*)"resulttype           ",pres.restype(1:len_trim(pres.restype))
            write(*,*)
        case("componentnames")
            allocate(pres.compname(pres.nval))
            idxi=index(orgline,'"')+1
            idxj=len_trim(orgline)
            read(orgline(idxi:idxj),'(a150)')orgline
            idxi=index(orgline,'"')-1
            read(orgline(1:idxi),'(a70)')pres.compname(1)
            do icomp=2, pres.nval
                idxi=index(orgline,'"')+1
                idxj=len_trim(orgline)
                read(orgline(idxi:idxj),'(a150)')orgline
                idxi=index(orgline,'"')+1
                idxj=len_trim(orgline)
                read(orgline(idxi:idxj),'(a150)')orgline
                idxi=index(orgline,'"')-1
                read(orgline(1:idxi),'(a70)')pres.compname(icomp)
            enddo
            if(len_trim(orgline)==0)then
                pres.nval= pres.nval-1
            endif
        case("values")
            nullify(pres.valhead)
            nullify(pres.vallast)
            icoor=0
            do
                read(resunit,'(a150)')orgline
                read(orgline(1:index(orgline,' ')),'(a20)')text
                select case(trim(text))
                case("end")
                    exit
                    case default
                    icoor=icoor+1
                    allocate(pres.pval)
                    allocate(pres.pval.dat(pres.nval))
                    read(orgline,*)pres.pval.index,pres.pval.dat
                    if(icoor/=pres.pval.index)stop 'res error!'
                    pres.pval.next=>null()
                    if(associated(pres.vallast))then
                        pres.vallast.next=>pres.pval
                        pres.vallast=>pres.pval
                    else
                        pres.valhead=>pres.pval
                        pres.vallast=>pres.pval
                    endif
                end select
            enddo
            allocate(pres.val(icoor))
            pres.pval=>pres.valhead
            icoor=0
            do while(associated(pres.pval))
                icoor=icoor+1
                pres.val(icoor)=pres.pval
                pres.pval=>pres.pval.next
            enddo
            case default ! for old format
            if(isoldformat)then
                nres=nres+1
                res.index=nres
                res.next=>null()
                read(orgline,*)res.resname,res.loadtype,res.timeana,res.datatype,res.dataloc,res.desccomp

                if(res.datatype==1) then
                    res.nval = 1
                elseif(res.datatype==2)then
                    res.nval = 3
                elseif(res.datatype==3)then
                    res.nval = 6
                endif

                if(res.desccomp>0)then
                    allocate(res.compname(res.nval))
                    do icomp = 1, res.nval
                        read(resunit,*)res.compname(icomp)
                    enddo
                endif

                write(*,*)"resultname           ",res.resname(1:len_trim(res.resname))
                write(*,*)"analysisname         ",res.ananame(1:len_trim(res.ananame))
                write(*,*)"timeanalysis         ",res.timeana
                write(*,*)"resulttype           ",res.datatype
                write(*,*)


                icoor=0
                do
                    read(resunit,'(a300)')orgline
                    read(orgline(1:index(orgline,' ')),'(a20)')text
                    icoor=icoor+1
                    allocate(res.pval)
                    allocate(res.pval.dat(res.nval))
                    read(orgline,*)res.pval.index,res.pval.dat
                    if(icoor/=res.pval.index)stop 'res error!'
                    res.pval.next=>null()
                    if(associated(res.vallast))then
                        res.vallast.next=>res.pval
                        res.vallast=>res.pval
                    else
                        res.valhead=>res.pval
                        res.vallast=>res.pval
                    endif
                    if(icoor==ncoor)exit
                enddo
                allocate(res.val(icoor))
                res.pval=>res.valhead
                icoor=0
                do while(associated(res.pval))
                    icoor=icoor+1
                    res.val(icoor)=res.pval
                    res.pval=>res.pval.next
                enddo

                if(lowcase(res.resname) .eq. 'stress')then
                    nwcoor = ncoor
                    tcoor = ncoor
                    call gid_fbeginresultheader(fdr,'stress','analysis',res.timeana,gid_matrix,gid_onnodes,gid_null)
                    call gid_fresultvalues(fdr)
                    do icut = 1,ncut
                        do iface = 1,cut(icut)%nface
                            call internalforce(res%val,cut(icut)%face(iface))



                            do icoor = 1, cut(icut)%face(iface)%nnode

                                tcoor = tcoor + 1

                                sxx = cut(icut)%face(iface)%res(icoor)%dat(1)
                                syy = cut(icut)%face(iface)%res(icoor)%dat(2)
                                szz = cut(icut)%face(iface)%res(icoor)%dat(3)
                                sxy = cut(icut)%face(iface)%res(icoor)%dat(4)
                                syz = cut(icut)%face(iface)%res(icoor)%dat(5)
                                sxz = cut(icut)%face(iface)%res(icoor)%dat(6)


                                call gid_fwrite3dmatrix(fdr,tcoor,sxx,syy,szz,sxy,syz,sxz)

                            enddo


                        enddo
                    enddo
                    call gid_fendresult(fdr)

                    call gid_fbeginresultheader(fdr,'axial_force','analysis',res.timeana,gid_scalar,gid_onnodes,gid_null)
                    call gid_fresultvalues(fdr)
                    do icut = 1,ncut
                        do iface = 1,cut(icut)%nface
                            call gid_fwritescalar(fdr,cut(icut)%face(iface)%cpidx,cut(icut)%face(iface)%nqm(1))

                        enddo
                    enddo

                    call gid_fendresult(fdr)

                    call gid_fbeginresultheader(fdr,'shear_force_xy','analysis',res.timeana,gid_scalar,gid_onnodes,gid_null)
                    call gid_fresultvalues(fdr)
                    do icut = 1,ncut
                        do iface = 1,cut(icut)%nface
                            call gid_fwritescalar(fdr,cut(icut)%face(iface)%cpidx,cut(icut)%face(iface)%nqm(2))
                        enddo
                    enddo

                    call gid_fendresult(fdr)

                    call gid_fbeginresultheader(fdr,'shear_force_xz','analysis',res.timeana,gid_scalar,gid_onnodes,gid_null)
                    call gid_fresultvalues(fdr)
                    do icut = 1,ncut
                        do iface = 1,cut(icut)%nface
                            call gid_fwritescalar(fdr,cut(icut)%face(iface)%cpidx,cut(icut)%face(iface)%nqm(3))
                        enddo
                    enddo

                    call gid_fendresult(fdr)

                    call gid_fbeginresultheader(fdr,'moment_x','analysis',res.timeana,gid_scalar,gid_onnodes,gid_null)
                    call gid_fresultvalues(fdr)
                    do icut = 1,ncut
                        do iface = 1,cut(icut)%nface
                            call gid_fwritescalar(fdr,cut(icut)%face(iface)%cpidx,cut(icut)%face(iface)%nqm(4))
                        enddo
                    enddo

                    call gid_fendresult(fdr)

                    call gid_fbeginresultheader(fdr,'moment_y','analysis',res.timeana,gid_scalar,gid_onnodes,gid_null)
                    call gid_fresultvalues(fdr)
                    do icut = 1,ncut
                        do iface = 1,cut(icut)%nface
                            call gid_fwritescalar(fdr,cut(icut)%face(iface)%cpidx,cut(icut)%face(iface)%nqm(5))
                        enddo
                    enddo

                    call gid_fendresult(fdr)

                    call gid_fbeginresultheader(fdr,'torsion_xy','analysis',res.timeana,gid_scalar,gid_onnodes,gid_null)
                    call gid_fresultvalues(fdr)
                    do icut = 1,ncut
                        do iface = 1,cut(icut)%nface
                            call gid_fwritescalar(fdr,cut(icut)%face(iface)%cpidx,cut(icut)%face(iface)%nqm(6))
                        enddo
                    enddo

                    call gid_fendresult(fdr)

                endif

                !if(res.resname .eq. 'stress')then
                !
                !    call gid_fbeginresultheader(fdr,'stress','analysis',res.timeana,gid_matrix,gid_onnodes,gid_null)
                !    call gid_fresultvalues(fdr)
                !
                !    do icoor = 1, ncoor
                !
                !        stress_mat(1,1) = res%val(icoor)%dat(1)
                !        stress_mat(2,2) = res%val(icoor)%dat(2)
                !        stress_mat(3,3) = res%val(icoor)%dat(3)
                !        stress_mat(1,2) = res%val(icoor)%dat(4)
                !        stress_mat(2,1) = res%val(icoor)%dat(4)
                !        stress_mat(2,3) = res%val(icoor)%dat(5)
                !        stress_mat(3,2) = res%val(icoor)%dat(5)
                !        stress_mat(1,3) = res%val(icoor)%dat(6)
                !        stress_mat(3,1) = res%val(icoor)%dat(6)
                !
                !        stress_mat = matmul(coor(icoor)%trans,matmul(stress_mat,transpose(coor(icoor)%trans)))
                !
                !        res%val(icoor)%dat(1) = stress_mat(1,1)
                !        res%val(icoor)%dat(2) = stress_mat(2,2)
                !        res%val(icoor)%dat(3) = stress_mat(3,3)
                !        res%val(icoor)%dat(4) = stress_mat(1,2)
                !        res%val(icoor)%dat(5) = stress_mat(2,1)
                !        res%val(icoor)%dat(6) = stress_mat(3,1)
                !
                !        sxx = res.val(icoor).dat(1)
                !        syy = res.val(icoor).dat(2)
                !        szz = res.val(icoor).dat(3)
                !        sxy = res.val(icoor).dat(4)
                !        syz = res.val(icoor).dat(5)
                !        sxz = res.val(icoor).dat(6)
                !
                !
                !        call gid_fwrite3dmatrix(fdr,icoor,sxx,syy,szz,sxy,syz,sxz)
                !
                !    enddo
                !    call gid_fendresult(fdr)
                !
                !elseif(res.resname .eq. 'displacement')then
                !    call gid_fbeginresultheader(fdr,'displacement','analysis',res.timeana,gid_vector,gid_onnodes,gid_null)
                !
                !    call gid_fresultvalues(fdr)
                !
                !    do icoor = 1, ncoor
                !
                !        x = res.val(icoor).dat(1)
                !        y = res.val(icoor).dat(2)
                !        z = res.val(icoor).dat(3)
                !
                !        call gid_fwritevector(fdr,icoor,x, y, z)
                !
                !    enddo
                !    call gid_fendresult(fdr)
                !
                !endif


                res.pval=>res.valhead
                icoor=0
                do while(associated(res.pval))
                    deallocate(res.pval.dat)
                    res.valhead=>res.pval.next
                    deallocate(res.pval)
                    res.pval=>res.valhead
                enddo
                if(res.desccomp>0)then
                    deallocate(res.compname)
                endif
                deallocate(res.val)
                nullify(res.valhead)
                nullify(res.vallast)


            endif

        end select
    enddo

99  call gid_fclosepostresultfile(fdr)
100 format(i10,10(2x,e20.8))
101 format(a15,i8,f12.6,5i8)

    end subroutine resprocess

    subroutine internalforce(resvalue,pface)
    type(resvalinfo),dimension(:),pointer::resvalue
    type(faceinfo) ::pface

    type(coorinfopointer),allocatable::fcoor(:)
    type(eleminfopointer),allocatable::felem(:)


    integer::icoor,ielem,inode,irel,pidx,igaus,dir
    integer::relate(2)
    real(8)::relval(6,2),pval(6),relcod(3,2),elcod(3),nqm(6),vec(3),rot(3,3),stres(3,3),dist(2)
    real(8)::disti,distj,distl

    type(resvalinfo),dimension(:),pointer::fresvalue

    fcoor = pface%coor
    felem = pface%elem

    allocate(fresvalue(pface%nnode))

    ! interpolate
    do icoor = 1, pface%nnode
        pcoor = fcoor(icoor)%dummy
        relate = pcoor%relate
        do irel = 1, 2
            pidx = relate(irel)
            relval(:,irel) = resvalue(pidx)%dat
            relcod(:,irel) = coor(pidx)%val
        enddo

        elcod = pcoor%val
        disti = norm2(elcod - relcod(:,1))
        distj = norm2(elcod - relcod(:,2))
        distl = norm2(relcod(:,2) - relcod(:,1))
        pval = disti/distl*relval(:,1) + distj/distl*relval(:,2)

        allocate(fresvalue(icoor)%dat(6))

        fresvalue(icoor)%dat = pval

    enddo

    pface%res => fresvalue

    !integral
    nqm = 0.
    select case(pface%cuttype)
    case(1)
        if(pface%dir==2)then
            do ielem = 1, pface%nelem
                pelem => felem(ielem)%dummy
                dir = pelem%dir
                rot = pelem%rot
                do igaus = 1, 4
                    pidx = pelem%node(igaus)-nwcoor
                    pval = fresvalue(pidx)%dat
                    nqm(1) = nqm(1) + pelem%djacb(igaus)*pval(2)
                    nqm(2) = nqm(2) + pelem%djacb(igaus)*pval(4)
                    nqm(3) = nqm(3) + pelem%djacb(igaus)*pval(6)
                    vec = - pface%cpoint
                    vec(1) = vec(1) + pelem%gpcod(1,igaus)
                    vec(3) = vec(3) + pelem%gpcod(2,igaus)
                    nqm(4) = nqm(4) + pelem%djacb(igaus)*pval(2)*vec(3)
                    nqm(5) = nqm(5) + pelem%djacb(igaus)*pval(2)*vec(1)
                    nqm(6) = nqm(6) + pelem%djacb(igaus)*pval(5)*norm2(vec)
                enddo
            enddo
        endif
    case(2)
        do ielem = 1, pface%nelem
            pelem => felem(ielem)%dummy
            rot = pelem%rot
            do igaus = 1, 4
                pidx = pelem%node(igaus)-nwcoor
                pval = fresvalue(pidx)%dat
                stres(1,1) = pval(1)
                stres(2,2) = pval(2)
                stres(3,3) = pval(3)
                stres(1,2) = pval(4)
                stres(2,1) = pval(4)
                stres(2,3) = pval(5)
                stres(3,2) = pval(5)
                stres(1,3) = pval(6)
                stres(3,1) = pval(6)
                stres = matmul(rot,matmul(stres,rot))
                pval(1) = stres(1,1)    !xx
                pval(2) = stres(2,2)    !yy
                pval(3) = stres(3,3)    !zz
                pval(4) = stres(1,2)    !xy
                pval(5) = stres(2,3)    !yz
                pval(6) = stres(3,1)    !xz

                nqm(1) = nqm(1) + pelem%djacb(igaus)*pval(3)  !Nxx
                nqm(2) = nqm(2) + pelem%djacb(igaus)*pval(6)  !Sxy
                nqm(3) = nqm(3) + pelem%djacb(igaus)*pval(5)  !Sxz
                dist = pelem%gpcod(:,igaus) - pface%lcenter
                nqm(4) = nqm(4) + pelem%djacb(igaus)*pval(3)*dist(1)   !Mx
                nqm(5) = nqm(5) + pelem%djacb(igaus)*pval(3)*dist(2)   !My
                nqm(6) = nqm(6) + pelem%djacb(igaus)*pval(4)*norm2(dist) !Tyz

            enddo
        enddo
    end select

    pface%nqm = nqm

    nwcoor = nwcoor + pface%nnode

    endsubroutine internalforce

    end program zsjmoment

