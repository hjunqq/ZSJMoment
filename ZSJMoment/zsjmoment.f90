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

    !��ȡ������Ϣ
    open(mshunit,file=fpath(1:len_trim(fpath))//".flavia.msh")

    call readmsh

    close(mshunit)

    !����

    open(cutunit,file=fpath(1:len_trim(fpath))//".cut")

    call readcut

    close(cutunit)

    call defelemedge

    call cutplane

    open(resunit,file=fpath(1:len_trim(fpath))//".flavia.res")

    call resprocess


    close(resunit)

    contains
    !>��ȡ������Ϣ
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ��� group��node��elem ȫ�ֱ������洢������Ϣ
    subroutine readmsh
    character(150)   ::orgline,text
    integer         ::idxi,idxj,igroup,icoor,ielem,idim

    ismeshgroup = .false.
    isoldformat = .true.
    ngroup=0
    ncoor=0
    nelem=0
    ngroup=0
    !������͵�Ԫ��ͷβ�ڵ�ָ���ÿ�
    nullify(grouphead)
    nullify(grouplast)
    nullify(coorhead)
    nullify(coorlast)
    nullify(elemhead)
    nullify(elemlast)
    do
        !��ȡһ������
        read(mshunit,'(a150)',end=100)orgline
        orgline = adjustl(orgline)                            !for old format
        read(orgline(1:index(orgline,' ')),'(a20)')text
        !ȡ���ݵĵ�һ������
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
            !��ʼ��ȡ��Ԫ��ڵ���Ϣ
            ngroup=ngroup+1
            !Ϊ��Ԫ������ڴ�
            allocate(pgroup)
            pgroup%index=ngroup

            if(.not.isoldformat)then
                idxi=index(orgline,'mesh')+len("mesh")+2          !ȥ��˫���ţ�����Ҫ���һ���ַ�
                idxj=index(orgline,'dimension')-1-2                !ȥ��˫���ţ���Ҫ���һ���ַ�
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
            
            !����ȡ���ĵ�Ԫ��ŵ������Ľ�β
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
            !Ϊ��Ԫ��ڵ��еĵ�Ԫ�б������ڴ棬Ȼ�󽫵�Ԫ�б�ָ��ָ��Ԫ��Ϣ��ַ
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

    !������ת���ɶ�̬�����ʽ���Է����Ժ��ȡ
    !Ϊ��Ԫ������ڴ�
100 allocate(group(ngroup))
    !��������ͷ��ʼ���ν�ֵ��������
    pgroup=>grouphead
    igroup=0
    do while(associated(pgroup))
        igroup=igroup+1
        group(igroup)=pgroup
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
        ndim = max(ndim,group(igroup)%dim)
    enddo
    end subroutine


    !>��ȡ������Ϣ
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

    ntype = 1
    allocate(elemlibs(ntype))
    elemlibs(1)%index = 9
    elemlibs(1)%nline = 12
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

    endsubroutine defelemedge

    !>����
    subroutine cutplane()
    implicit none

    integer         ::i,j,k
    integer         :: igroup,isec,nodeidx,inode,dir,nfaceelem
    integer         ::ielem, jelem, jnode, iline, tnode,iface
    integer         ::nodei,nodej
    type(coorinfo)      ::coori,coorj

    integer,allocatable::elemcross(:)

    real(8)::planenormal(3),centcoord(3),ispoint(3)

    integer::is

    integer::distance

    real::secvalue

    allocate(pelem)

    allocate(pelemlib)



    planenormal=(/0.0,sqrt(1-angular**2),angular/)




    do i=1,ncut


        nullify(facehead)
        nullify(facelast)


        isec = 0
        iface = 0
        dir = cut(i)%dir

        do
            isec = isec +1

            nullify(coorhead)
            nullify(coorlast)
            nullify(elemhead)
            nullify(elemlast)
            
            !��ʼ����Ԫ
            !do ielem = 1,nelem
            !    elem(ielem)%ifcross=0
            !enddo
            
            do igroup = 1,ngroup
                do ielem=1,group(igroup)%nelem
                    pelem => group(igroup)%elem(ielem)
                    pelem%ifcross=0
                enddo
            enddo
            

            ! find intersection element

            secvalue=cut(i)%spoint(dir)+cut(i)%interval*isec

            centcoord=(/0.0,secvalue,0.0/)
            nfaceelem = 0

            


            allocate(pcoor)
            do igroup = 1, ngroup
                if(cut(i)%appeargroup(igroup)==0)cycle
                do ielem = 1, group(igroup)%nelem
                    pelem => group(igroup)%elem(ielem)

                    if(.not.allocated(pelem%cross))then
                        allocate(pelem%cross(group(igroup)%nnode))
                    endif

                    pelem%cross=0

                    distance = 0

                    do inode = 1,group(igroup)%nnode
                        nodeidx = pelem%node(inode)
                        pcoor = coor(nodeidx)
                        pelem%cross(inode) = pointtoplane(pcoor%val,planenormal,centcoord)
                        distance = distance + pelem%cross(inode)
                    enddo
                    if(abs(distance)/=group(igroup)%nnode)then
                        pelem%ifcross=1
                        write(chkunit,*)pelem%index
                        nfaceelem = nfaceelem + 1
                    endif
                enddo
            enddo

            print*,nfaceelem
            write(chkunit,*)nfaceelem,"-------------------------"

            if(secvalue>cut(i)%epoint(dir))then
                exit
            endif

            if(nfaceelem==0)cycle

            allocate(pface)
            iface = iface +1
            pface%index = iface

            ! generate intersection node

            jelem = 0
            tnode = 0
            do igroup = 1, ngroup
                if(cut(i)%appeargroup(igroup)==0)cycle
                do ielem = 1, group(igroup)%nelem
                    pelem => group(igroup)%elem(ielem)
                    if(pelem%ifcross == 1)then
                        jelem = jelem +1
                        jnode = 0

                        pelemlib => elemlibs(1)

                        do iline = 1, pelemlib%nline

                            nodei = pelemlib%line(iline)%node(1)
                            nodej = pelemlib%line(iline)%node(2)

                            coori = coor(pelem%node(nodei))
                            coorj = coor(pelem%node(nodej))


                            is = intersection(coorj%val-coori%val,coori%val,planenormal,centcoord,ispoint)
                            if(is==1)then
                                !print '(2I5,3(F7.3,","),x,3(F7.3,","),x,3(F7.3,","))',coori.index,coorj.index,coori.val,coorj.val,ispoint
                                jnode = jnode +1
                                allocate(pcoor)
                                pcoor%val=ispoint
                                pcoor%index = jnode
                                pcoor%relate=(/coori%index,coorj%index/)
                                pcoor%next=>null()


                                if(associated(coorlast))then
                                    coorlast%next=>pcoor
                                    coorlast=>pcoor
                                else
                                    coorhead=>pcoor
                                    coorlast=>pcoor
                                endif

                            endif


                        enddo

                        tnode = tnode + jnode
                        allocate(piselem)
                        allocate(piselem%node(jnode))
                        piselem%index = jelem
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

            pface%nelem = jelem
            allocate(pface%elem(jelem))
            piselem => elemhead
            ielem = 0
            do while(associated(piselem))
                ielem = ielem +1
                pface%elem(ielem) = piselem
                piselem => piselem%next
            enddo
            allocate(pface%coor(tnode))
            pcoor => coorhead
            inode = 0
            do while(associated(pcoor))
                inode = inode +1
                pface%coor(inode) = pcoor
                pcoor => pcoor%next
            enddo

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

        print*,iface

    enddo



    end subroutine cutplane

    !>��ȡ����������Ϣ
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    subroutine resprocess
    use gidpost

    character(300)   ::orgline,text
    integer          ::idxi,idxj,ires,icomp,icoor,istep,idx,ielem,igroup,eidx
    real            ::curtime
    real,allocatable::time(:)

    type(gid_file) :: fdm, fdr
    real(8):: sxx,syy,szz,sxy,syz,sxz, x,y,z
    type(gid_elementtype):: etype
    type(resinfo)::res
    real:: stress_mat(3,3)


    !д�����ļ�
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
        pgroup=>group(igroup)
        select case(trim(pgroup.elemtype))
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


    end program zsjmoment
