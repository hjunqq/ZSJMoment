	!>ȫ�ֱ���ģ�飬�����˳�������Ҫ��ȫ�ֱ���
	module datatype
	!> ��������ڵ�.
	!!@param index ���������ֵ
	!!@param val �����ֵ
	!!@param next ָ����һ������ڵ�
	type coorinfo
		integer         ::index
		real(8),allocatable::val(:)
		real,allocatable::trans(:,:)
		integer         ::relate(2)
		type(coorinfo),pointer::next
	end type
	!> ��Ԫ����ڵ�
	!!@param index ��Ԫ����ֵ
	!!@param group ��Ԫ������
	!!@param node ��Ԫ�ڵ�
	!!@param next ָ����һ����Ԫ�ڵ�
	type eleminfo
		integer         ::index, group, ifcross
		integer,allocatable::node(:), cross(:)
		type(eleminfo),pointer::next
	end type
	
	type lineinfo
		integer			::index
		integer         ::node(2)
	endtype
	
	type elemtype
		integer			::index
		integer         ::nline,nface
		type(lineinfo),allocatable::line(:)
	endtype
	
	!> ��Ԫ������ڵ�
	!!@param meshname ��Ԫ�������
	!!@param elemtype ��Ԫ������
	!!@param index ��Ԫ������ֵ
	!!@param dim ��Ԫ��ά��
	!!@param nnode ÿ����Ԫ�ڵ���
	!!@param ncoor ��Ԫ���еĽڵ���
	!!@param nelem ��Ԫ���еĵ�Ԫ��
	!!@param next ָ����һ���ڵ�
	!!@param elem ��Ԫ���еĵ�Ԫ�б�
	type groupinfo
		character(70)   ::groupname,elemtype
		integer         ::index,dim,nnode,ncoor,nelem
		type(groupinfo),pointer::next
		type(eleminfo),dimension(:),pointer::elem
		type(coorinfo),dimension(:),pointer::coor
	end type
	!>���ֵ����ڵ�
	!!@param index ���ֵ����
	!!@param dat ���ֵ
	!!@param next ��һ���ڵ�
	type resvalinfo
		integer         ::index
		real(8),allocatable    ::dat(:)
		type(resvalinfo),pointer::next
	end type
	
	
	type faceinfo
		integer::index,nnode,nelem
		type(coorinfo),allocatable::coor(:)
		type(eleminfo),allocatable::elem(:)
		type(faceinfo),pointer::next
	end type faceinfo

	!>������Ϣ
	!!@param cuttype �������ͣ� 1 - ���ռ������ 2 - ���յ�Ԫ���� 3- ���ռ��������ӽ��ĵ�Ԫ��
	type cutinfo
		integer::index,dir,ngroup
		integer::cuttype
		real(8)::interval
		real(8)::spoint(3),epoint(3)
		integer,allocatable::appeargroup(:)
		integer,allocatable::group(:)
		type(faceinfo),allocatable::face(:)
	end type cutinfo
	

	!>���������ڵ�
	!!@param index ���������
	!!@param resname ���������
	!!@param ananame ��������
	!!@param restype ������ͣ���������������
	!!@param compname ����б�ÿһ�е�����
	!!@param val ���ֵ
	!!@param valhead ���ֵ����ͷ�ڵ�
	!!@param vallast ���ֵ����β�ڵ�
	!!@param pval ��ǰ���ֵ�ڵ�
	!!@param next ��һ�������
	type resinfo
		character(70)   ::resname,ananame,restype
		character(70),allocatable::compname(:)
		character(70)   ::loaddesc
		integer         ::index,nval,loadtype,datatype,dataloc,desccomp,gausspoint
		real(8)            ::timeana
		type(resvalinfo),dimension(:),pointer::val
		type(resvalinfo),pointer::valhead,pval,vallast
		type(resinfo),pointer::next
	end type


	type meshgroupinfo
		character(70)   ::name
		type(groupinfo),dimension(:),pointer::group
		type(resinfo),dimension(:),pointer::res
	endtype

	integer::mshunit,resunit,inpunit,cutunit,chkunit
	integer::ngroup,ncoor,nelem,nres,ntres,nstep,ndim,nzone,ncut
	logical::ismeshgroup,isoldformat
	character(len=128)::arg
	integer::narg,iarg
	real::pcenter(3)
	real::angular
	character(len=128)::restype
	character(len=128)::fpath,text
	logical::eof



	type(groupinfo),pointer::grouphead,pgroup,grouplast
	type(groupinfo),dimension(:),pointer::group
	type(coorinfo),pointer::coorhead,pcoor,coorlast
	type(coorinfo),dimension(:),pointer::coor
	type(eleminfo),pointer::elemhead,pelem,elemlast
	type(eleminfo),dimension(:),pointer::elem
	type(resinfo),pointer::reshead,pres,reslast
	type(resinfo),dimension(:),pointer::res
	type(resinfo),dimension(:),pointer::restrans
	type(cutinfo),pointer::cuthead,pcut,cutlast
	type(cutinfo),dimension(:),pointer::cut
	type(faceinfo),pointer::facehead,pface,facelast
	type(faceinfo),dimension(:),pointer::face
	
	type(eleminfo),pointer::piselem
	
	type(elemtype),dimension(:),pointer::elemlibs
	type(lineinfo),pointer::pline
	type(elemtype),pointer::pelemlib
	


	integer             ::ierr


	real,parameter:: pi=3.1415927
	real,parameter:: small=0.00000001

	end module
