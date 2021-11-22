	!>全局变量模块，声明此程序所需要的全局变量
	module datatype
	!> 坐标链表节点.
	!!@param index 坐标的索引值
	!!@param val 坐标的值
	!!@param next 指向下一个坐标节点
	type coorinfo
		integer         ::index
		real(8),allocatable::val(:)
		real,allocatable::trans(:,:)
		integer         ::relate(2)
		type(coorinfo),pointer::next
	end type
	!> 单元链表节点
	!!@param index 单元索引值
	!!@param group 单元所在组
	!!@param node 单元节点
	!!@param next 指向下一个单元节点
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
	
	!> 单元组链表节点
	!!@param meshname 单元组的名字
	!!@param elemtype 单元组类型
	!!@param index 单元组索引值
	!!@param dim 单元组维数
	!!@param nnode 每个单元节点数
	!!@param ncoor 单元组中的节点数
	!!@param nelem 单元组中的单元数
	!!@param next 指向下一个节点
	!!@param elem 单元组中的单元列表
	type groupinfo
		character(70)   ::groupname,elemtype
		integer         ::index,dim,nnode,ncoor,nelem
		type(groupinfo),pointer::next
		type(eleminfo),dimension(:),pointer::elem
		type(coorinfo),dimension(:),pointer::coor
	end type
	!>结果值链表节点
	!!@param index 结果值索引
	!!@param dat 结果值
	!!@param next 下一个节点
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

	!>切面信息
	!!@param cuttype 切面类型： 1 - 按照间隔切面 2 - 按照单元切面 3- 按照间隔查找最接近的单元面
	type cutinfo
		integer::index,dir,ngroup
		integer::cuttype
		real(8)::interval
		real(8)::spoint(3),epoint(3)
		integer,allocatable::appeargroup(:)
		integer,allocatable::group(:)
		type(faceinfo),allocatable::face(:)
	end type cutinfo
	

	!>结果组链表节点
	!!@param index 结果组索引
	!!@param resname 结果组名称
	!!@param ananame 分析类型
	!!@param restype 结果类型；如向量，张量等
	!!@param compname 结果列表每一列的名称
	!!@param val 结果值
	!!@param valhead 结果值链表头节点
	!!@param vallast 结果值链表尾节点
	!!@param pval 当前结果值节点
	!!@param next 下一个结果组
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
