========================================================================
	Fortran Console Application : "ZSJMoment" Project Overview
========================================================================

Intel(R) Fortran Console Application Wizard has created this 
"ZSJMoment" project for you as a starting point.

This file contains a summary of what you will find in each of the files 
that make up your project.

ZSJMoment.vfproj
	This is the main project file for Fortran projects generated using an 
	Application Wizard.  It contains information about the version of 
	Intel(R) Fortran that generated the file, and information about the 
	platforms, configurations, and project features selected with the 
	Application Wizard.

ZSJMoment.f90
	This is the main source file for the Fortran Console application. 
	It contains the program entry point.

/////////////////////////////////////////////////////////////////////////////
Other notes:
```fortran
ncut,originmesh,cutmesh,nbem,noutstep,nsurf
3    .False.    .False.  0     46  267
cut(i)%index,cut(i)%dir,cut(i)%cuttype,cut(i)%interval,cut(i)%nsurf
1                2           4         1.6           12
239*0 12*1 6*0 200*0
10000*0 200*1 6*0 200*0
199.996  384.003  46.916  199.968  768.003 46.533
cut(i)%index,cut(i)%dir,cut(i)%cuttype,cut(i)%interval,cut(i)%nsurf
1                2           4         1.6           12
251*0 12*1 6*0 200*0
10000*0 200*1 6*0 200*0
199.996  384.003  46.916  199.968  768.003 46.533
cut(i)%index,cut(i)%dir,cut(i)%cuttype,cut(i)%interval,cut(i)%nsurf
1                2           3         1.6           239
239*1 6*0 200*0
10000*0 200*1 6*0 200*0
199.996  384.003  46.916  199.968  768.003 46.533
```

```fortran
ncut--切面个数
originmesh--是否输出原始网格
cutmehs--是否输出切面网格
nbem--粱单元个数
noutstep--最大的结果输出步数
nsurf--ftr中切面个数

cut(i)%index,cut(i)%dir,cut(i)%cuttype,cut(i)%interval,cut(i)%nsurf

cut(i)%index--切面编号
cut(i)%dir--切面方向，当为环向时，填环垂直的方向
cut(i)%cuttype--切面类型
	1---沿坐标轴切割
	2---环向切割
	3---沿坐标轴读ftr
	4---环向读取ftr
cut(i)%interval--切面间隔
cut(i)%nsurf--当前切面中ftr面个数
下面
第一行，为ftr中切面参与情况，1表示参与，0表示不参与
第二行，切面为1-2时，整体的单元参与情况
第三行，切面为1-2时，切面的起始、终止坐标
```
/////////////////////////////////////////////////////////////////////////////
