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
ncut--�������
originmesh--�Ƿ����ԭʼ����
cutmehs--�Ƿ������������
nbem--����Ԫ����
noutstep--���Ľ���������
nsurf--ftr���������

cut(i)%index,cut(i)%dir,cut(i)%cuttype,cut(i)%interval,cut(i)%nsurf

cut(i)%index--������
cut(i)%dir--���淽�򣬵�Ϊ����ʱ�����ֱ�ķ���
cut(i)%cuttype--��������
	1---���������и�
	2---�����и�
	3---���������ftr
	4---�����ȡftr
cut(i)%interval--������
cut(i)%nsurf--��ǰ������ftr�����
����
��һ�У�Ϊftr��������������1��ʾ���룬0��ʾ������
�ڶ��У�����Ϊ1-2ʱ������ĵ�Ԫ�������
�����У�����Ϊ1-2ʱ���������ʼ����ֹ����
```
/////////////////////////////////////////////////////////////////////////////
