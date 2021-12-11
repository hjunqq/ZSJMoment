ndimn,npoin,   nelem,  nsurf,ngroup,nmats,mnode,opt,iter_tot,findpoint,miter,contact_cut
  3   1910      1171     0     9    9     8    0     200      0       10   0
readmesh writeghm write_msh ncontact thickness  readface  field
  2         1        1          0        0.4        0      "U"
mat_group
9*1
gap_group
9*1
icontact contactm
ksurf(1:nsurf) ! 1 surfaces by group;2 surfaces by number of material
9*1
gnode(1:ngroup)
5*8 2 100*2   !gnode
elemshape(1:ngroup)  !0-Point;1-Line,3-triangle;4-Quadrilateral;6-Tetrahedra;9-Prism;12-Hexahedra
5*12 1 100*12
contactm
isurf=1		 
    option1   write_chs         bnc
	1             1           sideload
    appear_process(1:ngroup or nmats) (following nsurf lines)
   0 1 1 1  0 10*1
    axis_ctl(1:ndimn),vmin(1:ndimn),vmax(1:ndimn)
	0 0  0            -0.021 20  0   3.1  50 0
    axis_rot(1:ndimn),rotmin(1:ndimn),rotmax(1:ndimn)
	0 1         0.  0.8       0.   1.1    0.
isurf=2		 
    option1   write_chs         bnc
	1             1           sideload
    appear_process(1:ngroup or nmats) (following nsurf lines)
   0 1 0
    axis_ctl(1:ndimn),vmin(1:ndimn),vmax(1:ndimn)
	1 1              0.219  -0.03  0.221  0.1220  6.5
    axis_rot(1:ndimn),rotmin(1:ndimn),rotmax(1:ndimn)
	0 0 0            -1.  -0.1  0.9    0   0.1 1.0