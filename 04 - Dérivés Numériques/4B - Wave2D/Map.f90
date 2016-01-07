! ---------------------------------------------------------------------------------
!
!      MAP enables the easy visualization of z=z(x,y) real maps
!           The data is either contained 
!              in a list_z(:)   list  (sequential storage of the map)
!           or in a list_z(:,:) array (two-dimensional storage of the map)
!
!      Required library : GrWin
!        
!      Default usage: call Map ( list_x, list_y, list_z )
!
!        Note that 'use MapPack' must be included in the calling program
!
!      Input:
!           - list_x : list containing the x values
!           - list_y : list containing the y values
!           - list_z : list or array containing the z=z(x,y) values
!
!        Note that single and double precisions are both supported.
!
!      Optional arguments:
!           - zlog :    if .true. log(z) is represented instead of z
!                       default is .false.
!           - xlabel :  sets the label of the X axis
!                       default is 'x'
!           - ylabel :  sets the label of the Y axis
!                       default is 'y'
!           - title :   sets the title
!                       default is 'Representation of z=z(x,y)'
!           - keep :    if .true. the window will be kept opened
!                       default is .false.
!           - refresh : if .true. the plot appears in the same window as the previous one
!                       default is .false.
!           - zmin :    sets the minimal value of list_z
!                       default is the actual value
!           - zmax :    sets the maximal value of list_z
!                       default is the actual value
!
!        Example: call Map ( list_x, list_y, list_z, zlog=.true. )
!
!      Author: Alexandre Mayer
!              Facultés Universitaires Notre-Dame de la Paix, Namur, Belgium
!              Web: http://perso.fundp.ac.be/~amayer
!           
! ---------------------------------------------------------------------------------
      Module MapPack

! ---------------------------------------------------------------------------------
! ------- Generic Interface Map ---------------------------------------------------
! ---------------------------------------------------------------------------------
      Interface Map
       module procedure MapListSP, MapListDP, MapTabSP, MapTabDP
      end Interface

      CONTAINS

! ---------------------------------------------------------------------------------
! ------- Subroutine MapListSP ----------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine MapListSP (list_x, list_y, list_z, zlog, xlabel, ylabel, title, &
                            keep, refresh, zmin, zmax)

      implicit none

      integer, parameter :: r=4
      real(kind=r),dimension(:),intent(in),target :: list_x, list_y, list_z
      real(kind=r),intent(in),optional :: zmin, zmax
      character(len=*),intent(in),optional :: xlabel, ylabel, title
      logical,intent(in),optional :: zlog, keep, refresh

      character(len=80) :: xlabel$, ylabel$, title$, nd1$, nd2$, format$
      real(kind=r),dimension(:),pointer :: list_z$
      real(kind=r),dimension(:),allocatable :: list_x$, list_y$
      integer :: i, j, k, resx, resy, ii, jj, color
      logical :: zlog$, keep$, refresh$
      
      real(kind=r),save :: xorig, yorig, xfin, yfin, zmin$, zmax$, range
      real(kind=r) :: x1, y1, x2, y2, pxlx, pxly
      integer :: sizex, sizey, resx$, resy$, nd
      integer,save :: unit$, IRTN
      character(len=80) :: text$

      interface
       Function KRGB(IR,IG,IB)
        integer :: KRGB
        integer,intent(in) :: IR, IG, IB
       end Function KRGB
      end interface
      
      zlog$ = .false.
      if (present(zlog)) zlog$=zlog
      xlabel$  = 'x'
      if (present(xlabel)) xlabel$=trim(adjustl(xlabel))
      ylabel$  = 'y'
      if (present(ylabel)) ylabel$=trim(adjustl(ylabel))
      title$  = 'Representation of z=z(x,y)'
      if (present(title))  title$ =trim(adjustl(title))
      keep$ = .false.
      if (present(keep)) keep$=keep
      refresh$ = .false.
      if (present(refresh)) refresh$=refresh

      if (zlog$) title$ = 'log('//trim(adjustl(title$))//')'

      call Purge(xlabel$)
      call Purge(ylabel$)
      call Purge(title$)

! A. Open Window and set parameters
! +++++++++++++++++++++++++++++++++
! - Open window
! -------------
      if (.not.refresh$) then
       unit$ = 0
       if (title$=="") title$="Representation of the data"
       IRTN = 0
       do while(IRTN==0)
        call GWOPENX(IRTN,unit$,0,0,19,0,10,'')    ! open graphics window
        if (IRTN==0) then
         print *, "Problem when opening the window ... will try again in 1 sec."
         call sleep(1)
        endif
       enddo
      endif
      
! - Size of the window
! --------------------
      call GWSIZE(IRTN,1,resx,resy)                ! get numbers of pixels      
            
! - Parameters of the representation
! ----------------------------------
      xorig = minval(list_x)
      xfin  = maxval(list_x)
      yorig = minval(list_y)
      yfin  = maxval(list_y)

      if (xfin-xorig == yfin-yorig) then           ! square representation
       resx$ = resy
       resy$ = resy
      elseif(xfin-xorig < yfin-yorig) then         ! rectangular representation
       resx$ = nint(resy*(xfin-xorig)/(yfin-yorig))
       resy$ = resy
      else
       resx$ = resy 
       resy$ = nint(resy*(yfin-yorig)/(xfin-xorig))
      endif

      xfin  = xorig + (xfin-xorig) *  float(resx)/resx$
      yorig = yorig - (yfin-yorig) * (float(resy)/resy$-1._r)

      call GWINDOW(IRTN,xorig,yorig,xfin,yfin)     ! world coordinate system

      pxlx  = (xfin-xorig) / resx                  ! pixel size
      pxly  = (yfin-yorig) / resy
      
! B. Transformation of the z data
! +++++++++++++++++++++++++++++++      
      if (zlog$) then
       allocate(list_z$(1:size(list_z,1)))
       list_z$ = list_z
       if (zlog$) list_z$ = log10(list_z$)
      else
       list_z$ => list_z
      endif
        
      if (present(zmin)) then
       zmin$   = zmin
       if (zlog$) zmin$=log10(zmin$)
       list_z$ = max(list_z$,zmin$)
      else
       zmin$   = minval(list_z$)
      endif

      if (present(zmax)) then
       zmax$   = zmax
       if (zlog$) zmax$=log10(zmax$)
       list_z$ = min(list_z$,zmax$)
      else
       zmax$   = maxval(list_z$)
      endif
 
      range = zmax$ - zmin$
      if (range==0._r) return

      sizex = size(list_x,1)
      sizey = size(list_y,1)

      list_z$ = (list_z$-zmin$) * (255._r / range)

! C. Representation of the map
! ++++++++++++++++++++++++++++
      if (sizex < resx$ .or. sizey < resy$) then
       allocate(list_x$(1:sizex),list_y$(1:sizey))
       do i = 1, sizex-1
        list_x$(i) = (list_x(i)+list_x(i+1))/2._r
       enddo
       list_x$(sizex) = list_x(sizex)
       do i = 1, sizey-1
        list_y$(i) = (list_y(i)+list_y(i+1))/2._r
       enddo
       list_y$(sizey) = list_y(sizey)
       k  = 0
       y2 = list_y(1)
       do j = 1, sizey
        y1 = y2
        y2 = list_y$(j)
        x2 = list_x(1)
        do i = 1, sizex
         x1 = x2
         x2 = list_x$(i)
         k = k + 1
         color = nint(list_z$(k))                                             ! gray level between 0 and 255
         color = KRGB(color,color,color)                                      ! RGB --> logical color value
         call GWSRECT(IRTN,x1,y1,x2,y2,color)
        enddo
       enddo
       deallocate(list_x$,list_y$)
      else
       do j = 1, resy$
        jj = 1 + nint(float((j-1)*(sizey-1))/resy$)
        do i = 1, resx$
         ii = 1 + nint(float((i-1)*(sizex-1))/resx$)
         k = ii + (jj-1) * sizex
         color = nint(list_z$(k))                                             ! gray level between 0 and 255
         color = KRGB(color,color,color)                                      ! RGB --> logical color value
         call GWSRECT(IRTN,(i-1)*pxlx,(j-1)*pxly,i*pxlx,j*pxly,color)
        enddo
       enddo
       x2 = list_x(sizex)
      endif

! D. Display information
! ++++++++++++++++++++++
      if (refresh$) then
       call GWSRECT(IRTN,x2+4*pxlx,yorig,xfin,yfin,0)
      endif

      pxlx = pxlx * resy / 250._r                                                      ! adapt pixel size
      pxly = pxly * resy / 250._r

      x1 = xorig +  resx$ * (xfin-xorig) / resx + 20._r * pxlx
      y1 = yfin - 20._r * pxly

! x
      call GWPUTTXT(IRTN,x1,y1,trim(adjustl(xlabel$))//"")

      nd = max (3,min(2-floor(log10(abs(xfin-xorig)/max(abs(xfin),abs(xorig)))),16))   ! number of significant digits
      write(nd1$,'(i4)') nd
      write(nd2$,'(i4)') nd+6
      format$ = "(g"//trim(adjustl(nd2$))//"."//trim(adjustl(nd1$))//")"

      y1 = y1 - 20. * pxly
      write(text$,format$) minval(list_x)
      call GWPUTTXT(IRTN,x1,y1,"   min = "//trim(adjustl(text$)))
            
      y1 = y1 - 20. * pxly
      write(text$,format$) maxval(list_x)
      call GWPUTTXT(IRTN,x1,y1,"   max = "//trim(adjustl(text$)))
       
! y
      y1 = y1 - 40. * pxly

      call GWPUTTXT(IRTN,x1,y1,trim(adjustl(ylabel$))//"")

      nd = max (3,min(2-floor(log10(abs(yfin-yorig)/max(abs(yfin),abs(yorig)))),16))   ! number of significant digits
      write(nd1$,'(i4)') nd
      write(nd2$,'(i4)') nd+6
      format$ = "(g"//trim(adjustl(nd2$))//"."//trim(adjustl(nd1$))//")"

      y1 = y1 - 20. * pxly
      write(text$,format$) minval(list_y)
      call GWPUTTXT(IRTN,x1,y1,"   min = "//trim(adjustl(text$)))
              
      y1 = y1 - 20. * pxly
      write(text$,format$) maxval(list_y)
      call GWPUTTXT(IRTN,x1,y1,"   max = "//trim(adjustl(text$)))
       
! z
      y1 = y1 - 40. * pxly
      call GWPUTTXT(IRTN,x1,y1,trim(adjustl(title$))//"")
       
      nd = max (3,min(2-floor(log10(abs(zmax$-zmin$)/max(abs(zmin$),abs(zmax$)))),16)) ! number of significant digits
      write(nd1$,'(i4)') nd
      write(nd2$,'(i4)') nd+6
      format$ = "(g"//trim(adjustl(nd2$))//"."//trim(adjustl(nd1$))//")"

      y1 = y1 - 20. * pxly
      write(text$,format$) zmin$
      call GWPUTTXT(IRTN,x1,y1,"   min = "//trim(adjustl(text$)))
       
      y1 = y1 - 20. * pxly
      write(text$,format$) zmax$
      call GWPUTTXT(IRTN,x1,y1,"   max = "//trim(adjustl(text$)))
       
      if (title$/="") call GWSETMSG(IRTN,trim(title$))                                 ! write title in status bar
       
      if (.not.keep$) call GWQUITX(IRTN,0)
      
      if (zlog$) deallocate(list_z$)

      CONTAINS

! ---------------------------------------------------------------------------------
! ------- Internal Subroutine Purge -----------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine Purge (var$)

      character(len=*),intent(inout) :: var$
      integer :: nd

      do while(index(var$,"\u") /= 0)
       nd = index(var$,"\u")
       var$ = trim(adjustl(var$(:nd-1)))//trim(var$(nd+2:))
      enddo
      do while(index(var$,"\d") /= 0)
       nd = index(var$,"\d")
       var$ = trim(adjustl(var$(:nd-1)))//trim(var$(nd+2:))
      enddo
      do while(index(var$,"\g") /= 0)
       nd = index(var$,"\g")
       var$ = trim(adjustl(var$(:nd-1)))//trim(var$(nd+2:))
      enddo

      end Subroutine Purge

      end Subroutine MapListSP

! ---------------------------------------------------------------------------------
! ------- Subroutine MapListDP ----------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine MapListDP (list_x, list_y, list_z, zlog, xlabel, ylabel, title, &
                            keep, refresh, zmin, zmax)

      implicit none

      integer, parameter :: r=8
      real(kind=r),dimension(:),intent(in),target :: list_x, list_y, list_z
      real(kind=r),intent(in),optional :: zmin, zmax
      character(len=*),intent(in),optional :: xlabel, ylabel, title
      logical,intent(in),optional :: zlog, keep, refresh

      real(kind=r) :: zmin$, zmax$
      character(len=80) :: xlabel$, ylabel$, title$
      logical :: zlog$, keep$, refresh$

      zlog$ = .false.
      if (present(zlog)) zlog$=zlog
      xlabel$  = 'x'
      if (present(xlabel)) xlabel$=trim(adjustl(xlabel))
      ylabel$  = 'y'
      if (present(ylabel)) ylabel$=trim(adjustl(ylabel))
      title$  = 'Representation of z=z(x,y)'
      if (present(title))  title$ =trim(adjustl(title))
      keep$ = .false.
      if (present(keep)) keep$=keep
      refresh$ = .false.
      if (present(refresh)) refresh$=refresh
      if (present(zmin)) then
       zmin$ = zmin
      else
       zmin$ = minval(list_z)
      endif
      if (present(zmax)) then
       zmax$ = zmax
      else
       zmax$ = maxval(list_z)
      endif

      call MapListSP(real(list_x,4),real(list_y,4),real(list_z,4), &
                     zlog=zlog$,xlabel=xlabel$,ylabel=ylabel$,title=title$, &
                     keep=keep$,refresh=refresh$,zmin=real(zmin$,4),zmax=real(zmax$,4))
 
      end Subroutine MapListDP

! ---------------------------------------------------------------------------------
! ------- Subroutine MapTabSP -----------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine MapTabSP (list_x, list_y, list_z, zlog, xlabel, ylabel, title, &
                           keep, refresh, zmin, zmax)

      implicit none

      integer, parameter :: r=4
      real(kind=r),dimension(:),intent(in),target :: list_x, list_y
      real(kind=r),dimension(:,:),intent(in),target :: list_z
      real(kind=r),intent(in),optional :: zmin, zmax
      character(len=*),intent(in),optional :: xlabel, ylabel, title
      logical,intent(in),optional :: zlog, keep, refresh

      real(kind=r) :: zmin$, zmax$
      character(len=80) :: xlabel$, ylabel$, title$
      logical :: zlog$, keep$, refresh$

      zlog$ = .false.
      if (present(zlog)) zlog$=zlog
      xlabel$  = 'x'
      if (present(xlabel)) xlabel$=trim(adjustl(xlabel))
      ylabel$  = 'y'
      if (present(ylabel)) ylabel$=trim(adjustl(ylabel))
      title$  = 'Representation of z=z(x,y)'
      if (present(title))  title$ =trim(adjustl(title))
      keep$ = .false.
      if (present(keep)) keep$=keep
      refresh$ = .false.
      if (present(refresh)) refresh$=refresh
      if (present(zmin)) then
       zmin$ = zmin
      else
       zmin$ = minval(list_z)
      endif
      if (present(zmax)) then
       zmax$ = zmax
      else
       zmax$ = maxval(list_z)
      endif

      call MapListSP(list_x,list_y,pack(list_z,.true.), &
                     zlog=zlog$,xlabel=xlabel$,ylabel=ylabel$,title=title$, &
                     keep=keep$,refresh=refresh$,zmin=zmin$,zmax=zmax$)

      end Subroutine MapTabSP

! ---------------------------------------------------------------------------------
! ------- Subroutine MapTabDP -----------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine MapTabDP (list_x, list_y, list_z, zlog, xlabel, ylabel, title, &
                           keep, refresh, zmin, zmax)

      implicit none

      integer, parameter :: r=8
      real(kind=r),dimension(:),intent(in),target :: list_x, list_y
      real(kind=r),dimension(:,:),intent(in),target :: list_z
      real(kind=r),intent(in),optional :: zmin, zmax
      character(len=*),intent(in),optional :: xlabel, ylabel, title
      logical,intent(in),optional :: zlog, keep, refresh

      real(kind=r) :: zmin$, zmax$
      character(len=80) :: xlabel$, ylabel$, title$
      logical :: zlog$, keep$, refresh$

      zlog$ = .false.
      if (present(zlog)) zlog$=zlog
      xlabel$  = 'x'
      if (present(xlabel)) xlabel$=trim(adjustl(xlabel))
      ylabel$  = 'y'
      if (present(ylabel)) ylabel$=trim(adjustl(ylabel))
      title$  = 'Representation of z=z(x,y)'
      if (present(title))  title$ =trim(adjustl(title))
      keep$ = .false.
      if (present(keep)) keep$=keep
      refresh$ = .false.
      if (present(refresh)) refresh$=refresh
      if (present(zmin)) then
       zmin$ = zmin
      else
       zmin$ = minval(list_z)
      endif
      if (present(zmax)) then
       zmax$ = zmax
      else
       zmax$ = maxval(list_z)
      endif

      call MapListSP(real(list_x,4),real(list_y,4),pack(real(list_z,4),.true.),&
                     zlog=zlog$,xlabel=xlabel$,ylabel=ylabel$,title=title$, &
                     keep=keep$,refresh=refresh$,zmin=real(zmin$,4),zmax=real(zmax$,4))

      end Subroutine MapTabDP

      end Module MapPack