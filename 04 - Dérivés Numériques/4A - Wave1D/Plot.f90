! ---------------------------------------------------------------------------------
!
!      PLOT enables the easy visualization of y=y(x) real functions,
!           where the data are either contained 
!              in list_x(:)   and list_y(:)   lists  (for a single function)
!           or in list_x(:,:) and list_y(:,:) arrays (for several functions)
!
!      Required library : GrWin 
!
!      Default usage: call Plot ( list_x, list_y )
!
!        Note that 'use PlotPack' must be included in the calling program
!
!      Input:
!           - list_x : list/array containing the x values
!           - list_y : list/array containing the y values
!
!        Note that simple and double precisions are both supported
!
!      Optional arguments:
!           - line :    if .false. the points are not connected
!                       default is .true.
!           - xlog :    if .true. log(x) is represented instead of x
!                       default is .false.
!           - ylog :    if .true. log(y) is represented instead of y
!                       default is .false.
!           - xlabel :  sets the label of the X axis
!                       default is 'x'
!           - ylabel :  sets the label of the Y axis
!                       default is 'y'
!           - title :   sets the title
!                       default is 'Representation of y=y(x)'
!           - keep :    if .true. the window will be kept opened
!                       default is .false.
!           - refresh : if .true. the plot appears in the same window as the previous one
!                       default is .false.
!
!        Example: call Plot ( list_x, list_y, ylog=.true.)
!
!      Author: Alexandre Mayer
!              Facultés Universitaires Notre-Dame de la Paix, Namur, Belgium
!              Web: http://perso.fundp.ac.be/~amayer
!           
! ---------------------------------------------------------------------------------
      Module PlotPack

! ---------------------------------------------------------------------------------
! ------- Generic Interface Plot --------------------------------------------------
! ---------------------------------------------------------------------------------
      Interface Plot
       module procedure PlotSP, PlotDP, PlotListSP, PlotListDP
      end Interface

      CONTAINS

! ---------------------------------------------------------------------------------
! ------- Subroutine PlotSP -------------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine PlotSP (list_x, list_y, line, xlog, ylog, xlabel, &
                         ylabel, title, keep, refresh)

      implicit none
      integer,parameter :: r=4
      real(kind=r),dimension(:,:),intent(in),target :: list_x, list_y
      character(len=*),intent(in),optional :: xlabel, ylabel, title
      logical,intent(in),optional :: line, xlog, ylog, keep, refresh

      logical :: line$, xlog$, ylog$, keep$, refresh$
      character(len=120) :: xlabel$, ylabel$, title$
      integer :: nb$, nplot$
!
      real(kind=r),dimension(:,:),pointer :: list_x$, list_y$
      real(kind=4) :: minx, maxx, miny, maxy
      integer :: i, j, IRTN

      line$ = .true.
      if (present(line)) line$=line
      xlog$ = .false.
      if (present(xlog)) xlog$=xlog
      ylog$ = .false.
      if (present(ylog)) ylog$=ylog
      xlabel$  = 'x'
      if (present(xlabel)) xlabel$=trim(adjustl(xlabel))
      ylabel$  = 'y'
      if (present(ylabel)) ylabel$=trim(adjustl(ylabel))
      title$  = 'Representation of y=y(x)'
      if (present(title))  title$ =trim(adjustl(title))
      keep$ = .false.
      if (present(keep)) keep$=keep
      refresh$ = .false.
      if (present(refresh)) refresh$=refresh

      nb$    = min(size(list_x,dim=1),size(list_y,dim=1))
      nplot$ = min(size(list_x,dim=2),size(list_y,dim=2))

! Data limits
! -----------
      minx  = minval(list_x) ; maxx  = maxval(list_x)
      miny  = minval(list_y) ; maxy  = maxval(list_y)

      if (xlog$) then
       minx = log10(minval(list_x,list_x>0._r)) ; maxx = log10(maxx)
      endif
      if (ylog$) then
       miny = log10(minval(list_y,list_y>0._r))
       maxy = log10(maxval(list_y,list_y>0._r))
      endif
      if (miny == maxy .or. minx == maxx) return

! Prepare Screens
! ---------------
      call PrepareScreen(minx,maxx,miny,maxy,line$,xlog$,ylog$, &
                         xlabel$,ylabel$,title$,keep$,refresh$)

! Data Transformation
! -------------------
      if (ylog$ .or. xlog$) then
       allocate(list_x$(1:nb$,1:nplot$),list_y$(1:nb$,1:nplot$))
       list_x$ = list_x ; list_y$ = list_y
       if (xlog$) list_x$ = log10(list_x$)
       if (ylog$) then
        where (list_y$>0._r)
         list_y$ = log10(list_y$)
        elsewhere
         list_y$ = miny
        end where
       endif
      else
       list_x$ => list_x
       list_y$ => list_y
      endif

! Check whether data points are ordered
! -------------------------------------
      if (line$) then
check: do j = 1, nplot$
        do i = 1, nb$-1
         if (list_x$(i,j)>list_x$(i+1,j)) then
          line$ = .false.
          exit check
         endif
        enddo
       enddo check
      endif

! Draw Data
! ---------
      do j = 1, nplot$
       call GWmove2(IRTN,list_x$(1,j),list_y$(1,j))
       call GWsetpxl(IRTN,list_x$(1,j),list_y$(1,j),19)
       do i = 2, nb$
        if (ylog$ .and. list_y(i,j)==0._r) cycle
        if (line$) call GWline2(IRTN,list_x$(i,j),list_y$(i,j))
        call GWsetpxl(IRTN,list_x$(i,j),list_y$(i,j),19)
       enddo       
      enddo

      if (ylog$ .or. xlog$) deallocate(list_y$,list_x$)

! Kill Screen
! -----------
      call KillScreen (keep$)

      end Subroutine PlotSP

! ---------------------------------------------------------------------------------
! ------- Subroutine PlotDP ----------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine PlotDP (list_x, list_y, line, xlog, ylog, xlabel, &
                         ylabel, title, keep, refresh)

      implicit none
      integer, parameter :: r=8
      real(kind=r),dimension(:,:),intent(in),target :: list_x, list_y
      character(len=*),intent(in),optional :: xlabel, ylabel, title
      logical,intent(in),optional :: line, xlog, ylog, keep, refresh

      logical :: line$, xlog$, ylog$, keep$, refresh$
      character(len=120) :: xlabel$, ylabel$, title$
      integer :: nb$, nplot$
!
      real(kind=r),dimension(:,:),pointer :: list_x$, list_y$
      real(kind=4) :: minx, maxx, miny, maxy
      integer :: i, j, IRTN

      line$ = .true.
      if (present(line)) line$=line
      xlog$ = .false.
      if (present(xlog)) xlog$=xlog
      ylog$ = .false.
      if (present(ylog)) ylog$=ylog
      xlabel$  = 'x'
      if (present(xlabel)) xlabel$=trim(adjustl(xlabel))
      ylabel$  = 'y'
      if (present(ylabel)) ylabel$=trim(adjustl(ylabel))
      title$  = 'Representation of y=y(x)'
      if (present(title))  title$ =trim(adjustl(title))
      keep$ = .false.
      if (present(keep)) keep$=keep
      refresh$ = .false.
      if (present(refresh)) refresh$=refresh

      nb$    = min(size(list_x,dim=1),size(list_y,dim=1))
      nplot$ = min(size(list_x,dim=2),size(list_y,dim=2))

!     call PlotSP(real(list_x,4),real(list_y,4),             &
!                 line=line$,xlog=xlog$,ylog=ylog$,          &
!                 xlabel=xlabel$,ylabel=ylabel$,title=title$,&
!                 keep=keep$,refresh=refresh$)

! Data limits
! -----------
      minx  = minval(list_x) ; maxx  = maxval(list_x)
      miny  = minval(list_y) ; maxy  = maxval(list_y)

      if (xlog$) then
       minx = log10(minval(list_x,list_x>0._r)) ; maxx = log10(maxx)
      endif
      if (ylog$) then
       miny = log10(minval(list_y,list_y>0._r))
       maxy = log10(maxval(list_y,list_y>0._r))
      endif
      if (miny == maxy .or. minx == maxx) return

! Prepare Screens
! ---------------
      call PrepareScreen(minx,maxx,miny,maxy,line$,xlog$,ylog$, &
                         xlabel$,ylabel$,title$,keep$,refresh$)

! Data Transformation
! -------------------
      if (ylog$ .or. xlog$) then
       allocate(list_x$(1:nb$,1:nplot$),list_y$(1:nb$,1:nplot$))
       list_x$ = list_x ; list_y$ = list_y
       if (xlog$) list_x$ = log10(list_x$)
       if (ylog$) then
        where (list_y$>0._r)
         list_y$ = log10(list_y$)
        elsewhere
         list_y$ = miny
        end where
       endif
      else
       list_x$ => list_x
       list_y$ => list_y
      endif

! Draw Data
! ---------
      do j = 1, nplot$
       call GWmove2(IRTN,real(list_x$(1,j),4),real(list_y$(1,j),4))
       call GWsetpxl(IRTN,real(list_x$(1,j),4),real(list_y$(1,j),4),19)
       do i = 2, nb$
        if (ylog$ .and. list_y(i,j)==0._r) cycle
        if (line$) call GWline2(IRTN,real(list_x$(i,j),4),real(list_y$(i,j),4))
        call GWsetpxl(IRTN,real(list_x$(i,j),4),real(list_y$(i,j),4),19)
       enddo       
      enddo

      if (ylog$ .or. xlog$) deallocate(list_y$,list_x$)

! Kill Screen
! -----------
      call KillScreen (keep$)

      end Subroutine PlotDP

! ---------------------------------------------------------------------------------
! ------- Subroutine PlotListSP ---------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine PlotListSP (list_x, list_y, line, xlog, ylog, xlabel, &
                             ylabel, title, keep, refresh, nplot)

      implicit none
      integer, parameter :: r=4
      real(kind=r),dimension(:),intent(in),target :: list_x, list_y
      character(len=*),intent(in),optional :: xlabel, ylabel, title
      logical,intent(in),optional :: line, xlog, ylog, keep, refresh
      integer,intent(in),optional :: nplot

      logical :: line$, xlog$, ylog$, keep$, refresh$
      character(len=120) :: xlabel$, ylabel$, title$
      integer :: nb$, nplot$
!
      real(kind=r),dimension(:),pointer :: list_x$, list_y$
      real(kind=4) :: minx, maxx, miny, maxy
      integer :: i, j, IRTN
 
      line$ = .true.
      if (present(line)) line$=line
      xlog$ = .false.
      if (present(xlog)) xlog$=xlog
      ylog$ = .false.
      if (present(ylog)) ylog$=ylog
      xlabel$  = 'x'
      if (present(xlabel)) xlabel$=trim(adjustl(xlabel))
      ylabel$  = 'y'
      if (present(ylabel)) ylabel$=trim(adjustl(ylabel))
      title$  = 'Representation of y=y(x)'
      if (present(title))  title$ =trim(adjustl(title))
      keep$ = .false.
      if (present(keep)) keep$=keep
      refresh$ = .false.
      if (present(refresh)) refresh$=refresh
      nplot$ = 1
      if (present(nplot)) nplot$ = nplot

      nb$ = min(size(list_x),size(list_y)) / nplot$

!     call PlotSP(reshape(list_x,(/nb$,nplot$/)),reshape(list_y,(/nb$,nplot$/)), &
!                 line=line$,xlog=xlog$,ylog=ylog$,           &
!                 xlabel=xlabel$,ylabel=ylabel$,title=title$, &
!                 keep=keep$,refresh=refresh$)

! Data limits
! -----------
      minx  = minval(list_x) ; maxx  = maxval(list_x)
      miny  = minval(list_y) ; maxy  = maxval(list_y)

      if (xlog$) then
       minx = log10(minval(list_x,list_x>0._r)) ; maxx = log10(maxx)
      endif
      if (ylog$) then
       miny = log10(minval(list_y,list_y>0._r))
       maxy = log10(maxval(list_y,list_y>0._r))
      endif
      if (miny == maxy .or. minx == maxx) return

! Prepare Screens
! ---------------
      call PrepareScreen(minx,maxx,miny,maxy,line$,xlog$,ylog$, &
                         xlabel$,ylabel$,title$,keep$,refresh$)

! Data Transformation
! -------------------
      if (ylog$ .or. xlog$) then
       allocate(list_x$(1:nb$*nplot$),list_y$(1:nb$*nplot$))
       list_x$ = list_x ; list_y$ = list_y
       if (xlog$) list_x$ = log10(list_x$)
       if (ylog$) then
        where (list_y$>0._r)
         list_y$ = log10(list_y$)
        elsewhere
         list_y$ = miny
        end where
       endif
      else
       list_x$ => list_x
       list_y$ => list_y
      endif

! Draw Data
! ---------
      do j = 1, nplot$
       call GWmove2(IRTN,list_x$(1+(j-1)*nb$),list_y$(1+(j-1)*nb$))
       call GWsetpxl(IRTN,list_x$(1+(j-1)*nb$),list_y$(1+(j-1)*nb$),19)
       do i = 2, nb$
        if (ylog$ .and. list_y(i+(j-1)*nb$)==0._r) cycle
        if (line$) call GWline2(IRTN,list_x$(i+(j-1)*nb$),list_y$(i+(j-1)*nb$))
        call GWsetpxl(IRTN,list_x$(i+(j-1)*nb$),list_y$(i+(j-1)*nb$),19)
       enddo       
      enddo

      if (ylog$ .or. xlog$) deallocate(list_y$,list_x$)

! Kill Screen
! -----------
      call KillScreen (keep$)

      end Subroutine PlotListSP

! ---------------------------------------------------------------------------------
! ------- Subroutine PlotListDP ---------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine PlotListDP (list_x, list_y, line, xlog, ylog, xlabel, &
                             ylabel, title, keep, refresh, nplot)

      implicit none
      integer, parameter :: r=8
      real(kind=r),dimension(:),intent(in),target :: list_x, list_y
      character(len=*),intent(in),optional :: xlabel, ylabel, title
      logical,intent(in),optional :: line, xlog, ylog, keep, refresh
      integer,intent(in),optional :: nplot

      logical :: line$, xlog$, ylog$, keep$, refresh$
      character(len=120) :: xlabel$, ylabel$, title$
      integer :: nb$, nplot$
!
      real(kind=r),dimension(:),pointer :: list_x$, list_y$
      real(kind=4) :: minx, maxx, miny, maxy
      integer :: i, j, IRTN

      line$ = .true.
      if (present(line)) line$=line
      xlog$ = .false.
      if (present(xlog)) xlog$=xlog
      ylog$ = .false.
      if (present(ylog)) ylog$=ylog
      xlabel$  = 'x'
      if (present(xlabel)) xlabel$=trim(adjustl(xlabel))
      ylabel$  = 'y'
      if (present(ylabel)) ylabel$=trim(adjustl(ylabel))
      title$  = 'Representation of y=y(x)'
      if (present(title))  title$ =trim(adjustl(title))
      keep$ = .false.
      if (present(keep)) keep$=keep
      refresh$ = .false.
      if (present(refresh)) refresh$=refresh
      nplot$ = 1
      if (present(nplot)) nplot$ = nplot

      nb$ = min(size(list_x),size(list_y)) / nplot$

!     call PlotSP(reshape(real(list_x,4),(/nb,nseries$/)),reshape(real(list_y,4),(/nb,nseries$/)), &
!                 line=line$,xlog=xlog$,ylog=ylog$, &
!                 xlabel=xlabel$,ylabel=ylabel$,title=title$, &
!                 keep=keep$,refresh=refresh$)

! Data limits
! -----------
      minx  = minval(list_x) ; maxx  = maxval(list_x)
      miny  = minval(list_y) ; maxy  = maxval(list_y)

      if (xlog$) then
       minx = log10(minval(list_x,list_x>0._r)) ; maxx = log10(maxx)
      endif
      if (ylog$) then
       miny = log10(minval(list_y,list_y>0._r))
       maxy = log10(maxval(list_y,list_y>0._r))
      endif
      if (miny == maxy .or. minx == maxx) return

! Prepare Screens
! ---------------
      call PrepareScreen(minx,maxx,miny,maxy,line$,xlog$,ylog$, &
                         xlabel$,ylabel$,title$,keep$,refresh$)

! Data Transformation
! -------------------
      if (ylog$ .or. xlog$) then
       allocate(list_x$(1:nb$*nplot$),list_y$(1:nb$*nplot$))
       list_x$ = list_x ; list_y$ = list_y
       if (xlog$) list_x$ = log10(list_x$)
       if (ylog$) then
        where (list_y$>0._r)
         list_y$ = log10(list_y$)
        elsewhere
         list_y$ = miny
        end where
       endif
      else
       list_x$ => list_x
       list_y$ => list_y
      endif

! Draw Data
! ---------
      do j = 1, nplot$
       call GWmove2(IRTN,real(list_x$(1+(j-1)*nb$),4),real(list_y$(1+(j-1)*nb$),4))
       call GWsetpxl(IRTN,real(list_x$(1+(j-1)*nb$),4),real(list_y$(1+(j-1)*nb$),4),19)
       do i = 2, nb$
        if (ylog$ .and. list_y(i+(j-1)*nb$)==0._r) cycle
        if (line$) call GWline2(IRTN,real(list_x$(i+(j-1)*nb$),4),real(list_y$(i+(j-1)*nb$),4))
        call GWsetpxl(IRTN,real(list_x$(i+(j-1)*nb$),4),real(list_y$(i+(j-1)*nb$),4),19)
       enddo       
      enddo

      if (ylog$ .or. xlog$) deallocate(list_y$,list_x$)

! Kill Screen
! -----------
      call KillScreen (keep$)

      end Subroutine PlotListDP

! ---------------------------------------------------------------------------------
! ------- Subroutine PrepareScreen ------------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine PrepareScreen (minx,maxx,miny,maxy,line$,xlog$,ylog$, &
                                xlabel$,ylabel$,title$,keep$,refresh$)

      implicit none
      real(kind=4),intent(in) :: minx, maxx, miny, maxy
      logical,intent(in) :: line$, xlog$, ylog$, keep$, refresh$
      character(len=120),intent(inout) :: xlabel$, ylabel$, title$
!
      integer, parameter :: r=4
      character(len=120) :: formatx, formaty, nd1$, nd2$
      character(len=15) :: indice
      real(kind=4) :: xcoord, ycoord, xorig, yorig, xfin, yfin, unitx, unity, pxlx, pxly, pos, W, H
      integer :: lenx, leny, n, i, j, resx, resy, nb, nd, nplot, IRTN
      integer,save :: unit$
      logical,save :: opened
      
      call Purge(xlabel$)                                ! purge from pgplot symbols
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
      else
       call GWCLEAR(IRTN,0)
      endif
      
! - Size of the window
! --------------------
      call GWSIZE(IRTN,1,resx,resy)                ! get numbers of pixels      
      
! - Parameters of the representation
! ----------------------------------
      xcoord = min(max(0._r,minx),maxx)            ! coordinates of the two axis
      ycoord = min(max(0._r,miny),maxy)

      xfin  = maxx + (maxx-minx)/30._r             ! representation limits
      xorig = minx - (maxx-minx)/30._r
      yfin  = maxy + (maxy-miny)/13._r
      yorig = miny - (maxy-miny)/13._r

      pxlx  = 6.5 * (xfin-xorig)/(resx-7)            ! pixel size (expanded)
      pxly  = 6.5 * (yfin-yorig)/(resy-7)

      if (xlog$) xlabel$ = 'log('//trim(adjustl(xlabel$))//')'
      lenx    = len(trim(xlabel$))
      unitx   = 10._r**(floor(log10((xfin-xorig)/2.5_r))) ! tick spacing for X
      if ((xfin-xorig)/unitx > 15._r) unitx = unitx * 5._r
      if (unitx < 1._r .or. unitx > 1.E4_r) then          ! format for values
       nd = max (2,min(2-floor(log10(abs(xfin-xorig)/max(abs(xfin),abs(xorig)))),16)) ! nombre de chiffres apres la virgule
       write(nd1$,'(i4)') nd
       write(nd2$,'(i4)') nd+6
       formatx = "(g"//trim(adjustl(nd2$))//"."//trim(adjustl(nd1$))//")"
      else
       formatx = '(f8.0)'
      endif

      if (ylog$) ylabel$ = 'log('//trim(adjustl(ylabel$))//')'
      leny    = len(trim(ylabel$))
      unity   = 10._r**(floor(log10((yfin-yorig)/4._r)))  ! tick spacing for Y
      if ((yfin-yorig)/unity > 14._r) unity = unity * 5._r
      if (unity < 1._r .or. unity > 1.E4_r) then          ! format for values
       nd = max (2,min(2-floor(log10(abs(yfin-yorig)/max(abs(yfin),abs(yorig)))),16)) ! nombre de chiffres apres la virgule
       write(nd1$,'(i4)') nd
       write(nd2$,'(i4)') nd+6
       formaty = "(g"//trim(adjustl(nd2$))//"."//trim(adjustl(nd1$))//")"
      else
       formaty = '(f8.0)'
      endif

      pos = nint(xcoord/unitx)*unitx
      if ( xorig+ 4*pxlx < pos .and. pos < xfin ) xcoord = pos
      pos = nint(ycoord/unity)*unity
      if ( yorig+16*pxly < pos .and. pos < yfin .and. pos < ycoord ) ycoord = pos

! - Set world coordinates
! -----------------------        
      call GWINDOW(IRTN,xorig,yorig,xfin,yfin)        ! define world coordinates
        
! B. Draw axis
! ++++++++++++
      call GWline(IRTN,xorig,ycoord,xfin,ycoord)      ! draw X axis
      call GWmove2(IRTN,xfin-4*pxlx,ycoord+4*pxly)    ! draw arrow on X axis
      call GWline2(IRTN,xfin,ycoord)                  
      call GWline2(IRTN,xfin-4*pxlx,ycoord-4*pxly)

      call GWline(IRTN,xcoord,yorig,xcoord,yfin)      ! draw Y axis
      call GWmove2(IRTN,xcoord+4*pxlx,yfin-4*pxly)    ! draw arrow on Y axis
      call GWline2(IRTN,xcoord,yfin)                  
      call GWline2(IRTN,xcoord-4*pxlx,yfin-4*pxly)

      call GWSETTXT(IRTN,-1,-1,2,-1,-1,'')            ! bottom-right reference point
      call GWputtxt(IRTN,xfin-7*pxlx,ycoord+5*pxly,xlabel$(:lenx)) ! write the label for X

      if (xcoord-(9*leny+5)*pxlx >= xorig) then       ! write the label for Y
       call GWSETTXT(IRTN,-1,-1,3,-1,-1,'')           ! top-right reference point
       call GWputtxt(IRTN,xcoord-5*pxlx,yfin-4*pxly,ylabel$(:leny))  
      else              
       pos = xcoord+5*pxlx
       if (floor((yfin-ycoord)/unity)/= 0 .and. ycoord+floor((yfin-ycoord)/unity)*unity>yfin-20*pxly) then
        write (indice,formaty) ycoord+floor((yfin-ycoord)/unity)*unity
        pos = pos + (len(trim(adjustl(indice)))) * 9*pxlx
       endif
       call GWSETTXT(IRTN,-1,-1,4,-1,-1,'')           ! top-left reference point
       call GWputtxt(IRTN,pos,yfin-4*pxly,ylabel$(:leny))
      endif

      call GWSETTXT(IRTN,-1,-1,8,-1,-1,'')         ! top-centered reference point
      do i = ceiling((xorig-xcoord)/unitx), floor((xfin-xcoord)/unitx)
       pos = xcoord + i * unitx                    ! draw ticks and values for X axis
       call GWmove2(IRTN,pos,ycoord+2*pxly)
       call GWline2(IRTN,pos,ycoord-2*pxly)
       if (i/=0) then
        write (indice,formatx) pos
        nd = index(indice,"0E")                    ! purge 
        if (nd/=0) indice = trim(adjustl(indice(:nd-1)))//trim(adjustl(indice(nd+1:)))
        nd = index(indice,"E-0")
        if (nd/=0) indice = trim(adjustl(indice(:nd+1)))//trim(adjustl(indice(nd+3:)))
        nd = index(indice,"E+0")
        if (nd/=0) indice = trim(adjustl(indice(:nd)))//trim(adjustl(indice(nd+3:)))
        lenx = len(trim(adjustl(indice)))
        call GWputtxt(IRTN,pos,ycoord-2*pxly,trim(adjustl(indice)))
       endif
      enddo

      call GWSETTXT(IRTN,-1,-1,5,-1,-1,'')         ! left-centered reference point
      do i = ceiling((yorig-ycoord)/unity), floor((yfin-ycoord)/unity)
       pos = ycoord + i * unity                    ! draw ticks and values for Y axis
       call GWmove2(IRTN,xcoord-2*pxlx,pos)
       call GWline2(IRTN,xcoord+2*pxlx,pos)
       if (i/=0) then
        write (indice,formaty) pos
        nd = index(indice,"0E")                    ! purge 
        if (nd/=0) indice = trim(adjustl(indice(:nd-1)))//trim(adjustl(indice(nd+1:)))
        nd = index(indice,"E-0")
        if (nd/=0) indice = trim(adjustl(indice(:nd+1)))//trim(adjustl(indice(nd+3:)))
        nd = index(indice,"E+0")
        if (nd/=0) indice = trim(adjustl(indice(:nd)))//trim(adjustl(indice(nd+3:)))
        if (xcoord+(3+9*len(trim(adjustl(indice))))*pxlx<xfin) then
         call GWputtxt(IRTN,xcoord+4*pxlx,pos+2*pxly,trim(adjustl(indice)))
        else
         call GWputtxt(IRTN,xcoord-(4+9*len(trim(adjustl(indice))))*pxlx,pos+2*pxly,trim(adjustl(indice)))
        endif
       endif
      enddo

      if (title$/="") call GWSETMSG(IRTN,trim(title$))      ! write title in status bar
      
      end Subroutine PrepareScreen

! ---------------------------------------------------------------------------------
! ------- Internal Subroutine Kill Screen -----------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine KillScreen (keep$)

      logical,intent(in) :: keep$

      integer :: IRTN

      if (.not.keep$) call GWQUITX(IRTN,0)

      end Subroutine KillScreen

! ---------------------------------------------------------------------------------
! ------- Internal Subroutine Purge -----------------------------------------------
! ---------------------------------------------------------------------------------
      Subroutine Purge(var$)

      character(len=*),intent(inout) :: var$
      integer :: pos

      do while(index(var$,"\u") /= 0)
       pos  = index(var$,"\u")
       var$ = trim(adjustl(var$(:pos-1)))//trim(var$(pos+2:))
      enddo
      do while(index(var$,"\d") /= 0)
       pos  = index(var$,"\d")
       var$ = trim(adjustl(var$(:pos-1)))//trim(var$(pos+2:))
      enddo
      do while(index(var$,"\g") /= 0)
       pos  = index(var$,"\g")
       var$ = trim(adjustl(var$(:pos-1)))//trim(var$(pos+2:))
      enddo

      end Subroutine Purge

      end Module PlotPack