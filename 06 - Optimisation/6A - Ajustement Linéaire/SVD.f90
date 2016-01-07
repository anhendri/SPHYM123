!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE KINDS
module KINDS
  implicit none
  integer, parameter :: R = 8
end module KINDS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program SVD
  use KINDS
  implicit none

  integer                     :: NPTS, INDX, DMIN, LWRK, INFO, EXON=2, N, M, NMBR
  integer, allocatable        :: IWRK(:)
  real(kind = R), allocatable :: PNTX(:),PNTY(:),SING(:),WORK(:),COEF(:),YCLC(:),A(:,:),&
                                 U(:,:),VT(:,:),SAVA(:,:),XPTS(:),EXP1(:),EXP2(:),EXP3(:)
  real(kind = R)              :: STEP
                                 
    select case(EXON)
        case(1)                    
            open(100, file='INPUT/INPUT1.dat')
                read(100, *)
                read(100, *) NPTS
                allocate(PNTX(NPTS), PNTY(NPTS))
                read(100, *) (PNTX(INDX), PNTY(INDX), INDX=1,NPTS)
            close(100)

            DMIN = min(NPTS,3)
            LWRK = 3*DMIN**2+max(NPTS,3,4*DMIN*(DMIN+1))

            allocate(SING(DMIN), U(NPTS,DMIN), VT(DMIN,3), WORK(LWRK), IWRK(8*DMIN))

            A = reshape([(/(1._R,INDX=1,NPTS)/),PNTX**2/exp(PNTX),sin(PNTX)**2],[NPTS,3])
            SAVA = A

            call DGESDD('S',NPTS,3,A,NPTS,SING,U,NPTS,VT,DMIN,WORK,LWRK,IWRK,INFO)

            COEF = matmul(transpose(U), PNTY)/SING
            where(SING == 0._R) COEF = 0._R
            COEF = matmul(transpose(VT), COEF)
            YCLC = matmul(SAVA, COEF)

            open(120, file ="OUTPUT1.dat")
            write(120, '(3E11.3)') (PNTX(INDX), PNTY(INDX), YCLC(INDX), INDX=1,NPTS)
            close(120)
        
        case(2)
            open(100, file='INPUT/INPUT2.dat')
                read(100,*) N, M
            close(100)

            STEP = 5._r/N
            DMIN = min(N,M)
            LWRK = 3*DMIN**2+max(N,M,4*DMIN*(DMIN+1))

            allocate(IWRK(8*DMIN), U(N,DMIN), VT(DMIN,M), WORK(LWRK), SING(DMIN))

            XPTS = STEP*real((/(INDX-1, INDX=1,N)/), R)
            EXP1 = exp(XPTS)

            A = reshape((/(XPTS**INDX, INDX=0,M-1)/),[N,M])
            SAVA = A
            COEF = 1._r/(/(product(real((/(NMBR, NMBR=1,INDX)/),R)), INDX=0,M-1)/)
            EXP2 = matmul(A, COEF)

            call dgesdd('S', N, M, A, N, SING, U, N, VT, DMIN, WORK, LWRK, IWRK, INFO)

            COEF = matmul(transpose(U), EXP1)/SING
            where(SING == 0._R) COEF = 0._R
            COEF = matmul(transpose(VT), COEF)
            EXP3  = matmul(SAVA, COEF)

            open(120, file ="OUTPUT2.dat")
            write(120, '(4E11.3)') (XPTS(INDX), EXP1(INDX), EXP2(INDX), EXP3(INDX), INDX=1,N)
            close(120)
        end select
        
end program SVD
