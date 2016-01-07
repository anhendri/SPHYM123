!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE KINDS
module KINDS
  implicit none
  integer, parameter :: R = 4
end module KINDS


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE INTERFACES
module INTERFACES
  implicit none
    interface
        subroutine INIT_RANDOM_SEED()
            integer                            :: INDX, N, CLCK
            integer, dimension(:), allocatable :: SEED
        end subroutine
        function SQRT_CMPLX(Z)
            use KINDS
            complex(kind = R)             :: SQRT_CMPLX
            complex(kind = R), intent(in) :: Z
        end function SQRT_CMPLX
    end interface
end module INTERFACES


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program JULIA
#define ZINI(POSX, POSY) cmplx((POSX-1)*PASX-2._R,(POSY-1)*PASY-2._R)                                          ! MACRO-DEFINITION : Nécessite de compiler avec -cpp !!!
  use KINDS
  use INTERFACES
  implicit none

  real(kind = R)                 :: PASX, PASY, IPRT, RPRT, RAND
  complex(kind = R)              :: CSTE, ZTMP
  integer                        :: DIMX, DIMY, POSX, POSY, INDX, LIMT, NIV1, PRFD

    open(100, file='INPUT.dat')
        read(100,*) DIMX, DIMY, LIMT, PRFD, CSTE
    close(100)

    call INIT_RANDOM_SEED()

    PASX = 4._R/(DIMX-1)
    PASY = 4._R/(DIMY-1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! CALCUL DE L'ENSEMBLE DE JULIA

    open(120, file = "OUTPUT_1.dat")                                                                           ! Ouverture du fichier contenant les données de sortie
    write(120,"(1I5)") DIMX, DIMY                                                                              ! Écriture des dimensions (nombres de discrétisations)
    write(120,"(1e11.3)") PASX*(/(POSX, POSX = 0,DIMX-1)/)-2._R, PASY*(/(POSY, POSY = 0,DIMY-1)/)-2._R         ! Écriture des valeurs réelles et imaginaires de z

    do POSX = 1,DIMX                                                                                           ! Pour chaque point de la grille ...
        do POSY = 1,DIMY
            NIV1 = 0                                                                                           ! Initialisation du niveau à 0
            ZTMP = ZINI(POSX, POSY)                                                                            ! Calcul du z0
            if(cabs(ZTMP) < max(2._R, cabs(CSTE))) NIV1 = 1                                                    ! Calcul de la profondeur initiale
            do INDX = 1,PRFD                                                                                   ! Pour chaque niveau de profondeur (valeur de n)
                ZTMP = ZTMP**2 + CSTE                                                                          ! On calcule zn
                if(cabs(ZTMP) < max(2._R, cabs(CSTE))) NIV1 = NIV1 + 1                                         ! Si la condition est vérifiée on incrémente le niveau
            enddo
            write(120,"(1I3)") NIV1                                                                            ! Écriture du niveau de gris dans le fichier
        enddo
    enddo
    close(120)                                                                                                 ! Fermeture du fichier

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! CALCUL DIRECT DU FRACTALE

    ZTMP = .5_R + SQRT_CMPLX(1._R - 4._R*CSTE)*sign(.5_R, cabs(1._R + SQRT_CMPLX(1._R - 4._R*CSTE)) - 1._R)    ! Calcul de la solution de l'équation

    open(120, file = "OUTPUT_2.dat")                                                                           ! Meme chose que pour Julia

    do INDX = 1, LIMT                                                                                          ! Pour chaque pas d'incrémentation ...
        RPRT = real(ZTMP)                                                                                      ! Calcul de la partie réelle de z
        IPRT = aimag(ZTMP)                                                                                     ! Calcul de la partie imaginaire de z
        if(max(abs(RPRT),abs(IPRT))<2._R) write(120,*) RPRT, IPRT                                              ! Test de l'appartenance de z dans le carré [-2,2]x[-2,2] et écriture
        ZTMP = SQRT_CMPLX(ZTMP-CSTE)                                                                           ! Calcul du z suivant
        call random_number(RAND)                                                                               ! Génération d'un nombre aléatoire
        if (RAND < 0.5)  ZTMP = -ZTMP                                                                          ! Choix du signe de la racine en fonction du nombre aléatoire
    enddo
    close(120)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! CALCUL DE L'ENSEMBLE DE MANDELBROOT                                                                         ! Même chose que Julia sauf que la constante change

    open(120, file = "OUTPUT_3.dat")
    write(120,"(1I5)") DIMX, DIMY
    write(120,"(1e11.3)") PASX*(/(POSX, POSX = 0,DIMX-1)/)-2._R, PASY*(/(POSY, POSY = 0,DIMY-1)/)-2._R

    do POSX = 1,DIMX
        do POSY = 1,DIMY
            NIV1 = 0
            ZTMP = ZINI(POSX, POSY)
            do INDX = 1,PRFD
                ZTMP = ZTMP**2+ZINI(POSX, POSY)
                if(cabs(ZTMP) < max(2._R, cabs(cmplx((POSX-1)*PASX-2._R,(POSY-1)*PASY-2._R)))) &
                NIV1 = NIV1 + 1
            enddo
            write(120,"(1I3)") NIV1
        enddo
    enddo
    close(120)

end program JULIA



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! FONCTION SQRT_CMPLX
function SQRT_CMPLX(Z)
  use KINDS
  complex(kind = R)            :: SQRT_CMPLX
  complex(kind = R),intent(in) :: Z

    SQRT_CMPLX = sqrt(abs(Z))*exp(cmplx(0,atan2(aimag(Z),real(Z,R))/2._R))

end function SQRT_CMPLX



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! SOUS-ROUTINE INIT_RANDOM_SEED
subroutine INIT_RANDOM_SEED()
  integer                            :: INDX, N, CLCK
  integer, dimension(:), allocatable :: SEED

    call random_seed(size = N)
    allocate(SEED(N))

    call system_clock(COUNT = CLCK)

    SEED = CLCK + 37*(/(INDX, INDX=0, N-1)/)
    call random_seed(PUT = SEED)

    deallocate(SEED)
end subroutine



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! VERSION ALTERNATIVE DU PROGRAMME (PLUS COURT, MOINS DE BOUCLE MAIS PLUS DE STOCKAGE)
!program JULIA
! use KINDS
! use INTERFACES
! implicit none
!
! real(kind = R)                 :: PASX, PASY, IPRT, RPRT, RAND
! complex(kind = R), allocatable :: ZINI(:,:), JLIA(:,:), MNDB(:,:)
! complex(kind = R)              :: CSTE, ZTMP
! integer, allocatable           :: LEVL(:,:,:)
! integer                        :: DIMX, DIMY, POSX, POSY, INDX, LIMT, PRFD
!
!   open(100, file='INPUT.dat')
!       read(100,*) DIMX, DIMY, LIMT, PRFD, CSTE
!   close(100)
!
!   allocate(LEVL(DIMX,DIMY,3), ZINI(DIMX, DIMY))
!
!   call INIT_RANDOM_SEED()
!
!   PASX = 4._R/(DIMX-1)
!   PASY = 4._R/(DIMY-1)
!
!   ZINI = reshape((/((/(cmplx(POSX*PASX,POSY*PASY), POSX=0,DIMX-1)/),POSY=0,DIMY-1)/),[DIMX,DIMY])-cmplx(2._R,2._R)
!
!   LEVL = 0
!
!   where(cabs(ZINI) < max(2._R, cabs(CSTE))) LEVL(:,:,1) = 1
!   JLIA = ZINI
!   MNDB = ZINI
!   do INDX = 1, PRFD
!       JLIA = JLIA**2+CSTE
!       MNDB = MNDB**2+ZINI
!       where(cabs(JLIA) < max(2._R, cabs(CSTE))) LEVL(:,:,1) = LEVL(:,:,1) + 1
!       where(cabs(MNDB) < max(2._R, cabs(ZINI))) LEVL(:,:,3) = LEVL(:,:,3) + 1
!   enddo
!
!   ZTMP = .5_R + SQRT_CMPLX(1._R - 4._R*CSTE)*sign(.5_R, cabs(1._R + SQRT_CMPLX(1._R - 4._R*CSTE)) - 1._R)
!
!   do INDX = 1, LIMT
!       RPRT = real(ZTMP)
!       IPRT = aimag(ZTMP)
!       if(max(abs(RPRT),abs(IPRT))<2._R) LEVL(int((RPRT+2._r)/PASX)+1,int((IPRT+2._r)/PASY)+1,2) = 1
!       ZTMP = SQRT_CMPLX(ZTMP-CSTE)
!       call random_number(RAND)
!       if(RAND < 0.5)  ZTMP = -ZTMP
!   enddo
!
!   do INDX = 1,3
!       open(120, file = "OUTPUT_"//achar(48+INDX)//".dat")
!           write(120,"(1I4)") DIMX, DIMY
!           write(120,"(1e11.3)") real(ZINI(:,1)), aimag(ZINI(1,:))
!           write(120,"(1I2)") transpose(LEVL(:,:,INDX))
!       close(120)
!   end do
!
!end program JULIA
