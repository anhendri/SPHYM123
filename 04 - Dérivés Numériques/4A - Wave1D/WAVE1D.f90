!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE KINDS
module KINDS
  implicit none
  integer, parameter :: k = 4                                                                                  ! Définition de la précision
end module KINDS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program WAVE1D
  use KINDS                                                                                                    ! Module de précision
  use PLOTPACK                                                                                                 ! Module d'affichage graphique
  implicit none

  real(kind = k)              :: L = 100., VTSS = 10., R = 5., PASX = .1, PAST, FREQ = .5, pi = 3.1415926536   ! Définition des constantes du système
  real(kind = k), allocatable :: POSY(:,:), POSX(:)                                                            ! Vecteur et matrice des positions
  integer                     :: INDX, N, LIMT = 1000000, PART = 4

    PAST = PASX/VTSS                                                                                           ! Calcul de l'intervale de temps
    N = int(L/PASX)                                                                                            ! Calcul du nombre de points
    allocate(POSX(0:N), POSY(0:N,0:2))                                                                         ! Allocation de la mémoire
    POSX = PASX*real((/(INDX, INDX = 0,N)/),k)                                                                 ! Calcul des positions en x
    POSY = 0._k                                                                                                ! Initialisation des positions en y
    POSY(:,1) = exp(-((POSX-L/2.)/R)**2)                                                                       ! Initialisation des positions en y

    select case(PART)                                                                                          ! Choix des conditions initiales en fonction de la partie du programme désirée
        case(1)
            POSY(1:N-1,2) = POSY(1:N-1,1)+(VTSS*PAST/PASX)**2.*(POSY(:N-2,1)-2*POSY(1:N-1,1)+POSY(2:,1))
        case(2:3)
            POSY(1:N-1,2) = POSY(:N-2,1)
    end select
    
    call plot(POSX, POSY(:,1), keep=.true., refresh=.false.)                                                   ! Affichage d'un premier graphe

    do INDX = 0, LIMT                                                                                          ! Pendant un certain temps ...
        if(PART==3) then                                                                                       ! En fonction du cas que nous voulons
            POSY([0,N],2) = POSY([1,N-1],1)                                                                    ! On applique les conditions particulières
        elseif(PART==4) then
            POSY(N/2,2) = POSY(N/2,2) + sin(2*pi*FREQ*INDX*PAST)
        endif
        POSY(:,:1) = POSY(:,1:)                                                                                ! Actualisation des conditions initiales
        call plot(POSX, POSY(:,1), keep=.true., refresh=.true.)                                                ! Actualisation du graphe
        POSY(1:N-1,2) = POSY(:N-2,1) + POSY(2:,1) - POSY(1:N-1,0)                                              ! Calcul des nouvelles positions
    end do

end program
