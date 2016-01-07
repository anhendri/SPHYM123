!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE KINDS
module KINDS
  implicit none
  integer, parameter :: R = 4                                                                                  ! Definition de la precision
end module KINDS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE COMMON_VALUES
module COMMON_VALUES
  use kinds
  implicit none
  integer, save                     :: DIM0, DIM2                                                              ! Dimensions des vecteurs
  real(kind = R), allocatable, save :: CRNT(:), P(:), Y2(:), I(:), POWR(:)                                     ! Vecteurs des grandeurs physiques
end module COMMON_VALUES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE INTERFACES
module INTERFACES
    interface
        function FUNC(X)
          use KINDS
          use nr, only :  splint
          use COMMON_VALUES
          implicit none
          real(kind = R)             :: FUNC
          real(kind = R), intent(in) :: X
        end function FUNC
    end interface
end module INTERFACES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program INTERPOLATION
  use KINDS
  use nr, only : spline, splint, golden                                                                        ! Sous-routines externe utilisees
  use INTERFACES
  use COMMON_VALUES
  implicit none

  integer                     :: INDX, DIM1, MINX(1)
  real(kind = R), allocatable :: X(:), Y(:), VOLT(:)
  real(kind = R)              :: XMIN

    open(100, file='INPUT/donnees.dat')                                                                        ! Ouverture du fichier d'entree
        read(100, *) DIM1, DIM2                                                                                ! Lecture des dimensions des vecteurs
    close(100)                                                                                                 ! Fermeture du fichier d'entree

    open(101, file='INPUT/cell.dat')                                                                           ! Ouverture du second fichier d'entree
    read(101, *)                                                                                               ! On ignore la premiere ligne (les titres)
    read(101, *) DIM0                                                                                          ! Lecture du nombre de points
    allocate(VOLT(DIM0), CRNT(DIM0), Y2(DIM0))                                                                 ! Allocation des vecteurs que l'on va lire
    read(101,*) (CRNT(INDX), VOLT(INDX),INDX = 1,DIM0)                                                         ! Lecture du courant et de la tension
    close(101)                                                                                                 ! Fermeture du fichier

    call spline(CRNT, VOLT, 1.E30, 1.E30, Y2)                                                                  ! Calcul des derivees secondes

    X = CRNT(1) + (/(INDX, INDX = 0,DIM1-1)/)*(CRNT(DIM0)-CRNT(1))/(DIM1-1)                                    ! Discretisation du courant
    Y = (/(splint(CRNT, VOLT, Y2, X(INDX)), INDX =1,DIM1)/)                                                    ! Interpolation de la tension pour le "nouveau" courant
    POWR = CRNT * VOLT                                                                                         ! Calcul de la puissance pour les points du fichier

    open(102, file ="OUTPUT_0.dat")                                                                            ! Ouverture du premier fichier de sortie
    write(102,'(3E11.3)') (CRNT(INDX), VOLT(INDX), POWR(INDX),INDX = 1,DIM0)                                   ! Ecridure des grandeurs mesurees et de la puissance
    close(102)                                                                                                 ! Fermeture du fichier

    open(102, file ="OUTPUT_1.dat")                                                                            ! Ouverture du deuxieme fichier de sortie
    write(102,'(2E11.3)') (X(INDX), Y(INDX), INDX = 1,DIM1)                                                    ! Ecriture du courant discretise et de la tension interpolee
    close(102)                                                                                                 ! Fermeture du fichier

    call spline(CRNT, POWR, 1.E30, 1.E30, Y2)                                                                  ! Calcul des derivees secondes de l'interpolation

    I = CRNT(1) + (/(INDX, INDX = 0,DIM2-1)/)*(CRNT(DIM0)-CRNT(1))/(DIM2-1)                                    ! Discretisation du courant (pas forcemet le meme nombre de points qu'avant)
    P = -(/(FUNC(I(INDX)), INDX = 1,DIM2)/)                                                                    ! Calcul de la puissance par interpolation

    open(103, file ="OUTPUT_2.dat")                                                                            ! Ouverture du troisieme fichier de sortie
    write(103,'(2E11.3)') (I(INDX), P(INDX), INDX = 1,DIM2)                                                    ! Ecriture du courant discretise et de la puissance interpolee
    close(103)                                                                                                 ! Fermeture du fichier

    MINX = maxloc(P,P.GT.max(P(1),P(DIM2)))                                                                    ! Recherche du minimum de FUNC parmis les points disponibles
    
    MINX(1) = golden(I(1), I(MINX(1)), I(DIM2), FUNC, 1.E-7, XMIN)                                             ! Calcul du vrai minimum
    write(*,*) XMIN                                                                                            ! Affichage du resultat a l'ecran

end program INTERPOLATION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! FONCTION FUNC
function FUNC(X) 
  use kinds
  use nr, only :  splint
  use COMMON_VALUES
  implicit none

  real(kind = R)             :: FUNC
  real(kind = R), intent(in) :: X

    FUNC = -splint(CRNT,POWR,Y2,X)

end function FUNC
