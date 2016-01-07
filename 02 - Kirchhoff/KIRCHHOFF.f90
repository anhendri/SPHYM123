!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program KIRCHHOFF
  use forsythe, only : decomp, solve                                                                           ! Utilisation de modules
  use nrtype
  implicit none

  integer                     :: INDX, NBCT, IDEX, NMAX
  integer, allocatable        :: IPVT(:), SYST(:)
  real(kind = 8)              :: RSST, COND, V, RSEQ
  real(kind = 8), allocatable :: WORK(:), B(:), MTRX(:,:)

    open(100, file='INPUT.dat')                                                                                ! Ouverture du fichier d'entrée
        read(100, *) NMAX, RSST, V                                                                             ! Lecture des paramètres
    close(100)                                                                                                 ! Fermeture du fichier

    do NBCT = 4, NMAX                                                                                          ! Pour chaque valeur du nombre de boucles
        allocate(MTRX(NBCT+1,NBCT+1), IPVT(NBCT+1), WORK(NBCT+1))                                              ! On change les dimensions des vecteurs/matrices

        B = [V, (/(0._8, INDX=1,NBCT)/)]                                                                       ! Génrétation du vecteur B
        SYST = [-1, -1, (/(0, INDX=1,NBCT-3)/), -1, 3, -1, (/(0,INDX=1,NBCT-3)/), -1, -1]                      ! Génération d'un vecteur super utile (voir plus bas)

        MTRX(:,1)  = RSST*[(NBCT+1)/2, -1, (/(0,INDX=1,NBCT/2)/), (/(-1,INDX=NBCT/2+2,NBCT)/)]                 ! Création de la première ligne de la matrice
        MTRX(:,2:) = RSST*reshape((/(SYST(NBCT+2-INDX:2*NBCT+2-INDX), INDX = 2,NBCT+1)/),NBCT+[1,0])           ! Création des autres lignes de la matrice
        MTRX(1,:) = MTRX(:,1)                                                                                  ! Modification de la première colonne de la matrice

        call decomp(NBCT+1, NBCT+1, MTRX, COND, IPVT, WORK)                                                    ! Calcul de la décomposition de la matrice
        call solve(NBCT+1, NBCT+1, MTRX, B, IPVT)                                                              ! Résolution du système

        RSEQ = V/B(1)                                                                                          ! Calcul de la résistance équivalente
        write(*,'(I3, 4E13.5)') NBCT, RSST, V, RSEQ, COND                                                      ! Affichage à l'écran les données interessantes

        deallocate(MTRX, IPVT, WORK, SYST, B)                                                                  ! Désallocation de tous les vecteurs/matrices
    end do
end program KIRCHHOFF
