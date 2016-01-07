!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME : EXPONENTIELLE.F90
!! DATE      : 30/09/2015
!! AUTEUR    : ANTHONY HENDRICKX
!! VARIABLES : R    : ENTIER : CONTIENT LA PR�CISION, D�FINI DANS LE MODULE KINDS
!!             K    : ENTIER : INDEX VARIANT DE KMIN A KMAX POUR LE CALCUL DE L'EXPONENTIELLE
!!             KMIN : ENTIER : LIMITE INF�RIEURE DE K, LUE DANS LE FICHIER D'ENTR�E
!!             KMAX : ENTIER : LIMITE SUP�RIEURE DE K, LUE DANS LE FICHIER D'ENTR�E
!!             IDX1 : ENTIER : INDEX UTILIS� DANS LES BOUCLES
!!             IDX2 : ENTIER : INDEX UTILIS� DANS LES BOUCLES
!!             EXPN : R�EL   : CONTIENT LA VALEUR APPROXIM�E DE L'EXPONENTIELLE
!!             FACT : R�EL   : CONTIENT LE FACTEUR 1+1/N


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE KINDS
module KINDS
  integer, parameter :: R = 4
end module


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program EXPONENTIELLE
  use KINDS
  implicit none

  integer(kind = R) :: K, KMIN, KMAX, IDX1 !, IDX2                                                      ! IDX2 doit etre d�comment� pour utiliser les boucles imbriqu�es
  real(kind = R)    :: EXPN, FACT

10  format(I2, 4E13.6)                                                                                  ! Je d�clare le format utiliser pour �crire dans les fichiers de sortie

    open(100, FILE = 'INPUT/DONNEES.dat')                                                               ! J'ouvre le fichier d'entr�e
        read(100, *) KMIN, KMAX                                                                         ! Je lit les valeurs de KMIN et KMAX dans le fichier
    close(100)

    open(120, FILE = "SORTIE_"//achar(R+48)//".dat")                                                    ! J'ouvre un fichier (achar(R+48) permet de transformer R (nombre) en caract�re

!   call system("CLS & @ECHO %TIME%")                                                                   ! J'efface la console et j'affiche l'heure

    do K = KMIN, KMAX
        FACT = 1._R + 2._R**real(-K,R)                                                                  ! Je calcule le facteur 1+1/N pour chaque K
        EXPN = 1._R                                                                                     ! Je reinitialise l'exponentielle
        do IDX1 = 1,2**K
            EXPN = EXPN*FACT                                                                            ! Je multiplie N fois le facteur 1+1/N par lui-meme
        end do
        write(120,10) K, 2._R**real(K,R), FACT, EXPN, abs(EXPN-exp(1._R))                               ! J'�cris ce qui est demand� (K, N, 1 + 1/N, (1+1/N)^N et l'erreur relative)
    end do

    close(120)
!   call system("@ECHO %TIME%")                                                                         ! J'affiche le temps d'execution
    call system("TYPE SORTIE_"//achar(R+48)//".dat & PAUSE")                                            ! J'affiche le contenu du fichier dans la console et je mets en pause
!   write (*,*) char(7)                                                                                 ! Je demande � l'ordinateur d'�mettre un bip sonore pour pr�venir de la fin


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! POUR ALLER PLUS LOIN : Boucles imbriqu�es                                                            ! Meme principe que juste avant mais avec des boucles imbriqu�es

!   open(120, file = "SORTIE_"//achar(R+48)//"_2.dat")
!   do K = KMIN, KMAX
!       FACT = 1._R + 2._R**real(-K,R)
!       EXPN = 1._R
!       if(K.GT.30) then
!           do IDX1 = 1,2**30
!               do IDX2 = 1, 2**(K-30)
!                   EXPN = EXPN*FACT
!               end do
!           end do
!       else
!           do IDX1 = 1,2**K
!               EXPN = EXPN*FACT
!           end do
!       end if
!       write(120,10) K, 2._R**real(K,R), FACT, EXPN, abs(EXPN-exp(1._R))
!   end do
!   close(120)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! POUR ALLER PLUS LOIN : Base 3                                                                        ! M�me principe que juste avant mais avec une base de 3 au lieu de 2

!   open(120, file = "SORTIE_"//achar(R+48)//"_3.dat")
!   do K = KMIN, KMAX
!       FACT = 1._R + 3._R**real(-K,R)
!       EXPN = 1._R
!       if(K.GT.18) then
!           do IDX1 = 1,3**18
!               do IDX2 = 1,3**(K-18)
!                   EXPN = EXPN*FACT
!               end do
!           end do
!       else
!           do IDX1 = 1,3**K
!               EXPN = EXPN*FACT
!           end do
!       end if
!       write(120,10) K, 3._R**real(K,R), FACT, EXPN, abs(EXPN-exp(1._R))
!   end do

end program EXPONENTIELLE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME ALTERNATIF (ON SE PASSE DE LA BOUCLE, DONC ON OBSERVE PAS LES M�ME ERREURS)
!program EXPONENTIELLE
!  use KINDS
!  implicit none
!  integer(kind = R)           :: INDX, KMIN, KMAX, BASE = 2_R
!  real(kind = R), allocatable :: DATA(:,:)
!
!    open(100, file = 'INPUT/DONNEES.dat')
!        read(100, *) KMIN, KMAX
!    close(100)
!
!    allocate(DATA(5,KMIN:KMAX))
!
!    DATA(2,:) = real((/(INDX, INDX = KMIN, KMAX)/), R)
!    DATA(1,:) = real(BASE,R)**DATA(2,:)
!    DATA(3,:) = (1._R + (1._R/DATA(1,:)))
!    DATA(4,:) = DATA(3,:)**INT(DATA(1,:),R)
!    DATA(5,:) = abs(DATA(4,:) - exp(1._R))
!
!    open(120, file = "SORTIE_"//achar(R+48)//".dat")
!
!    write(120,"(5E13.6)") DATA
!    write(*,  "(5E13.6)") DATA
!    write(121,"(2E13.6)") DATA([1,5],:)
!
!end program EXPONENTIELLE
