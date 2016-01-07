!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE KINDS
module KINDS
  implicit none
  integer, parameter :: k = 4                                                                                  ! D√©finition de la pr√©cision
end module KINDS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program WAVE2D
  use KINDS                                                                                                    ! Module de pr√©cision
  use MAPPACK                                                                                                 ! Module d'affichage graphique
  implicit none
  
  real(kind = k) :: PASX = 1., PASY = 1., PAST, VTSS = 10., Lx = 100., Ly = 100., FREQ = 2., pi = 3.1415926536 
  real(kind = k), allocatable :: POSX(:), POSY(:), POSZ(:,:,:)
  integer        :: Nx = 140, Ny = 140, IDXX, IDXY, INDX, LIMT = 1000
  
    allocate(POSX(0:Nx), POSY(0:Ny), POSZ(0:Nx, 0:Ny, 0:2))
    POSX = PASX*real((/(INDX, INDX = 0,Nx)/),k)
    POSY = PASY*real((/(INDX, INDX = 0,Ny)/),k)
    POSZ(:,:,0:2) = 0.
    POSZ(Nx/3,Ny/2,0) = 1.

    PAST = sqrt(PASX**2.+PASY**2.)/(2.*VTSS)
  
    do IDXX = 1,Nx-1
        do IDXY = 1,Ny-1 
            POSZ(IDXX,IDXY,1) = POSZ(IDXX,IDXY,0) &
                              + (VTSS*PAST/PASX)**2./2.*(POSZ(IDXX-1,IDXY,0)-2*POSZ(IDXX,IDXY,0)+POSZ(IDXX+1,IDXY,0)) &
                              + (VTSS*PAST/PASY)**2./2.*(POSZ(IDXX,IDXY-1,0)-2*POSZ(IDXX,IDXY,0)+POSZ(IDXX,IDXY+1,0))
        end do
    end do
    POSZ(Nx/3,Ny/2,1) = POSZ(Nx/3,Ny/2,1) + sin(2*pi*FREQ*PAST)
    call Map(POSX, POSY, reshape(POSZ(:,:,1),[Nx*Ny]), keep=.true., refresh=.false.)
  
    do INDX = 1,1000
        do IDXX = 1,Nx-1
            do IDXY = 1,Ny-1 
                POSZ(IDXX,IDXY,2) = 2*POSZ(IDXX,IDXY,1)-POSZ(IDXX,IDXY,0) &
                                  + (VTSS*PAST/PASX)**2.*(POSZ(IDXX-1,IDXY,1)-2*POSZ(IDXX,IDXY,1)+POSZ(IDXX+1,IDXY,1)) &
                                  + (VTSS*PAST/PASY)**2.*(POSZ(IDXX,IDXY-1,1)-2*POSZ(IDXX,IDXY,1)+POSZ(IDXX,IDXY+1,1))
            end do
        end do
        POSZ(:,:,:1) = POSZ(:,:,1:)
        POSZ(Nx/3,Ny/2,1) = POSZ(Nx/3,Ny/2,1) + sin(2*pi*FREQ*PAST)
        call Map(POSX, POSY, reshape(POSZ(:,:,1),[Nx*Ny]), keep=.true., refresh=.true.)
    end do
	
! 05/01 : anglais (09:30 ‡ 10h30)
! 06/01 : particule quantiques
! 08/01 : electronique
! 11/01 : physique du solide
! 13/01 : Capteur
! 18/01 : Approche numÈrique
	

end program
