! RUN: %dragonegg -S %s -O3 -fplugin-arg-dragonegg-enable-gcc-optzns
! PR12610

module solv_cap
  integer, parameter, public :: dp = selected_real_kind(5)
  real(kind=dp), private :: D1, D2
  integer,       private, save :: Ng1=0, Ng2=0
  integer,       private, pointer,     dimension(:,:)  :: Grid
contains
  subroutine Preco(X)
    real(kind=dp), intent(in out), dimension(0:,0:)  :: X
    complex(kind=dp), allocatable, dimension(:,:)    :: t
    real(kind=dp)             :: K0, D
    integer                   :: i, j, is, js
    allocate( t(0:Ng1-1,0:Ng2-1) )
    t = X
    call Fourir2D(t, 1)
    K0 = 0.15_dp
    do j=0,Ng2-1
      js = min(j, Ng2-j)
      do i=0,Ng1-1
        is = min(i, Ng1-i)
        D = sqrt( (K0+is**2)/(D1*Ng1)**2 + (K0+js**2)/(D2*Ng2)**2 )
        t(i,j) = t(i,j) * D
      end do
    end do
    call Fourir2D(t, -1)
    X = t(0:Ng1-1,0:Ng2-1)
    where (Grid==0)
      X = 0  
    end where
    deallocate( t )
    return
  end subroutine Preco
end
program capacitance
end
