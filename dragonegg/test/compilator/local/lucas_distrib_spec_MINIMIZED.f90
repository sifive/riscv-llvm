        program lucas_distrib
        real*8, allocatable :: a(:,:),wt(:)
        integer, parameter :: knowns(37)=(/2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,2281,3217,4253,4423 &
        ,9689,9941,11213,19937,21701,23209,44497,86243,110503,132049,216091,756839,859433,1257787,1398269,2976221,3021377/)
        character*16, external :: hex_res64
        if(ihi==p-1.and.maxval(abs(a))==0) then
          if(any(p==knowns))then
          endif
          write(*,'(i10,a18)')p,hex_res64(a,n,pad,p,nbits,bits0,base_index(0),p-1)
        endif
        end program lucas_distrib
        character*16 function hex_res64(a,n,pad,p,nbits,bits0,base_index_word,iter)
        OUTER: do i=7,0,-1
          do j=n/8-1,0,-1
            if(a(j,i)/=0) then
            endif
          enddo
        enddo OUTER
        do i=1,ndigit
        enddo
        RES: do i=1,ndigit-1
          if(nrem /= 0)then
          endif
        enddo RES
        end function hex_res64
        subroutine mers_mod_square(a,base,baseinv,base_index,wt,n,pad,ilo,ihi,p,ofile,diagnose)
        logical, save :: first_entry=.true.
        if(first_entry) then
          do i=1,n
          enddo
          do i=1,n,2
            if(j > i) then
            endif
            do
            enddo
          enddo
        endif                           
        if(ilo==1)then
          do i=0,7
          enddo
        endif
        do iter=ilo+1,ihi-1
        do i=0,7
        do j=0,n8-1
        enddo
        enddo
        if(temp > err_abs .and. frac > err_rel)then
        endif
        if(fracmax >= 0.4d0)then
          if(fracmax > 0.45 ) then
          endif
        endif
        do
        enddo
        enddo
        if(temp > err_abs .and. frac > err_rel)then
        endif
        if(fracmax >= 0.4d0)then
          if(fracmax > 0.45 ) then
          endif
        endif
        do
        enddo
        end subroutine mers_mod_square
        subroutine fft_square(b,a,index,n,pad,check1i)
        logical, save :: first_entry=.true.
        if(first_entry) then
          do
          enddo
        if(mod(n2bit,3)==0)then
        endif
          do
            do m=1,mm-1
            enddo
          enddo
          if(mod(n2bit,3)==1)then
            do m=1,n8-1
            enddo
          endif
        endif
            do i=ilo,ihi,16
            enddo
          do m=1,8
            do i=ilo,ihi,16
            enddo
          enddo
        do j=2,n4
            enddo
          do m=1,8
            do i=ilo,ihi,16
            enddo
          enddo
        if(n2==65536)then
          do m=1,32768
            do i=ilo,ihi,4
            enddo
          enddo
        endif
        if(n2==131072)then
          do m=1,32768
            do i=ilo,ihi,8
            enddo
          enddo
        endif
          do m=1,32768
            do i=ilo,ihi,16
            enddo
          enddo
        if(n2==524288)then
          do m=1,262144
            do i=ilo,ihi,4
            enddo
          enddo
        endif
          do m=1,262144
            do i=ilo,ihi,16
            enddo
          enddo
        end subroutine fft_square
