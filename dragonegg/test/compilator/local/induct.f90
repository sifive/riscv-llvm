module computer_time_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

implicit none

public :: computer_time

!
integer, parameter, private :: LONGreal = selected_real_kind(15,90)

contains


     subroutine computer_time (tnow)
!
!        purpose:     return the elapsed system time for this
!                     process on a sun microsystems computer.
!        coded:       f90 version coded 1 may 1993
!        author:      john k. prentice
!
!        input:    none
!
!        output:
!
!        tnow      real      elapsed system time (seconds)
!
!
!        external routines used: etime
!
      real (kind = LONGreal), intent(out) :: tnow
!
      logical, save :: first = .true.
      logical, save :: first_flip
      integer :: counted, count_rate, count_max
      real (kind = LONGreal) :: trate, tmax
      real (kind = LONGreal), save :: tfirst
!
      call system_clock (counted, count_rate, count_max)
      if (counted < 0 .or. count_rate == 0) then
          tnow = 0.0_LONGreal
      else
          tnow = real(counted,LONGreal)
          trate = real(count_rate,LONGreal)
          tnow = tnow/trate
!
          if (first) then
              first = .false.
              tfirst = tnow
              first_flip = .true.
          else if (tnow < tfirst) then
              if (.not. first_flip) then
                  tmax = real(count_max,LONGreal)/trate
                  tfirst = tfirst - tmax
              else
                  tmax = real(count_max,LONGreal)/trate
                  tfirst = -(tmax - tfirst)
                  first_flip = .false.
              end if
          end if
!
          tnow   = tnow - tfirst
      end if
!
      end subroutine computer_time

end module computer_time_m

module define_type

integer, parameter, private :: longreal = selected_real_kind(15,90)

!
!=========== type definitions =============
!
      type, public :: vector
          real (kind = longreal) :: x, y, z
      end type vector
!
      type, public :: quad_inductor
          integer :: i1, i2
          type (vector) :: current_vector1, current_vector2, v1, v2, v3, v4
          real (kind = longreal) :: self_ind, thickness, resistance
      end type quad_inductor
!
      type, public :: coil
          real (kind = longreal) :: a, b, r, h, turns, theta, phi, psi, self_ind
          type (vector) :: origin
          logical :: circular, rectangular
      end type coil

end module define_type
module scc_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

implicit none

public :: self_ind_cir_coil

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal

contains

      subroutine self_ind_cir_coil (r, l, turns, mu, self_l)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        19 september 1997
!
!      purpose:      compute the self inductance of a circular coil.  the self
!                    inductance is defined as:
!
!                          l = mu/(4 pi i*i) int dv1 [int dv2 (j1 * j2)/r]
!
!                    where
!                          l   = self inductance of coil
!                          mu  = permeability of the medium
!                          i   = current in coil
!                          j1  = current density in coil
!                          j2  = current density in coil
!
!                    int dv1 and int dv2 denote volume integrals over the coil.
!                    we assume the coil is made of very thin wire.
!
!                    this routine uses the equation for the self-inductance that is
!                    in "static and dynamic electricity" by w. r. smythe, third edition,
!                    mcgraw-hill book company, 1968, page 346.
!
!############################################################################################
!
!      input:
!
!        r                       [real, selected_real_kind(15,90)]
!                                radius of coil
!
!        l                       [real, selected_real_kind(15,90)]
!                                length of coil
!
!        turns                   [real, selected_real_kind(15,90)]
!                                turns per unit length of coil
!
!        mu                      [real, selected_real_kind(15,90)]
!                                permeability of the medium
!
!     output:
!
!        self_l                  [real, selected_real_kind(15,90)]
!                                self-inductance of the coil.
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = longreal), intent(in) :: r, l, turns, mu
      real (kind = longreal), intent(out) :: self_l
!
!========== internal variables ============
!
!
      real (kind = longreal) :: alpha, modulus, pk, ak, bk, ae, be, elliptice, elliptick
!
!        evaluate the complete elliptic integrals of modulus sin(alpha).  we use a polynomial
!        approximation given by hastings.  this approximation is from "computation of
!        special functions" by shanjie zhang and jianming jin, john wiley and sons, inc,
!        1996, page 661ff.
!
      alpha = atan(2.0_longreal*r/l)
      modulus = sin(alpha)
!
      pk = 1.0_longreal - modulus**2
      ak = (((0.01451196212_longreal*pk+0.03742563713_longreal)*pk+                         &
            0.03590092383_longreal)*pk+0.09666344259_longreal)*pk+1.38629436112_longreal
      bk = (((0.00441787012_longreal*pk+0.03328355346_longreal)*pk+                         &
            0.06880248576_longreal)*pk+0.12498593597_longreal)*pk+0.5_longreal
      elliptick = ak - bk * log(pk)
!
      ae = (((0.01736506451_longreal*pk+0.04757383546_longreal)*pk+                         &
            0.0626060122_longreal)*pk+0.44325141463_longreal)*pk+1.0_longreal
      be = (((0.00526449639_longreal*pk+0.04069697526_longreal)*pk+                         &
            0.09200180037_longreal)*pk+0.2499836831_longreal)*pk
      elliptice = ae - be * log(pk)
!
!        evaluate self-inductance
!
      self_l = (mu * turns**2 * l**2 * 2.0_longreal * r)/3.0_longreal *                     &
               (((tan(alpha)**2-1.0_longreal)*elliptice+elliptick)/sin(alpha) -             &
               tan(alpha)**2)
!
      end subroutine self_ind_cir_coil

end module scc_m
module src_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

implicit none

public :: self_ind_rec_coil
private :: i_star, i_star_star

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal

contains

      subroutine self_ind_rec_coil (a, b, h, turns, mu, self_l)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        29 september 1997
!
!      purpose:      compute the self inductance of a square coil.  the self
!                    inductance is defined as:
!
!                          l = mu/(4 pi i*i) int dv1 [int dv2 (j1 * j2)/r]
!
!                    where
!                          l   = self inductance of coil
!                          mu  = permeability of the medium
!                          i   = current in coil
!                          j1  = current density in coil
!                          j2  = current density in coil
!
!                    int dv1 and int dv2 denote volume integrals over the coil.
!                    we assume the coil is made of very thin wire.
!
!                    this routine implements an analytical evaluation for the
!                    self-inductance derived by john prentice.
!
!############################################################################################
!
!      input:
!
!        a                       [real, selected_real_kind(15,90)]
!                                first dimension of the rectanglar coil
!
!        b                       [real, selected_real_kind(15,90)]
!                                second dimension of the rectangular coil
!
!        h                       [real, selected_real_kind(15,90)]
!                                length of coil
!
!        turns                   [real, selected_real_kind(15,90)]
!                                turns per unit length of coil.
!
!        mu                      [real, selected_real_kind(15,90)]
!                                permeability of the medium
!
!     output:
!
!        self_l                  [real, selected_real_kind(15,90)]
!                                self-inductance of the coil.
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = longreal), intent(in) :: a, b, h, turns, mu
      real (kind = longreal), intent(out) :: self_l
!
!========== internal variables ============
!
!
      real (kind = longreal) :: i1, i2, i3, i4
!
!        evaluate integral i1
!
!           i1 = int(dz) int(dz') int(dx) int(dx') 1/sqrt((x-x')^2 + (z-z')^2)
!
!        where the z and z' integrals go from 0 to h and the x and x' integrals go
!        from -a/2 to a/2.
!
      i1 = i_star_star (h, a)
!
!        evaluate integral i2
!
!           i2 = int(dz) int(dz') int(dx) int(dx') 1/sqrt((x-x')^2 + (z-z')^2 + b^2)
!
!        where the z and z' integrals go from 0 to h and the x and x' integrals go
!        from -a/2 to a/2 and b>0.
!
      i2 = i_star (h, a, b)
!
!        evaluate integral i3
!
!           i3 = int(dz) int(dz') int(dy) int(dy') 1/sqrt((y-y')^2 + (z-z')^2)
!
!        where the z and z' integrals go from 0 to h and the y and y' integrals go
!        from -b/2 to b/2.
!
      i3 = i_star_star (h, b)
!
!        evaluate integral i4
!
!           i4 = int(dz) int(dz') int(dy) int(dy') 1/sqrt((y-y')^2 + (z-z')^2 + a^2)
!
!        where the z and z' integrals go from 0 to h and the y and y' integrals go
!        from -b/2 to b/2 and a>0.
!
      i4 = i_star (h, b, a)
!
!        evaluate the self-inductance
!
      self_l = mu * turns**2 / (2.0_longreal * pi) * (i1 - i2 + i3 - i4)
!
      end subroutine self_ind_rec_coil



      function i_star (h, a, b) result (integral)
!
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        29 september 1997
!
!      purpose:      analytically evaluate the integral of the form:
!
!                    int(dz) int(dz') int(dx) int(dx') 1/sqrt((x-x')^2 + (z-z')^2 + b^2)
!
!                    where the z and z' integrals go from 0 to h and the x and x'
!                    integrals go from -a/2 to a/2.  b is always non-zero.
!
!############################################################################################
!
!      input:
!
!        h                       [real, selected_real_kind(15,90)]
!                                limit of integration for z and z' integrals
!
!        a                       [real, selected_real_kind(15,90)]
!                                limit of integration for the x and x' integrals
!
!        b                       [real, selected_real_kind(15,90)]
!                                non-zero constant in denominator of integration kernel
!
!     output:
!
!        integral                [real, selected_real_kind(15,90)]
!                                value of the integral.
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = longreal), intent(in) :: h, a, b
      real (kind = longreal) :: integral
!
!========== internal variables ============
!
      real (kind = longreal) :: i1_prime, i2_prime, i3_prime, i4_prime, factor1, factor2,   &
                                factor3
!
!        evaluate integral i1'
!
!           i1' = 4ah int(d rho) int (d lambda) 1/sqrt(rho^2 + lambda^2 + b^2)
!
!        where the rho integration is from 0 to h and the lambda integration is
!        from 0 to a.
!
      factor1 = sqrt(a**2 + b**2 + h**2)
      factor2 = sqrt(b**2 + h**2)
      factor3 = sqrt(a**2 + b**2)
      i1_prime = 4.0_longreal * a * h**2 * (log((a+factor1)/factor2) +                      &
                     (b/h)*atan((a*b)/(h*factor1))) + 4.0_longreal * a**2 * h *             &
                     (log((h+factor1)/factor3) + (b/a)*atan((h*b)/(a*factor1))) -           &
                     2.0_longreal * pi * a * b * h
!
!
!        evaluate integral i2'
!
!           i2' = -4a int(d rho) int (d lambda) rho/sqrt(rho^2 + lambda^2 + b^2)
!
!        where the rho integration is from 0 to h and the lambda integration is
!        from 0 to a.
!
      factor1 = sqrt(a**2 + b**2 + h**2)
      factor2 = sqrt(b**2 + h**2)
      factor3 = sqrt(a**2 + b**2)
      i2_prime = -2.0_longreal * a * (h**2 * log((a+factor1)/factor2) + b**2 *              &
                       log((b*(a+factor1))/(factor2*(a+factor3))) + a * (factor1 - factor3))
!
!
!        evaluate integral i3'
!
!           i3' = -4h int(d rho) int (d lambda) lambda/sqrt(rho^2 + lambda^2 + b^2)
!
!        where the rho integration is from 0 to h and the lambda integration is
!        from 0 to a.
!
      factor1 = sqrt(a**2 + b**2 + h**2)
      factor2 = sqrt(a**2 + b**2)
      factor3 = sqrt(b**2 + h**2)
      i3_prime = -2.0_longreal * h * (a**2 * log((h+factor1)/factor2) + b**2 *              &
                       log((b*(h+factor1))/(factor2*(h+factor3))) + h * (factor1 - factor3))
!
!
!        evaluate integral i4'
!
!           i4' = 4 int(d rho) int (d lambda) (rho * lambda)/sqrt(rho^2 + lambda^2 + b^2)
!
!        where the rho integration is from 0 to h and the lambda integration is
!        from 0 to a.
!
      factor1 = a**2 + b**2 + h**2
      factor2 = a**2 + b**2
      factor3 = b**2 + h**2
      i4_prime = 4.0_longreal/3.0_longreal * (factor1**1.5_longreal - factor2**1.5_longreal &
                                                             - factor3**1.5_longreal + b**3)
!
!        evaluate the final integral
!
      integral = i1_prime + i2_prime + i3_prime + i4_prime
!
      end function i_star


      function i_star_star (h, a) result (integral)
!
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        29 september 1997
!
!      purpose:      analytically evaluate the integral of the form:
!
!                    int(dz) int(dz') int(dx) int(dx') 1/sqrt((x-x')^2 + (z-z')^2)
!
!                    where the z and z' integrals go from 0 to h and the x and x'
!                    integrals go from -a/2 to a/2.
!
!############################################################################################
!
!      input:
!
!        h                       [real, selected_real_kind(15,90)]
!                                limit of integration for z and z' integrals
!
!        a                       [real, selected_real_kind(15,90)]
!                                limit of integration for the x and x' integrals
!
!     output:
!
!        integral                [real, selected_real_kind(15,90)]
!                                value of the integral.
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = longreal), intent(in) :: h, a
      real (kind = longreal) :: integral
!
!========== internal variables ============
!
      real (kind = longreal) :: i1_prime_prime, i2_prime_prime, i3_prime_prime,             &
                                i4_prime_prime, factor
!
      factor = sqrt(a**2 + h**2)
!
!        evaluate integral i1''
!
!           i1'' = 4ah int(d rho) int (d lambda) 1/sqrt(rho^2 + lambda^2)
!
!        where the rho integration is from 0 to h and the lambda integration is
!        from 0 to a.
!
      i1_prime_prime = 4.0_longreal * a * h**2 * log((a+factor)/h) + 4.0_longreal *         &
                                                                a**2 * h * log((h+factor)/a)
!
!
!        evaluate integral i2''
!
!           i2'' = -4a int(d rho) int (d lambda) rho/sqrt(rho^2 + lambda^2)
!
!        where the rho integration is from 0 to h and the lambda integration is
!        from 0 to a.
!
      i2_prime_prime = -2.0_longreal * a * (h**2 * log((a+factor)/h) + a*(factor - a))
!
!
!        evaluate integral i3''
!
!           i3'' = -4h int(d rho) int (d lambda) lambda/sqrt(rho^2 + lambda^2)
!
!        where the rho integration is from 0 to h and the lambda integration is
!        from 0 to a.
!
      i3_prime_prime = -2.0_longreal * h * (a**2 * log((h+factor)/a) + h*(factor - h))

!
!
!        evaluate integral i4''
!
!           i4'' = 4 int(d rho) int (d lambda) (rho * lambda)/sqrt(rho^2 + lambda^2)
!
!        where the rho integration is from 0 to h and the lambda integration is
!        from 0 to a.
!
      i4_prime_prime = 4.0_longreal/3.0_longreal * (factor**3 - a**3 - h**3)
!
!        evaluate the final integral
!
      integral = i1_prime_prime + i2_prime_prime + i3_prime_prime + i4_prime_prime
!
      end function i_star_star

end module src_m
MODULE free_input

implicit none

private
public :: next, value, convert_lower_case, check_eof, check_number

integer, parameter, public :: nin = 10
character (len=80), public :: card, field, lfield
integer, public ::   icpnt, free_format_error_flag
logical, public ::   eoff
integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = LONGreal), public :: real_variable

CONTAINS

     subroutine next
!
      implicit none
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: icend = 80
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, k, iend, istart, kmin, nodel
      logical :: first
      character, dimension(3) :: delim

      save first
!-----------------------------------------------
      data first/.TRUE./
      data nodel, delim/3, ' ', ',', '='/
!
!        on first pass, initialize icpnt pointer
!
      if (first) then
          icpnt = icend + 1
          first = .FALSE.
      endif
!
!        if icpnt>icend, read the next record off unit 2 into
!        the string 'card'.  next verify that this is a non-
!        blank card.  if it is blank or an input comment card
!        (asterisk in column 1), skip it and get the next record
!
   10 continue
      eoff = .FALSE.
      if (icpnt > icend) then
          read (nin, '(a)', end=50) card
          if (card(1:icend)==' ' .or. card(1:1)=='*') go to 10
          icpnt = 1
      endif
!
!        get the next sub-string.  we do this as follows.  we
!        look for the next delimeter.  if it is as the same
!        position as the current pointer, we advance the pointer
!        and try again.  if not, then the pointer is at the beginning
!        of a sub-string and the delimeter is trailing this sub-string.
!        note that we look for all the delimeters possible before
!        taking any action.
!
      do i = icpnt, icend
          istart = i
          kmin = 0
          do j = 1, nodel
              k = index(card(i:icend),delim(j))
!
!        index returns positions relative the beginning of the
!        sub-string.  hence we add in the appropiate off-set to
!        give the index relative to the beginning of the string
!        card, not just the sub-string card(i:icend).
!
              if (k /= 0) then
                  k = k + i - 1
                  if (kmin == 0) then
                      kmin = k
                  else
                      kmin = min(k,kmin)
                  endif
              endif
          end do
!
!        if kmin is not equal to the current pointer position, then
!        it must be pointing at the trailing delimeter of a valid
!        sub-string.
!
          if (kmin /= i) then
              if (kmin > 0) then
                  iend = kmin - 1
                  go to 40
              endif
!
!        if we fall through, there was no delimeter
!        found on the remainder of this record.  this means
!        the entire remainder of this record is a valid sub-string
!
              iend = icend
              go to 40
          endif
      end do
!
!        if we fall through this loop, there were no more non-
!        delimeters on this record.  go get next record
!
      icpnt = icend + 1
      go to 10
!
!        put the sub-string into the string 'field'.  note
!        that fortran 77 pads the string with blanks
!
   40 continue
      field = card(istart:iend)
      icpnt = iend + 2
      return 
!
!        end of file encountered, set flag and return
!
   50 continue
      eoff = .TRUE.
      icpnt = icend + 1
      return 
      end subroutine next

      subroutine value(result, itype)
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
!
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer itype
      real (kind = LONGreal) :: result
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!-----------------------------------------------
!
!        get next field off unit 2
!
      call next
      if (eoff) then
          itype = -1
      else
!
!        read field as a numeric
!
          read (field, fmt='(bn,f20.0)', err=10) result
          itype = 1
          go to 20
!
!        the only possibility left is that this was an alphanumeric
!
   10     continue
          itype = 0
   20     continue
      endif
!
      end subroutine value

      function convert_lower_case (input_string) result (output_string)
!
      character (len=1) :: input_string, output_string
      integer :: collating_difference
!
      if (ichar(input_string) >= ichar('A') .AND. ichar(input_string) <= ichar('Z')) then
          collating_difference = ichar(input_string) - ichar('A')
          output_string = char(ichar('a') + collating_difference)
      else
          output_string = input_string
      end if
!
      end function convert_lower_case

      subroutine check_eof 
!
      if (free_format_error_flag == (-1)) then
          print *," "
          print *,"Abort.  Unexpected end of file while reading input. " 
          print *,"Was reading the line:"
          print *,card
          print *," "
          stop
      end if
!
      end subroutine check_eof
!
      subroutine check_number
!
      if (free_format_error_flag == 0) then
          print *," "
          print *,"Abort.  Expected a number on input and instead encountered the word: "
          print *,field
          print *," "
          print *,"was reading the line:"
          print *,card
          print *," "
          stop
      end if
!
      end subroutine check_number



END MODULE free_input
module mcc_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

implicit none

public :: mutual_ind_cir_cir_coils

!
integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal

contains

      subroutine mutual_ind_cir_cir_coils (r1, r2, x12, y12, z12, l1, l2, turns1, turns2,  &
                                            mu, rotate, m, l12)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        6 september 1997
!
!      purpose:      compute the mutual inductance of two coupled circular coils.  the
!                    coils can be oriented arbitrarily in space.  the mutual inductance
!                    is defined as:
!
!                          l12 = mu/(4 pi i1 i2) int dv1 [int dv2 (j1 * j2)/r]
!
!                    where
!                          l12 = mutual inductance between coils 1 and 2
!                          mu  = permeability of the medium
!                          i1  = current in coil 1
!                          i2  = current in coil 2
!                          j1  = current density in coil 1
!                          j2  = current density in coil 2
!
!                    int dv1 and int dv2 denote volume integrals over coil 1 and 2,
!                    respectively.  we assume the coils are made of very thin wires,
!                    which allows us to reduce the integral to:
!
!                          l12 = (mu turns1 turn2 r1 r2)/(4 pi) * int dz1 [int dz2
!                                   [int dtheta1 [int dtheta2 cos(theta_1 - theta_2) /
!                                       sqrt((x_1 - x_2 - x12)**2 + (y_1 - y_2 - y12)**2 +
!                                           (z_1 - z_2 - z12)**2)
!
!                    where int dz1 is from -l1/2 to l1/2, int dz2 is from -l2/2 to l2/2,
!                    int dtheta1 is from 0 to 2 pi and int dtheta2 is from 0 to 2 pi.
!
!                    this routine calculates these integrals numerically.  the dtheta1 and
!                    dtheta2 integrals are done using an m point euler integration (see
!                    25.4.60 on page 891 of "handbook of mathematical functions" by
!                    m. abramowitz and i. stegun, dover publications, 1972).  the
!                    dz1 dz2 integration is done using a 9 point, 6th order gaussian
!                    quadrature in the plane (see 25.4.62 on page 892 of "handbook of
!                    mathematical functions" by m. abramowitz and i. stegun, dover
!                    publications, 1972).
!
!############################################################################################
!
!      input:
!
!        r1                      [real, selected_real_kind(15,90)]
!                                radius of coil 1
!
!        r2                      [real, selected_real_kind(15,90)]
!                                radius of coil 2
!
!        x12                     [real, selected_real_kind(15,90)]
!                                x coordinate of the center of coil 2 relative to coil 1
!
!        y12                     [real, selected_real_kind(15,90)]
!                                y coordinate of the center of coil 2 relative to coil 1
!
!        z12                     [real, selected_real_kind(15,90)]
!                                z coordinate of the center of coil 2 relative to coil 1
!
!        l1                      [real, selected_real_kind(15,90)]
!                                length of coil 1
!
!        l2                      [real, selected_real_kind(15,90)]
!                                length of coil 2
!
!        turns1                  [real, selected_real_kind(15,90)]
!                                turns per unit length of coil 1
!
!        turns2                  [real, selected_real_kind(15,90)]
!                                turns per unit length of coil 2
!
!        mu                      [real, selected_real_kind(15,90)]
!                                permeability of the medium
!
!        rotate                  [real, selected_real_kind(15,90), dimension(3,3)]
!                                rotation matrix that maps the coordinate system for coil 2
!                                into the coordinate system of coil 1.
!
!        m                       [integer]
!                                number of quadrature points for theta integrals
!
!     output:
!
!        l12                     [real, selected_real_kind(15,90)]
!                                mutual inductance between the coils.
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = longreal), intent(in) :: r1, r2, x12, y12, z12, l1, l2, turns1, turns2,  &
                                            mu
      real (kind = longreal), dimension(:,:), intent(in) :: rotate
      integer, intent(in) :: m
      real (kind = longreal), intent(out) :: l12
!
!========== internal variables ============
!
!
      integer :: n1, n2, i
      real (kind = longreal) :: coefficient, theta_1, theta_2, x_1, x_2, y_1, y_2, z_1,     &
                                z_2, theta_l12, x_2rot, y_2rot, z_2rot, tvx, tvy,           &
                                numerator, denominator
      real (kind = longreal), dimension(1:9), save :: z1g, z2g, zw
      logical, save :: first = .true.


!
!        on the first call to this routine, initialize the weights for a 9 point 6th order
!        gaussian quadrature on a square.

      if (first) then
          z1g(1) = 0.0_longreal
          z2g(1) = 0.0_longreal
          zw(1) = 16.0_longreal/81.0_longreal
          z1g(2) = -sqrt(0.6_longreal)
          z2g(2) = -sqrt(0.6_longreal)
          zw(2) = 25.0_longreal/324.0_longreal
          z1g(3) = -sqrt(0.6_longreal)
          z2g(3) = sqrt(0.6_longreal)
          zw(3) = 25.0_longreal/324.0_longreal
          z1g(4) = sqrt(0.6_longreal)
          z2g(4) = -sqrt(0.6_longreal)
          zw(4) = 25.0_longreal/324.0_longreal
          z1g(5) = sqrt(0.6_longreal)
          z2g(5) = sqrt(0.6_longreal)
          zw(5) = 25.0_longreal/324.0_longreal
          z1g(6) = 0.0_longreal
          z2g(6) = -sqrt(0.6_longreal)
          zw(6) = 10.0_longreal/81.0_longreal
          z1g(7) = 0.0_longreal
          z2g(7) = sqrt(0.6_longreal)
          zw(7) = 10.0_longreal/81.0_longreal
          z1g(8) = -sqrt(0.6_longreal)
          z2g(8) = 0.0_longreal
          zw(8) = 10.0_longreal/81.0_longreal
          z1g(9) = sqrt(0.6_longreal)
          z2g(9) = 0.0_longreal
          zw(9) = 10.0_longreal/81.0_longreal
          first = .false.
      end if
!
      l12 = 0.0_longreal
!
!        outer loop (gauss) is the gaussian quadrature from -11/2 to l1/2 and from
!        -l2/2 to l2/2.  the next most inner loop (theta1) does a line integral from
!        0 to 2 pi.  the inner most integral (theta2) does the other line integral from
!        0 t0 2 pi.
!
gauss:do i = 1, 9
          theta_l12 = 0.0_longreal
          z_1 = 0.5 * l1 * z1g(i)
          z_2 = 0.5 * l2 * z2g(i)
theta1:   do n1 = 1, 2*m
              theta_1 = pi*real(n1,longreal)/real(m,longreal)
              x_1 = r1 * cos(theta_1)
              y_1 = r1 * sin(theta_1)
theta2:       do n2 = 1, 2*m
                  theta_2 = pi*real(n2,longreal)/real(m,longreal)
                  x_2 = r2 * cos(theta_2)
                  y_2 = r2 * sin(theta_2)
!
!        rotate coordinates of the point on coil 2 into the coordinate system
!        for coil 1
!
                  x_2rot = rotate(1,1)*x_2 + rotate(1,2)*y_2 + rotate(1,3)*z_2
                  y_2rot = rotate(2,1)*x_2 + rotate(2,2)*y_2 + rotate(2,3)*z_2
                  z_2rot = rotate(3,1)*x_2 + rotate(3,2)*y_2 + rotate(3,3)*z_2
!
!        compute the theta unit vector for the point on coil 2 in the coordinate
!        system of coil 1
!
                  tvx = -rotate(1,1)*sin(theta_2) + rotate(1,2)*cos(theta_2)
                  tvy = -rotate(2,1)*sin(theta_2) + rotate(2,2)*cos(theta_2)
!
                  numerator = -sin(theta_1)*tvx + cos(theta_1)*tvy
                  denominator = sqrt((x_1 - x_2rot - x12) **2 + (y_1 - y_2rot - y12)**2 +   &
                                                                     (z_1 - z_2rot - z12)**2)
                  theta_l12 = theta_l12 + numerator/denominator
              end do theta2
           end do theta1
           l12 = l12 + zw(i)*theta_l12
      end do gauss
!
!        multiply these integrals by the appropriate quadrature weights, lengths, and
!        physical constants
!
      coefficient = (mu * l1 * l2 * pi * r1 * r2 * turns1 * turns2)/(4.0_longreal *         &
                                                                         real(m,longreal)**2)
      l12 = coefficient * l12
!
      end subroutine mutual_ind_cir_cir_coils

end module mcc_m
module mcr_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

implicit none

public :: mutual_ind_cir_rec_coils

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal

contains

      subroutine mutual_ind_cir_rec_coils (radius, length, width, x12, y12, z12, l1, l2,    &
                                           turns1, turns2, mu, rotate1, rotate2, m, l12)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        6 september 1997
!
!      purpose:      compute the mutual inductance of two coupled coils.  one coil is
!                    circular and the other is rectangular.  the coils can be oriented
!                    arbitrarily in space.  the mutual inductance is defined as:
!
!                          l12 = mu/(4 pi i1 i2) int dv1 [int dv2 (j1 * j2)/r]
!
!                    where
!                          l12 = mutual inductance between coils 1 and 2
!                          mu  = permeability of the medium
!                          i1  = current in coil 1
!                          i2  = current in coil 2
!                          j1  = current density in coil 1
!                          j2  = current density in coil 2
!
!                    int dv1 and int dv2 denote volume integrals over coil 1 and 2,
!                    respectively.  we assume the coils are made of very thin wires,
!                    which allows us to reduce the integral to:
!
!                          l12 = (mu turns1 turn2 radius r2)/(4 pi) * int dz1 [int dz2
!                                   [int dtheta1 [int dtheta2 cos(theta_1 - theta_2) /
!                                       sqrt((x_1 - x_2 - x12)**2 + (y_1 - y_2 - y12)**2 +
!                                           (z_1 - z_2 - z12)**2)
!
!                    where int dz1 is from -l1/2 to l1/2, int dz2 is from -l2/2 to l2/2,
!                    int dtheta1 is from 0 to 2 pi and int dtheta2 is from 0 to 2 pi.
!
!                    this routine calculates these integrals numerically.  the dtheta1 and
!                    dtheta2 integrals are done using an m point euler integration (see
!                    25.4.60 on page 891 of "handbook of mathematical functions" by
!                    m. abramowitz and i. stegun, dover publications, 1972).  the
!                    dz1 dz2 integration is done using a 9 point, 6th order gaussian
!                    quadrature in the plane (see 25.4.62 on page 892 of "handbook of
!                    mathematical functions" by m. abramowitz and i. stegun, dover
!                    publications, 1972).
!
!############################################################################################
!
!      input:
!
!        radius                  [real, selected_real_kind(15,90)]
!                                radius of circular coil
!
!        length                  [real, selected_real_kind(15,90)]
!                                length of rectangular coil
!
!        width                   [real, selected_real_kind(15,90)]
!                                width of rectangular coil
!
!        x12                     [real, selected_real_kind(15,90)]
!                                x coordinate of the center of rectangular coil 2
!                                relative to circular coil
!
!        y12                     [real, selected_real_kind(15,90)]
!                                y coordinate of the center of rectangular coil 2
!                                relative to circular coil
!
!        z12                     [real, selected_real_kind(15,90)]
!                                z coordinate of the center of rectangular coil 2
!                                relative to circular coil
!
!        l1                      [real, selected_real_kind(15,90)]
!                                length of circular coil
!
!        l2                      [real, selected_real_kind(15,90)]
!                                length of rectangular coil
!
!        turns1                  [real, selected_real_kind(15,90)]
!                                turns per unit length of circular coil
!
!        turns2                  [real, selected_real_kind(15,90)]
!                                turns per unit length of circular coil
!
!        mu                      [real, selected_real_kind(15,90)]
!                                permeability of the medium
!
!        rotate                  [real, selected_real_kind(15,90), dimension(3,3)]
!                                rotation matrix that maps the coordinate system for the
!                                rectangular coil into the coordinate system of the
!                                circular coil.
!
!        m                       [integer]
!                                number of quadrature points for theta integrals
!
!     output:
!
!        l12                     [real, selected_real_kind(15,90)]
!                                mutual inductance between the coils.
!
!############################################################################################
!
!
!=========== formal variables =============
!
      real (kind = longreal), intent(in) :: radius, length, width, x12, y12, z12, l1, l2,   &
                                            turns1, turns2, mu
      real (kind = longreal), dimension(:,:), intent(in) :: rotate1, rotate2
      integer, intent(in) :: m
      real (kind = longreal), intent(out) :: l12
!
!========== internal variables ============
!
!
      integer :: n1, n2, i
      real (kind = longreal) :: coefficient, theta_1, theta_2, x_1, x_2, y_1, y_2, z_1,     &
                                z_2, theta_l12, x_2rot, y_2rot, z_2rot, tvx_1, tvy_1,       &
                                tvx_2, tvy_2, x_1rot, y_1rot, z_1rot, numerator,            &
                                denominator, s, ds, deltas
      real (kind = longreal), dimension(9), save :: z1g, z2g, zw
      logical, save :: first = .true.
!
!        on the first call to this routine, initialize the weights for a 9 point 6th order
!        gaussian quadrature on a square.

      if (first) then
          z1g(1) = 0.0_longreal
          z2g(1) = 0.0_longreal
          zw(1) = 16.0_longreal/81.0_longreal
          z1g(2) = -sqrt(0.6_longreal)
          z2g(2) = -sqrt(0.6_longreal)
          zw(2) = 25.0_longreal/324.0_longreal
          z1g(3) = -sqrt(0.6_longreal)
          z2g(3) = sqrt(0.6_longreal)
          zw(3) = 25.0_longreal/324.0_longreal
          z1g(4) = sqrt(0.6_longreal)
          z2g(4) = -sqrt(0.6_longreal)
          zw(4) = 25.0_longreal/324.0_longreal
          z1g(5) = sqrt(0.6_longreal)
          z2g(5) = sqrt(0.6_longreal)
          zw(5) = 25.0_longreal/324.0_longreal
          z1g(6) = 0.0_longreal
          z2g(6) = -sqrt(0.6_longreal)
          zw(6) = 10.0_longreal/81.0_longreal
          z1g(7) = 0.0_longreal
          z2g(7) = sqrt(0.6_longreal)
          zw(7) = 10.0_longreal/81.0_longreal
          z1g(8) = -sqrt(0.6_longreal)
          z2g(8) = 0.0_longreal
          zw(8) = 10.0_longreal/81.0_longreal
          z1g(9) = sqrt(0.6_longreal)
          z2g(9) = 0.0_longreal
          zw(9) = 10.0_longreal/81.0_longreal
          first = .false.
      end if
!
      l12 = 0.0_longreal
      ds = 2.0_longreal * (width + length) / real(2*m,longreal)
!
!        outer loop (gauss) is the gaussian quadrature from -11/2 to l1/2 and from
!        -l2/2 to l2/2.  the next most inner loop (theta1) does a line integral from
!        0 to 2 pi.  the inner most integral (theta2) does the other line integral from
!        0 t0 2 pi.
!
gauss:do i = 1, 9
          theta_l12 = 0.0_longreal
          z_1 = 0.5 * l1 * z1g(i)
          z_2 = 0.5 * l2 * z2g(i)
theta1:   do n1 = 1, 2*m
              theta_1 = pi*real(n1,longreal)/real(m,longreal)
              x_1 = radius * cos(theta_1)
              y_1 = radius * sin(theta_1)
theta2:       do n2 = 1, 2*m
                  s = real(n2,longreal) * ds
                  if (s <= 0.5_longreal * length) then
                      x_2 = 0.5_longreal * width
                      y_2 = s
                      theta_2 = atan(y_2/x_2)
                  else if (s <= 0.5_longreal * length + width) then
                      deltas = s - 0.5_longreal * length
                      x_2 = 0.5_longreal * width - deltas
                      y_2 = 0.5_longreal * length
                      theta_2 = 0.5_longreal * pi - atan(x_2/y_2)
                  else if (s <= 1.5_longreal * length + width) then
                      deltas = s - width - 0.5_longreal * length
                      x_2 = - 0.5_longreal * width
                      y_2 = 0.5_longreal * length - deltas
                      theta_2 = pi + atan(y_2/x_2)
                  else if (s < 2.0_longreal * width + 1.5_longreal * length) then
                      deltas = s - width - 1.5_longreal * length
                      x_2 = -0.5_longreal * width + deltas
                      y_2 = - 0.5_longreal * length
                      theta_2 = 1.5_longreal * pi - atan(x_2/y_2)
                  else
                      deltas = s - 2.0_longreal * width - 1.5_longreal * length
                      x_2 = 0.5_longreal * width
                      y_2 = -0.5_longreal * length + deltas
                      theta_2 = 2.0_longreal * pi + atan(y_2/x_2)
                  end if
!
!        rotate coordinates of the point on coil 1 into the can coordinate system
!
                  x_1rot = rotate1(1,1)*x_1 + rotate1(1,2)*y_1 + rotate1(1,3)*z_1
                  y_1rot = rotate1(2,1)*x_1 + rotate1(2,2)*y_1 + rotate1(2,3)*z_1
                  z_1rot = rotate1(3,1)*x_1 + rotate1(3,2)*y_1 + rotate1(3,3)*z_1
!
!        compute the theta unit vector for the point on coil 2 in the can coordinate system
!
                  tvx_1 = -rotate1(1,1)*sin(theta_1) + rotate1(1,2)*cos(theta_1)
                  tvy_1 = -rotate1(2,1)*sin(theta_1) + rotate1(2,2)*cos(theta_1)
!
!        rotate coordinates of the point on coil 2 into the can coordinate system
!
                  x_2rot = rotate2(1,1)*x_2 + rotate2(1,2)*y_2 + rotate2(1,3)*z_2
                  y_2rot = rotate2(2,1)*x_2 + rotate2(2,2)*y_2 + rotate2(2,3)*z_2
                  z_2rot = rotate2(3,1)*x_2 + rotate2(3,2)*y_2 + rotate2(3,3)*z_2
!
!        compute the theta unit vector for the point on coil 2 in the can coordinate system
!
                  tvx_2 = -rotate2(1,1)*sin(theta_2) + rotate2(1,2)*cos(theta_2)
                  tvy_2 = -rotate2(2,1)*sin(theta_2) + rotate2(2,2)*cos(theta_2)
!
                  numerator = tvx_1*tvx_2 + tvy_1*tvy_2
                  denominator = sqrt((x_1rot - x_2rot - x12) **2 +                         &
                                    (y_1rot - y_2rot - y12)**2 + (z_1rot - z_2rot - z12)**2)
                  theta_l12 = theta_l12 + numerator/denominator
              end do theta2
           end do theta1
           l12 = l12 + zw(i)*theta_l12
      end do gauss
!
!        multiply these integrals by the appropriate quadrature weights, lengths, and
!        physical constants
!
      coefficient = (mu * l1 * l2 * radius * (length + width) * turns1 * turns2)/           &
                                                         (4.0_longreal * real(m,longreal)**2)
      l12 = coefficient * l12
!
      end subroutine mutual_ind_cir_rec_coils

end module mcr_m
module misc_input_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use free_input

implicit none

private
public :: misc_input

integer, parameter, private :: longreal = selected_real_kind(15,90)

contains

      subroutine misc_input (qq_mutual_inductance_cutoff, wq_mutual_inductance_cutoff,      &
                             rq_mutual_inductance_cutoff, inductor_to_ground_capacitance,   &
                             theta_integral_quad_points, input_file)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        12 october 1997
!
!      purpose:      define the geometry of the res-q 3 coil
!
!############################################################################################
!
!=========== formal variables =============
!
      real (kind = LONGreal), intent(out) :: qq_mutual_inductance_cutoff,                   &
                                             wq_mutual_inductance_cutoff,                   &
&                                            rq_mutual_inductance_cutoff,                   &
                                             inductor_to_ground_capacitance
      integer, intent(out) :: theta_integral_quad_points
      character (len=80), intent(in) :: input_file
!
!========== internal variables ============
!
      integer :: n, wand_count, coil_count
      real (kind = LONGreal) :: real_value
!
      open (unit=nin,file=input_file,status="old",form="formatted",action="read")
      icpnt = 9999
!
!        set defaults
!
      qq_mutual_inductance_cutoff = 1.0e-10_LONGreal
      wq_mutual_inductance_cutoff = 1.0e-10_LONGreal
      rq_mutual_inductance_cutoff = 1.0e-10_LONGreal
      inductor_to_ground_capacitance = 0.0_LONGreal
      theta_integral_quad_points = 10
!
!
!        read through input until resq coil input is located
!
resq: do
          call next
          if (.NOT. eoff) then
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field /= 'misc_calculation_definitions:') cycle resq
!
              do 
                  call next
                  if (eoff) then
                      print *," "
                      print *,"unexpected end of file while reading resq coil ",        &
                                                                        "definition, abort."
                      print *," "
                      stop
                  end if
                  if (len(field) > 0) then
                      do n = 1, len(field)
                          field(n:n) = convert_lower_case(field(n:n))  
                      end do
                  end if
!
                  if (field == 'end_of_misc_calculation_definitions') then
                      exit resq
                  else if (field == 'quad_quad_mutual_inductance_cutoff') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      qq_mutual_inductance_cutoff = real_variable
                  else if (field == 'wand_quad_mutual_inductance_cutoff') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      wq_mutual_inductance_cutoff = real_variable
                  else if (field == 'resq_coil_quad_mutual_inductance_cutoff') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      rq_mutual_inductance_cutoff = real_variable
                  else if (field == 'inductor_to_ground_capacitance') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      inductor_to_ground_capacitance = real_variable
                  else if (field == 'theta_integral_quadrature_points') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      theta_integral_quad_points  = int(real_variable + 0.5_LONGreal)
                  else
                      print *," "
                      print *,"unrecognized word in misc input definition, abort."
                      print *," "
                      print *,"The unrecognized word was: ",field
                      print *," "
                      stop
                  end if
              end do
          end if
      end do resq
!
      close (unit = nin)
!
!        check input to catch obvious errors
!
      if (qq_mutual_inductance_cutoff < 0.0_LONGreal) then
          print *," "
          print *,"quad/quad inductance cutoff must be >= 0, abort while reading input."
          print *," "
          stop
      else if (qq_mutual_inductance_cutoff > 1.0_LONGreal) then
          print *," "
          print *,"quad/quad inductance cutoff must be <= 1, abort while reading input."
          print *," "
          stop
      else if (wq_mutual_inductance_cutoff < 0.0_LONGreal) then
          print *," "
          print *,"wand/quad inductance cutoff must be >= 0, abort while reading input."
          print *," "
          stop
      else if (wq_mutual_inductance_cutoff > 1.0_LONGreal) then
          print *," "
          print *,"wand/quad inductance cutoff must be <= 1, abort while reading input."
          print *," "
          stop
      else if (rq_mutual_inductance_cutoff < 0.0_LONGreal) then
          print *," "
          print *,"resq coil/quad inductance cutoff must be >= 0, abort while reading input."
          print *," "
          stop
      else if (rq_mutual_inductance_cutoff > 1.0_LONGreal) then
          print *," "
          print *,"resq coil/quad inductance cutoff must be <= 1, abort while reading input."
          print *," "
          stop
      else if (theta_integral_quad_points < 10) then
          print *," "
          print *,"need at least 10 theta integral quadrature points, abort while ",        &
                                                                             "reading input."
          print *," "
          stop
      end if
!
      end subroutine misc_input

end module misc_input_m
module mqc_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type

implicit none

private
public :: mutual_ind_quad_cir_coil

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal
real (kind = longreal), parameter, private :: small = 1.0e-10_longreal

contains

      subroutine mutual_ind_quad_cir_coil (r_coil, x_coil, y_coil, z_coil, h_coil, n_coil,  &
                                                      rotate_coil, rect_inductor, m, mu, l12)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        6 september 1997
!
!      purpose:      compute the mutual inductance of a rectangle and a circular coil.  the
!                    coil and rectangle can be oriented arbitrarily in space.
!
!############################################################################################
!
!      input:
!
!
!        m                       [integer]
!                                number of quadrature points for theta integrals
!
!     output:
!
!        l12                     [real, selected_real_kind(15,90)]
!                                mutual inductance .
!
!############################################################################################
!
!
!=========== formal variables =============
!
      type (quad_inductor), intent(in) :: rect_inductor
      real (kind = longreal), intent(in) :: r_coil, x_coil, y_coil, z_coil, h_coil, n_coil, &
                                            mu
      real (kind = longreal), dimension(:,:), intent(in) :: rotate_coil
      integer, intent(in) :: m
      real (kind = longreal), intent(out) :: l12
!
!========== internal variables ============
!
!
      real (kind = longreal), dimension(3,3) :: rotate_quad
      real (kind = longreal), dimension(9), save :: x2gauss, y2gauss, w2gauss, z1gauss,     &
                                                    w1gauss
      real (kind = longreal) :: xxvec, xyvec, xzvec, yxvec, yyvec, yzvec, zxvec, zyvec,     &
                                zzvec, magnitude, l12_lower, l12_upper, dx, dy, dz, theta,  &
                                a, b1, b2, numerator, denominator, coefficient, angle
      real (kind = longreal), dimension(3) :: c_vector, q_vector, rot_c_vector,             &
                                              rot_q_vector, current_vector,                 &
                                              coil_current_vec, coil_tmp_vector
      integer :: i, j, k
      logical, save :: first = .true.
!
!        on the first call to this routine, initialize the weights for a 9 point 6th order
!        gaussian quadrature on a square.

      if (first) then
          x2gauss(1) = 0.0_longreal
          y2gauss(1) = 0.0_longreal
          w2gauss(1) = 16.0_longreal/81.0_longreal
          x2gauss(2) = -sqrt(0.6_longreal)
          y2gauss(2) = -sqrt(0.6_longreal)
          w2gauss(2) = 25.0_longreal/324.0_longreal
          x2gauss(3) = -sqrt(0.6_longreal)
          y2gauss(3) = sqrt(0.6_longreal)
          w2gauss(3) = 25.0_longreal/324.0_longreal
          x2gauss(4) = sqrt(0.6_longreal)
          y2gauss(4) = -sqrt(0.6_longreal)
          w2gauss(4) = 25.0_longreal/324.0_longreal
          x2gauss(5) = sqrt(0.6_longreal)
          y2gauss(5) = sqrt(0.6_longreal)
          w2gauss(5) = 25.0_longreal/324.0_longreal
          x2gauss(6) = 0.0_longreal
          y2gauss(6) = -sqrt(0.6_longreal)
          w2gauss(6) = 10.0_longreal/81.0_longreal
          x2gauss(7) = 0.0_longreal
          y2gauss(7) = sqrt(0.6_longreal)
          w2gauss(7) = 10.0_longreal/81.0_longreal
          x2gauss(8) = -sqrt(0.6_longreal)
          y2gauss(8) = 0.0_longreal
          w2gauss(8) = 10.0_longreal/81.0_longreal
          x2gauss(9) = sqrt(0.6_longreal)
          y2gauss(9) = 0.0_longreal
          w2gauss(9) = 10.0_longreal/81.0_longreal
!
          z1gauss(1) = -0.9681602395_longreal
          w1gauss(1) = 0.0812743883_longreal
          z1gauss(2) = -0.8360311073_longreal
          w1gauss(2) = 0.1806481606_longreal
          z1gauss(3) = -0.6133714327_longreal
          w1gauss(3) = 0.2606106964_longreal
          z1gauss(4) = -0.3242534234_longreal
          w1gauss(4) = 0.3123470770_longreal
          z1gauss(5) = 0.0_longreal
          w1gauss(5) = 0.3302393550_longreal
          do i = 6, 9
              z1gauss(i) = - z1gauss(10-i)
              w1gauss(i) = w1gauss(10-i)
          end do
!
          first = .false.
      end if
!
!        build dimensions of inductor
!
      a = sqrt((rect_inductor%v2%x - rect_inductor%v4%x)**2 + (rect_inductor%v2%y -         &
                       rect_inductor%v4%y)**2 + (rect_inductor%v2%z - rect_inductor%v4%z)**2)
      magnitude = sqrt((rect_inductor%v2%x - rect_inductor%v1%x)**2 +                       &
                                 (rect_inductor%v2%y -  rect_inductor%v1%y)**2 +            &
                                               (rect_inductor%v2%z - rect_inductor%v1%z)**2)
      angle = asin(0.5_LONGreal * a / magnitude)
      b1 = magnitude * cos(angle)
      magnitude = sqrt((rect_inductor%v3%x - rect_inductor%v2%x)**2 +                       &
                                 (rect_inductor%v3%y -  rect_inductor%v2%y)**2 +            &
                                               (rect_inductor%v3%z - rect_inductor%v2%z)**2)
      angle = asin(0.5_LONGreal * a / magnitude)
      b2 = magnitude * cos(angle)
!
!        compute the radius vector from the vertex 1 of the quad to the center of the coil
!
      dx = x_coil - rect_inductor%v4%x
      dy = y_coil - rect_inductor%v4%y
      dz = z_coil - rect_inductor%v4%z
!
!        compute the mutual inductance between the coil and the lower half of the quad
!
      current_vector(1) = rect_inductor%current_vector1%x
      current_vector(2) = rect_inductor%current_vector1%y
      current_vector(3) = rect_inductor%current_vector1%z
!
!        first build the rotation matrix for the lower half of the quad
!
!        define a coordinate system for which this quad is in the xy plane.
!        first define the x axis as radius vector of node 2 minus radius vector of
!        node 4
!
      xxvec = rect_inductor%v2%x - rect_inductor%v4%x
      xyvec = rect_inductor%v2%y - rect_inductor%v4%y
      xzvec = rect_inductor%v2%z - rect_inductor%v4%z
      magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
      if (magnitude <= epsilon(1.0_longreal)) then
          print *," "
          print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
          print *," "
          stop
      end if
      xxvec = xxvec / magnitude
      xyvec = xyvec / magnitude
      xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
      yxvec = rect_inductor%current_vector1%x
      yyvec = rect_inductor%current_vector1%y
      yzvec = rect_inductor%current_vector1%z
!
!        define the z axis as the cross-product of the x and y axes
!
      zxvec = xyvec*yzvec - xzvec*yyvec
      zyvec = xzvec*yxvec - xxvec*yzvec
      zzvec = xxvec*yyvec - xyvec*yxvec
      magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
      if (magnitude <= epsilon(1.0_longreal)) then
          print *," "
          print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
          print *," "
          stop
      end if
      zxvec = zxvec / magnitude
      zyvec = zyvec / magnitude
      zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
      rotate_quad(1,1) = xxvec
      rotate_quad(1,2) = xyvec
      rotate_quad(1,3) = xzvec
      rotate_quad(2,1) = yxvec
      rotate_quad(2,2) = yyvec
      rotate_quad(2,3) = yzvec
      rotate_quad(3,1) = zxvec
      rotate_quad(3,2) = zyvec
      rotate_quad(3,3) = zzvec
      rotate_quad = transpose(rotate_quad)
!
!        outer loop is the integration over the circumference of the coil.  the next most
!        inner loop is a gaussian integration over the height of the coil.  the inner
!        most loop is the gaussian integration over the quad
!
      l12_lower = 0.0_longreal
!
      do i = 1, 2*m
          theta = pi*real(i,longreal)/real(m,longreal)
          c_vector(1) = r_coil * cos(theta)
          c_vector(2) = r_coil * sin(theta)
!
!       compute current vector for the coil in the global coordinate system
!
          coil_tmp_vector(1) = -sin(theta)
          coil_tmp_vector(2) = cos(theta)
          coil_tmp_vector(3) = 0.0_longreal
          coil_current_vec(1) = dot_product(rotate_coil(1,:),coil_tmp_vector(:))
          coil_current_vec(2) = dot_product(rotate_coil(2,:),coil_tmp_vector(:))
          coil_current_vec(3) = dot_product(rotate_coil(3,:),coil_tmp_vector(:))
!
          do j = 1, 9
              c_vector(3) = 0.5 * h_coil * z1gauss(j)
!
!       rotate coil vector into the global coordinate system and translate it
!
              rot_c_vector(1) = dot_product(rotate_coil(1,:),c_vector(:)) + dx
              rot_c_vector(2) = dot_product(rotate_coil(2,:),c_vector(:)) + dy
              rot_c_vector(3) = dot_product(rotate_coil(3,:),c_vector(:)) + dz
!
              do k = 1, 9
                  q_vector(1) = 0.5_longreal * a * (x2gauss(k) + 1.0_longreal)
                  q_vector(2) = 0.5_longreal * b1 * (y2gauss(k) - 1.0_longreal)
                  q_vector(3) = 0.0_longreal
!
!       rotate quad vector into the global coordinate system
!
                  rot_q_vector(1) = dot_product(rotate_quad(1,:),q_vector(:))
                  rot_q_vector(2) = dot_product(rotate_quad(2,:),q_vector(:))
                  rot_q_vector(3) = dot_product(rotate_quad(3,:),q_vector(:))
!
!       compute and add in quadrature term
!
                  numerator = w1gauss(j) * w2gauss(k) *                                     &
                                                 dot_product(coil_current_vec,current_vector)
                  denominator = sqrt(dot_product(rot_c_vector-rot_q_vector,                 &
                                                                  rot_c_vector-rot_q_vector))
                  l12_lower = l12_lower + numerator/denominator
              end do
          end do
      end do
!
!        compute the mutual inductance between the coil and the upper half of the quad
!
      current_vector(1) = rect_inductor%current_vector2%x
      current_vector(2) = rect_inductor%current_vector2%y
      current_vector(3) = rect_inductor%current_vector2%z
!
!        first build the rotation matrix for the upper half of the quad
!
!        define a coordinate system for which this quad is in the xy plane.
!        first define the x axis as radius vector of node 2 minus radius vector of
!        node 4
!
      xxvec = rect_inductor%v2%x - rect_inductor%v4%x
      xyvec = rect_inductor%v2%y - rect_inductor%v4%y
      xzvec = rect_inductor%v2%z - rect_inductor%v4%z
      magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
      if (magnitude <= epsilon(1.0_longreal)) then
          print *," "
          print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
          print *," "
          stop
      end if
      xxvec = xxvec / magnitude
      xyvec = xyvec / magnitude
      xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
      yxvec = rect_inductor%current_vector2%x
      yyvec = rect_inductor%current_vector2%y
      yzvec = rect_inductor%current_vector2%z
!
!        define the z axis as the cross-product of the x and y axes
!
      zxvec = xyvec*yzvec - xzvec*yyvec
      zyvec = xzvec*yxvec - xxvec*yzvec
      zzvec = xxvec*yyvec - xyvec*yxvec
      magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
      if (magnitude <= epsilon(1.0_longreal)) then
          print *," "
          print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
          print *," "
          stop
      end if
      zxvec = zxvec / magnitude
      zyvec = zyvec / magnitude
      zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
      rotate_quad(1,1) = xxvec
      rotate_quad(1,2) = xyvec
      rotate_quad(1,3) = xzvec
      rotate_quad(2,1) = yxvec
      rotate_quad(2,2) = yyvec
      rotate_quad(2,3) = yzvec
      rotate_quad(3,1) = zxvec
      rotate_quad(3,2) = zyvec
      rotate_quad(3,3) = zzvec
      rotate_quad = transpose(rotate_quad)
!
!        outer loop is the integration over the circumference of the coil.  the next most
!        inner loop is a gaussian integration over the height of the coil.  the inner
!        most loop is the gaussian integration over the quad
!
      l12_upper = 0.0_longreal
!
      do i = 1, 2*m
          theta = pi*real(i,longreal)/real(m,longreal)
          c_vector(1) = r_coil * cos(theta)
          c_vector(2) = r_coil * sin(theta)
!
!       compute current vector for the coil in the global coordinate system
!
          coil_tmp_vector(1) = -sin(theta)
          coil_tmp_vector(2) = cos(theta)
          coil_tmp_vector(3) = 0.0_longreal
          coil_current_vec(1) = dot_product(rotate_coil(1,:),coil_tmp_vector(:))
          coil_current_vec(2) = dot_product(rotate_coil(2,:),coil_tmp_vector(:))
          coil_current_vec(3) = dot_product(rotate_coil(3,:),coil_tmp_vector(:))
!
          do j = 1, 9
              c_vector(3) = 0.5 * h_coil * z1gauss(j)
!
!       rotate coil vector into the global coordinate system and translate it
!
              rot_c_vector(1) = dot_product(rotate_coil(1,:),c_vector(:)) + dx
              rot_c_vector(2) = dot_product(rotate_coil(2,:),c_vector(:)) + dy
              rot_c_vector(3) = dot_product(rotate_coil(3,:),c_vector(:)) + dz
!
              do k = 1, 9
                  q_vector(1) = 0.5_longreal * a * (x2gauss(k) + 1.0_longreal)
                  q_vector(2) = 0.25_longreal * b2 * (y2gauss(k) + 1.0_longreal)
                  q_vector(3) = 0.0_longreal
!
!       rotate quad vector into the global coordinate system
!
                  rot_q_vector(1) = dot_product(rotate_quad(1,:),q_vector(:))
                  rot_q_vector(2) = dot_product(rotate_quad(2,:),q_vector(:))
                  rot_q_vector(3) = dot_product(rotate_quad(3,:),q_vector(:))
!
!       compute and add in quadrature term
!
                  numerator = w1gauss(j) * w2gauss(k) *                                     &
                                                 dot_product(coil_current_vec,current_vector)
                  denominator = sqrt(dot_product(rot_c_vector-rot_q_vector,                 &
                                                                  rot_c_vector-rot_q_vector))
                  l12_upper = l12_upper + numerator/denominator

              end do
          end do
      end do
!
!       sum the terms to get the total mutual inductance
!
      coefficient = (mu * r_coil * n_coil * h_coil)/(8.0_longreal * real(m,longreal))
      l12 = coefficient * (b1 * l12_lower + b2 * l12_upper)
!
      end subroutine mutual_ind_quad_cir_coil

end module mqc_m
module mqr_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type

implicit none

private
public :: mutual_ind_quad_rec_coil

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal
real (kind = longreal), parameter, private :: small = 1.0e-10_longreal

contains

      subroutine mutual_ind_quad_rec_coil (a_coil, b_coil, x_coil, y_coil, z_coil, h_coil,  &
                                           n_coil, rotate_coil, rect_inductor, m, mu, l12)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        6 september 1997
!
!      purpose:      compute the mutual inductance of a rectangle and a rectangular coil.
!                    the coil and rectangle can be oriented arbitrarily in space.
!
!############################################################################################
!
!      input:
!
!
!        m                       [integer]
!                                number of quadrature points for theta integrals
!
!     output:
!
!        l12                     [real, selected_real_kind(15,90)]
!                                mutual inductance .
!
!############################################################################################
!
!=========== formal variables =============
!
      type (quad_inductor), intent(in) :: rect_inductor
      real (kind = longreal), intent(in) :: a_coil, b_coil, x_coil, y_coil, z_coil, h_coil, &
                                            n_coil, mu
      real (kind = longreal), dimension(:,:), intent(in) :: rotate_coil
      integer, intent(in) :: m
      real (kind = longreal), intent(out) :: l12
!
!========== internal variables ============
!
!
      real (kind = longreal), dimension(3,3) :: rotate_quad
      real (kind = longreal), dimension(9), save :: x2gauss, y2gauss, w2gauss, z1gauss,     &
                                                    w1gauss
      real (kind = longreal) :: xxvec, xyvec, xzvec, yxvec, yyvec, yzvec, zxvec, zyvec,     &
                                zzvec, magnitude, l12_lower, l12_upper, dx, dy, dz,         &
                                a, b1, b2, numerator, denominator, coefficient,             &
                                s, ds, deltas, theta
      real (kind = longreal), dimension(3) :: c_vector, q_vector, rot_c_vector,             &
                                              rot_q_vector, current_vector,                 &
                                              coil_current_vec, coil_tmp_vector
      integer :: i, j, k
      logical, save :: first = .true.
!
!        on the first call to this routine, initialize the weights for a 9 point 6th order
!        gaussian quadrature on a square.

      if (first) then
          x2gauss(1) = 0.0_longreal
          y2gauss(1) = 0.0_longreal
          w2gauss(1) = 16.0_longreal/81.0_longreal
          x2gauss(2) = -sqrt(0.6_longreal)
          y2gauss(2) = -sqrt(0.6_longreal)
          w2gauss(2) = 25.0_longreal/324.0_longreal
          x2gauss(3) = -sqrt(0.6_longreal)
          y2gauss(3) = sqrt(0.6_longreal)
          w2gauss(3) = 25.0_longreal/324.0_longreal
          x2gauss(4) = sqrt(0.6_longreal)
          y2gauss(4) = -sqrt(0.6_longreal)
          w2gauss(4) = 25.0_longreal/324.0_longreal
          x2gauss(5) = sqrt(0.6_longreal)
          y2gauss(5) = sqrt(0.6_longreal)
          w2gauss(5) = 25.0_longreal/324.0_longreal
          x2gauss(6) = 0.0_longreal
          y2gauss(6) = -sqrt(0.6_longreal)
          w2gauss(6) = 10.0_longreal/81.0_longreal
          x2gauss(7) = 0.0_longreal
          y2gauss(7) = sqrt(0.6_longreal)
          w2gauss(7) = 10.0_longreal/81.0_longreal
          x2gauss(8) = -sqrt(0.6_longreal)
          y2gauss(8) = 0.0_longreal
          w2gauss(8) = 10.0_longreal/81.0_longreal
          x2gauss(9) = sqrt(0.6_longreal)
          y2gauss(9) = 0.0_longreal
          w2gauss(9) = 10.0_longreal/81.0_longreal
!
          z1gauss(1) = -0.9681602395_longreal
          w1gauss(1) = 0.0812743883_longreal
          z1gauss(2) = -0.8360311073_longreal
          w1gauss(2) = 0.1806481606_longreal
          z1gauss(3) = -0.6133714327_longreal
          w1gauss(3) = 0.2606106964_longreal
          z1gauss(4) = -0.3242534234_longreal
          w1gauss(4) = 0.3123470770_longreal
          z1gauss(5) = 0.0_longreal
          w1gauss(5) = 0.3302393550_longreal
          do i = 6, 9
              z1gauss(i) = - z1gauss(10-i)
              w1gauss(i) = w1gauss(10-i)
          end do
!
          first = .false.
      end if
!
!        build dimensions of inductor
!
      a = sqrt((rect_inductor%v2%x - rect_inductor%v4%x)**2 + (rect_inductor%v2%y -         &
                    rect_inductor%v4%y)**2 + (rect_inductor%v2%z - rect_inductor%v4%z)**2)
      magnitude = sqrt((rect_inductor%v2%x - rect_inductor%v1%x)**2 +                       &
                                 (rect_inductor%v2%y -  rect_inductor%v1%y)**2 +            &
                                               (rect_inductor%v2%z - rect_inductor%v1%z)**2)
      theta = asin(0.5_LONGreal * a / magnitude)
      b1 = magnitude * cos(theta)
      magnitude = sqrt((rect_inductor%v3%x - rect_inductor%v2%x)**2 +                       &
                                 (rect_inductor%v3%y -  rect_inductor%v2%y)**2 +            &
                                               (rect_inductor%v3%z - rect_inductor%v2%z)**2)
      theta = asin(0.5_LONGreal * a / magnitude)
      b2 = magnitude * cos(theta)
!
!        compute the radius vector from the vertex 4 of the quad to the center of the coil
!
      dx = x_coil - rect_inductor%v4%x
      dy = y_coil - rect_inductor%v4%y
      dz = z_coil - rect_inductor%v4%z
!
!        compute quadrature interval over the circumference of the coil
!
      ds = 2.0_longreal * (a_coil + b_coil) / real(2*m,longreal)
!
!        compute the mutual inductance between the coil and the lower half of the quad
!
      current_vector(1) = rect_inductor%current_vector1%x
      current_vector(2) = rect_inductor%current_vector1%y
      current_vector(3) = rect_inductor%current_vector1%z
!
!        first build the rotation matrix for the lower half of the quad
!
!        define a coordinate system for which this quad is in the xy plane.
!        first define the x axis as radius vector of node 2 minus radius vector of
!        node 4
!
      xxvec = rect_inductor%v2%x - rect_inductor%v4%x
      xyvec = rect_inductor%v2%y - rect_inductor%v4%y
      xzvec = rect_inductor%v2%z - rect_inductor%v4%z
      magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
      if (magnitude <= epsilon(1.0_longreal)) then
          print *," "
          print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
          print *," "
          stop
      end if
      xxvec = xxvec / magnitude
      xyvec = xyvec / magnitude
      xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
      yxvec = rect_inductor%current_vector1%x
      yyvec = rect_inductor%current_vector1%y
      yzvec = rect_inductor%current_vector1%z
!
!        define the z axis as the cross-product of the x and y axes
!
      zxvec = xyvec*yzvec - xzvec*yyvec
      zyvec = xzvec*yxvec - xxvec*yzvec
      zzvec = xxvec*yyvec - xyvec*yxvec
      magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
      if (magnitude <= epsilon(1.0_longreal)) then
          print *," "
          print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
          print *," "
          stop
      end if
      zxvec = zxvec / magnitude
      zyvec = zyvec / magnitude
      zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
      rotate_quad(1,1) = xxvec
      rotate_quad(1,2) = xyvec
      rotate_quad(1,3) = xzvec
      rotate_quad(2,1) = yxvec
      rotate_quad(2,2) = yyvec
      rotate_quad(2,3) = yzvec
      rotate_quad(3,1) = zxvec
      rotate_quad(3,2) = zyvec
      rotate_quad(3,3) = zzvec
      rotate_quad = transpose(rotate_quad)
!
!        outer loop is the integration over the circumference of the coil.  the next most
!        inner loop is a gaussian integration over the height of the coil.  the inner
!        most loop is the gaussian integration over the quad
!
      l12_lower = 0.0_longreal
!
      do i = 1, 2*m
          s = real(i,longreal) * ds
          if (s <= 0.5_longreal * a_coil) then
              c_vector(1) = 0.5_longreal * b_coil
              c_vector(2) = s
              coil_tmp_vector(1) = 0.0_longreal
              coil_tmp_vector(2) = 1.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          else if (s <= 0.5_longreal * a_coil + b_coil) then
              deltas = s - 0.5_longreal * a_coil
              c_vector(1) = 0.5_longreal * b_coil - deltas
              c_vector(2) = 0.5_longreal * a_coil
              coil_tmp_vector(1) = -1.0_longreal
              coil_tmp_vector(2) = 0.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          else if (s <= 1.5_longreal * a_coil + b_coil) then
              deltas = s - b_coil - 0.5_longreal * a_coil
              c_vector(1) = - 0.5_longreal * b_coil
              c_vector(2) = 0.5_longreal * a_coil - deltas
              coil_tmp_vector(1) = 0.0_longreal
              coil_tmp_vector(2) = -1.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          else if (s < 2.0_longreal * b_coil + 1.5_longreal * a_coil) then
              deltas = s - b_coil - 1.5_longreal * a_coil
              c_vector(1) = -0.5_longreal * b_coil + deltas
              c_vector(2) = - 0.5_longreal * a_coil
              coil_tmp_vector(1) = 1.0_longreal
              coil_tmp_vector(2) = 0.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          else
              deltas = s - 2.0_longreal * b_coil - 1.5_longreal * a_coil
              c_vector(1) = 0.5_longreal * b_coil
              c_vector(2) = -0.5_longreal * a_coil + deltas
              coil_tmp_vector(1) = 0.0_longreal
              coil_tmp_vector(2) = 1.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          end if
!
!       compute current vector for the coil in the global coordinate system
!
          coil_current_vec(1) = dot_product(rotate_coil(1,:),coil_tmp_vector(:))
          coil_current_vec(2) = dot_product(rotate_coil(2,:),coil_tmp_vector(:))
          coil_current_vec(3) = dot_product(rotate_coil(3,:),coil_tmp_vector(:))
!
          do j = 1, 9
              c_vector(3) = 0.5 * h_coil * z1gauss(j)
!
!       rotate coil vector into the global coordinate system and translate it
!
              rot_c_vector(1) = dot_product(rotate_coil(1,:),c_vector(:)) + dx
              rot_c_vector(2) = dot_product(rotate_coil(2,:),c_vector(:)) + dy
              rot_c_vector(3) = dot_product(rotate_coil(3,:),c_vector(:)) + dz
!
              do k = 1, 9
                  q_vector(1) = 0.5_longreal * a * (x2gauss(k) + 1.0_longreal)
                  q_vector(2) = 0.5_longreal * b1 * (y2gauss(k) - 1.0_longreal)
                  q_vector(3) = 0.0_longreal
!
!       rotate quad vector into the global coordinate system
!
                  rot_q_vector(1) = dot_product(rotate_quad(1,:),q_vector(:))
                  rot_q_vector(2) = dot_product(rotate_quad(2,:),q_vector(:))
                  rot_q_vector(3) = dot_product(rotate_quad(3,:),q_vector(:))
!
!       compute and add in quadrature term
!
                  numerator = w1gauss(j) * w2gauss(k) *                                     &
                                                 dot_product(coil_current_vec,current_vector)
                  denominator = sqrt(dot_product(rot_c_vector-rot_q_vector,                 &
                                                                  rot_c_vector-rot_q_vector))
                  l12_lower = l12_lower + numerator/denominator

              end do
          end do
      end do
!
!        compute the mutual inductance between the coil and the upper half of the quad
!
      current_vector(1) = rect_inductor%current_vector2%x
      current_vector(2) = rect_inductor%current_vector2%y
      current_vector(3) = rect_inductor%current_vector2%z
!
!        first build the rotation matrix for the upper half of the quad
!
!        define a coordinate system for which this quad is in the xy plane.
!        first define the x axis as radius vector of node 2 minus radius vector of
!        node 4
!
      xxvec = rect_inductor%v2%x - rect_inductor%v4%x
      xyvec = rect_inductor%v2%y - rect_inductor%v4%y
      xzvec = rect_inductor%v2%z - rect_inductor%v4%z
      magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
      if (magnitude <= epsilon(1.0_longreal)) then
          print *," "
          print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
          print *," "
          stop
      end if
      xxvec = xxvec / magnitude
      xyvec = xyvec / magnitude
      xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
      yxvec = rect_inductor%current_vector2%x
      yyvec = rect_inductor%current_vector2%y
      yzvec = rect_inductor%current_vector2%z
!
!        define the z axis as the cross-product of the x and y axes
!
      zxvec = xyvec*yzvec - xzvec*yyvec
      zyvec = xzvec*yxvec - xxvec*yzvec
      zzvec = xxvec*yyvec - xyvec*yxvec
      magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
      if (magnitude <= epsilon(1.0_longreal)) then
          print *," "
          print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
          print *," "
          stop
      end if
      zxvec = zxvec / magnitude
      zyvec = zyvec / magnitude
      zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
      rotate_quad(1,1) = xxvec
      rotate_quad(1,2) = xyvec
      rotate_quad(1,3) = xzvec
      rotate_quad(2,1) = yxvec
      rotate_quad(2,2) = yyvec
      rotate_quad(2,3) = yzvec
      rotate_quad(3,1) = zxvec
      rotate_quad(3,2) = zyvec
      rotate_quad(3,3) = zzvec
      rotate_quad = transpose(rotate_quad)
!
!        outer loop is the integration over the circumference of the coil.  the next most
!        inner loop is a gaussian integration over the height of the coil.  the inner
!        most loop is the gaussian integration over the quad
!
      l12_upper = 0.0_longreal
!
      do i = 1, 2*m
          s = real(i,longreal) * ds
          if (s <= 0.5_longreal * a_coil) then
              c_vector(1) = 0.5_longreal * b_coil
              c_vector(2) = s
              coil_tmp_vector(1) = 0.0_longreal
              coil_tmp_vector(2) = 1.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          else if (s <= 0.5_longreal * a_coil + b_coil) then
              deltas = s - 0.5_longreal * a_coil
              c_vector(1) = 0.5_longreal * b_coil - deltas
              c_vector(2) = 0.5_longreal * a_coil
              coil_tmp_vector(1) = -1.0_longreal
              coil_tmp_vector(2) = 0.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          else if (s <= 1.5_longreal * a_coil + b_coil) then
              deltas = s - b_coil - 0.5_longreal * a_coil
              c_vector(1) = - 0.5_longreal * b_coil
              c_vector(2) = 0.5_longreal * a_coil - deltas
              coil_tmp_vector(1) = 0.0_longreal
              coil_tmp_vector(2) = -1.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          else if (s < 2.0_longreal * b_coil + 1.5_longreal * a_coil) then
              deltas = s - b_coil - 1.5_longreal * a_coil
              c_vector(1) = -0.5_longreal * b_coil + deltas
              c_vector(2) = - 0.5_longreal * a_coil
              coil_tmp_vector(1) = 1.0_longreal
              coil_tmp_vector(2) = 0.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          else
              deltas = s - 2.0_longreal * b_coil - 1.5_longreal * a_coil
              c_vector(1) = 0.5_longreal * b_coil
              c_vector(2) = -0.5_longreal * a_coil + deltas
              coil_tmp_vector(1) = 0.0_longreal
              coil_tmp_vector(2) = 1.0_longreal
              coil_tmp_vector(3) = 0.0_longreal
          end if
!
!       compute current vector for the coil in the global coordinate system
!
          coil_current_vec(1) = dot_product(rotate_coil(1,:),coil_tmp_vector(:))
          coil_current_vec(2) = dot_product(rotate_coil(2,:),coil_tmp_vector(:))
          coil_current_vec(3) = dot_product(rotate_coil(3,:),coil_tmp_vector(:))
!
          do j = 1, 9
              c_vector(3) = 0.5 * h_coil * z1gauss(j)
!
!       rotate coil vector into the global coordinate system and translate it
!
              rot_c_vector(1) = dot_product(rotate_coil(1,:),c_vector(:)) + dx
              rot_c_vector(2) = dot_product(rotate_coil(2,:),c_vector(:)) + dy
              rot_c_vector(3) = dot_product(rotate_coil(3,:),c_vector(:)) + dz
!
              do k = 1, 9
                  q_vector(1) = 0.5_longreal * a * (x2gauss(k) + 1.0_longreal)
                  q_vector(2) = 0.5_longreal * b2 * (y2gauss(k) + 1.0_longreal)
                  q_vector(3) = 0.0_longreal
!
!       rotate quad vector into the global coordinate system
!
                  rot_q_vector(1) = dot_product(rotate_quad(1,:),q_vector(:))
                  rot_q_vector(2) = dot_product(rotate_quad(2,:),q_vector(:))
                  rot_q_vector(3) = dot_product(rotate_quad(3,:),q_vector(:))
!
!       compute and add in quadrature term
!
                  numerator = w1gauss(j) * w2gauss(k) *                                     &
                                                 dot_product(coil_current_vec,current_vector)
                  denominator = sqrt(dot_product(rot_c_vector-rot_q_vector,                 &
                                                                  rot_c_vector-rot_q_vector))
                  l12_upper = l12_upper + numerator/denominator

              end do
          end do
      end do
!
!       sum the terms to get the total mutual inductance
!
      coefficient = (mu * (a_coil + b_coil) * n_coil * h_coil)/(8.0_longreal *              &
                                                                       real(m,longreal) * pi)
      l12 = coefficient * (b1 * l12_lower + b2 * l12_upper)
!
      end subroutine mutual_ind_quad_rec_coil

end module mqr_m
module m_quad_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type

implicit none

private
public :: quad_mutual_inductance
private :: self_inductance, asinh

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal
real (kind = longreal), parameter, private :: small = 1.0e-10_longreal

contains

      subroutine quad_mutual_inductance (inductor1, inductor2, mu, mutual_inductance)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        11 october 1997
!
!      purpose:      compute the mutual inductance between two arbitrary non-planar
!                    quadralaterals in three-dimensions
!
!############################################################################################
!
!      input:
!
!        mu                      [real, selected_real_kind(15,90)]
!                                permeability of the medium
!
!     output:
!
!
!
!############################################################################################
!
!=========== formal variables =============
!
      type (quad_inductor), intent(in) :: inductor1, inductor2
      real (kind = longreal), intent(in) :: mu
      real (kind = longreal), intent(out) :: mutual_inductance
!
!========== internal variables ============
!
      real (kind = longreal), dimension(3) :: current1a, current1b, current2a, current2b,  &
&                                             r1_vec, r2_vec, r1_rot_vec, r2_rot_vec
      real (kind = longreal) :: a1, b1a, b1b, a2, b2a, b2b, mutual_1a_2a,                  &
                                mutual_1a_2b, mutual_1b_2a, mutual_1b_2b, dx, dy, dz, sgn, &
                                coefficient, magnitude, theta, xxvec, xyvec, xzvec, yxvec, &
                                yyvec, yzvec, zxvec, zyvec, zzvec
      real (kind = longreal), dimension(9), save :: z1g, z2g, zw
      real (kind = LONGreal), dimension(3,3) :: rotate1, rotate2
      integer :: i, j
      logical :: touch
      logical, save :: first = .true.
!
!        on the first call to this routine, initialize the weights for a 9 point 6th order
!        gaussian quadrature on a square.
!
      if (first) then
          z1g(1) = 0.0_longreal
          z2g(1) = 0.0_longreal
          zw(1) = 16.0_longreal/81.0_longreal
          z1g(2) = -sqrt(0.6_longreal)
          z2g(2) = -sqrt(0.6_longreal)
          zw(2) = 25.0_longreal/324.0_longreal
          z1g(3) = -sqrt(0.6_longreal)
          z2g(3) = sqrt(0.6_longreal)
          zw(3) = 25.0_longreal/324.0_longreal
          z1g(4) = sqrt(0.6_longreal)
          z2g(4) = -sqrt(0.6_longreal)
          zw(4) = 25.0_longreal/324.0_longreal
          z1g(5) = sqrt(0.6_longreal)
          z2g(5) = sqrt(0.6_longreal)
          zw(5) = 25.0_longreal/324.0_longreal
          z1g(6) = 0.0_longreal
          z2g(6) = -sqrt(0.6_longreal)
          zw(6) = 10.0_longreal/81.0_longreal
          z1g(7) = 0.0_longreal
          z2g(7) = sqrt(0.6_longreal)
          zw(7) = 10.0_longreal/81.0_longreal
          z1g(8) = -sqrt(0.6_longreal)
          z2g(8) = 0.0_longreal
          zw(8) = 10.0_longreal/81.0_longreal
          z1g(9) = sqrt(0.6_longreal)
          z2g(9) = 0.0_longreal
          zw(9) = 10.0_longreal/81.0_longreal
          first = .false.
      end if
!
      mutual_1a_2a = 0.0_longreal
      mutual_1a_2b = 0.0_longreal
      mutual_1b_2a = 0.0_longreal
      mutual_1b_2b = 0.0_longreal
!
!        build the current vectors for each inductor-section
!
!
      current1a(1) = inductor1%current_vector1%x
      current1a(2) = inductor1%current_vector1%y
      current1a(3) = inductor1%current_vector1%z
      current1b(1) = inductor1%current_vector2%x
      current1b(2) = inductor1%current_vector2%y
      current1b(3) = inductor1%current_vector2%z
      current2a(1) = inductor2%current_vector1%x
      current2a(2) = inductor2%current_vector1%y
      current2a(3) = inductor2%current_vector1%z
      current2b(1) = inductor2%current_vector2%x
      current2b(2) = inductor2%current_vector2%y
      current2b(3) = inductor2%current_vector2%z
!
!        build dimensions of each inductor
!
      a1 = sqrt((inductor1%v2%x - inductor1%v4%x)**2 + (inductor1%v2%y -                    &
                      inductor1%v4%y)**2 + (inductor1%v2%z - inductor1%v4%z)**2)
      magnitude = sqrt((inductor1%v2%x - inductor1%v1%x)**2 + (inductor1%v2%y -             &
                      inductor1%v1%y)**2 + (inductor1%v2%z - inductor1%v1%z)**2)
      theta = asin(0.5_LONGreal * a1 / magnitude)
      b1a = magnitude * cos(theta)
      magnitude = sqrt((inductor1%v3%x - inductor1%v2%x)**2 + (inductor1%v3%y -             &
                      inductor1%v2%y)**2 + (inductor1%v3%z - inductor1%v2%z)**2)
      theta = asin(0.5_LONGreal * a1 / magnitude)
      b1b = magnitude * cos(theta)
      a2 = sqrt((inductor2%v2%x - inductor2%v4%x)**2 + (inductor2%v2%y -                    &
                      inductor2%v4%y)**2 + (inductor2%v2%z - inductor2%v4%z)**2)
      magnitude = sqrt((inductor2%v2%x - inductor2%v1%x)**2 + (inductor2%v2%y -             &
                      inductor2%v1%y)**2 + (inductor2%v2%z - inductor2%v1%z)**2)
      theta = asin(0.5_LONGreal * a2 / magnitude)
      b2a = magnitude * cos(theta)
      magnitude = sqrt((inductor2%v3%x - inductor2%v2%x)**2 + (inductor2%v3%y -             &
                      inductor2%v2%y)**2 + (inductor2%v3%z - inductor2%v2%z)**2)
      theta = asin(0.5_LONGreal * a2 / magnitude)
      b2b = magnitude * cos(theta)
!
!        build vector between vertex 4 of each inductor
!
      dx = inductor2%v4%x - inductor1%v4%x
      dy = inductor2%v4%y - inductor1%v4%y
      dz = inductor2%v4%z - inductor1%v4%z
!
!       mutual inductance of 1a with 2a
!
      if (abs(abs(dot_product(current1a,current2a)) - 1.0_longreal) < small) then
!
!        see if these rectangles share a common edge.  If so, do the mutual
!        inductance analytically. 
!
          touch = .FALSE.
          if (abs(inductor1%v1%x - inductor2%v1%x) < small .AND.                            &
                               abs(inductor1%v1%y - inductor2%v1%y) < small .AND.           &
                                           abs(inductor1%v1%z - inductor2%v1%z) < small) then
              mutual_1a_2a = - 0.5_LONGreal * (self_inductance(a1, b1a + b2a, mu) -         &
                                self_inductance(a1, b1a, mu) -  self_inductance(a2, b2a, mu))
              mutual_1a_2a = mutual_1a_2a / (a1*a2)
              if (abs(a1-a2) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          else if (abs(inductor1%v2%x - inductor2%v4%x) < small .AND.                       &
                               abs(inductor1%v2%y - inductor2%v4%y) < small .AND.           &
                                           abs(inductor1%v2%z - inductor2%v4%z) < small) then
              mutual_1a_2a = 0.5_LONGreal * (self_inductance(a1 + a2, b1a, mu) -            &
                                self_inductance(a1, b1a, mu) -  self_inductance(a2, b2a, mu))
              mutual_1a_2a = mutual_1a_2a / (a1*a2)
              if (abs(b1a-b2a) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          else if (abs(inductor1%v4%x - inductor2%v2%x) < small .AND.                       &
                               abs(inductor1%v4%y - inductor2%v2%y) < small .AND.           &
                                           abs(inductor1%v4%z - inductor2%v2%z) < small) then
              mutual_1a_2a = 0.5_LONGreal * (self_inductance(a1 + a2, b1a, mu) -            &
                                self_inductance(a1, b1a, mu) -  self_inductance(a2, b2a, mu))
              mutual_1a_2a = mutual_1a_2a / (a1*a2)
              if (abs(b1a-b2a) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          end if
!
          if (.NOT. touch) then
!
!        do the calculation numerically
!
!
!        first build the rotation matrix for the lower half of the 1st quad
!
!        define a coordinate system for which this quad is in the xy plane.
!        first define the x axis as radius vector of node 2 minus radius vector of
!        node 4
!
              xxvec = inductor1%v2%x - inductor1%v4%x
              xyvec = inductor1%v2%y - inductor1%v4%y
              xzvec = inductor1%v2%z - inductor1%v4%z
              magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              xxvec = xxvec / magnitude
              xyvec = xyvec / magnitude
              xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
              yxvec = inductor1%current_vector1%x
              yyvec = inductor1%current_vector1%y
              yzvec = inductor1%current_vector1%z
!
!        define the z axis as the cross-product of the x and y axes
!  
              zxvec = xyvec*yzvec - xzvec*yyvec
              zyvec = xzvec*yxvec - xxvec*yzvec
              zzvec = xxvec*yyvec - xyvec*yxvec
              magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              zxvec = zxvec / magnitude
              zyvec = zyvec / magnitude
              zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
              rotate1(1,1) = xxvec
              rotate1(1,2) = xyvec
              rotate1(1,3) = xzvec
              rotate1(2,1) = yxvec
              rotate1(2,2) = yyvec
              rotate1(2,3) = yzvec
              rotate1(3,1) = zxvec
              rotate1(3,2) = zyvec
              rotate1(3,3) = zzvec
              rotate1 = transpose(rotate1)
!
!        build rotation matrix for lower half of 2nd inductor
!
              xxvec = inductor2%v2%x - inductor2%v4%x
              xyvec = inductor2%v2%y - inductor2%v4%y
              xzvec = inductor2%v2%z - inductor2%v4%z
              magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              xxvec = xxvec / magnitude
              xyvec = xyvec / magnitude
              xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
              yxvec = inductor2%current_vector1%x
              yyvec = inductor2%current_vector1%y
              yzvec = inductor2%current_vector1%z
!
!        define the z axis as the cross-product of the x and y axes
!
              zxvec = xyvec*yzvec - xzvec*yyvec
              zyvec = xzvec*yxvec - xxvec*yzvec
              zzvec = xxvec*yyvec - xyvec*yxvec
              magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              zxvec = zxvec / magnitude
              zyvec = zyvec / magnitude
              zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
              rotate2(1,1) = xxvec
              rotate2(1,2) = xyvec
              rotate2(1,3) = xzvec
              rotate2(2,1) = yxvec
              rotate2(2,2) = yyvec
              rotate2(2,3) = yzvec
              rotate2(3,1) = zxvec
              rotate2(3,2) = zyvec
              rotate2(3,3) = zzvec
              rotate2 = transpose(rotate2)
!
!        compute sgn for transforming the integrations into the same orientation for each
!        inductor
!
              if (dot_product(current1a,current2a) > 0.0_longreal) then
                  sgn = 1.0_longreal
              else
                  sgn = -1.0_longreal
              end if
!
!        do numerical integrations to compute mutual inductances
!
              coefficient = (mu * b1a * b2a * sgn)/(4.0_longreal * pi)
              mutual_1a_2a = 0.0_longreal
              do i = 1, 9
                  r1_vec(1) = 0.5_longreal * a1 * (z1g(i) + 1.0_longreal)
                  r1_vec(2) = 0.5_longreal * b1a * (z2g(i) - 1.0_longreal)
                  r1_vec(3) = 0.0_LONGreal
                  r1_rot_vec(1) = dot_product(rotate1(1,:),r1_vec(:))
                  r1_rot_vec(2) = dot_product(rotate1(2,:),r1_vec(:))
                  r1_rot_vec(3) = dot_product(rotate1(3,:),r1_vec(:))
                  do j = 1, 9
                      r2_vec(1) = 0.5_longreal * a2 * (z1g(j) + 1.0_longreal) 
                      r2_vec(2) = 0.5_longreal * b2a * (z2g(j) - 1.0_longreal)
                      r2_vec(3) = 0.0_LONGreal
                      r2_rot_vec(1) = dot_product(rotate2(1,:),r2_vec(:)) + dx
                      r2_rot_vec(2) = dot_product(rotate2(2,:),r2_vec(:)) + dy
                      r2_rot_vec(3) = dot_product(rotate2(3,:),r2_vec(:)) + dz
                      mutual_1a_2a = mutual_1a_2a  + (zw(i)*zw(j))/                         &
                            sqrt(dot_product(r1_rot_vec-r2_rot_vec, r1_rot_vec-r2_rot_vec))
                  end do
              end do
              mutual_1a_2a = coefficient * mutual_1a_2a
          end if
!
      else if (abs(dot_product(current1a,current2a)) > small) then
          print *," "
          print *,"non-planar and non-orthogonal rectangle in quad_mutual_inductance, abort"
          print *," "
          stop
      end if
!
!       mutual inductance of 1a with 2b
!
!          currents are parallel:
!
      if (abs(abs(dot_product(current1a,current2b)) - 1.0_longreal) < small) then
!
!        see if these rectangles share a common edge.  If so, do the mutual
!        inductance analytically. 
!
          touch = .FALSE.
          if (abs(inductor1%v1%x - inductor2%v3%x) < small .AND.                            &
                          abs(inductor1%v1%y - inductor2%v3%y) < small .AND.                &
                                          abs(inductor1%v1%z - inductor2%v3%z) < small) then
              mutual_1a_2b = 0.5_LONGreal * (self_inductance(a1, b1a + b2b, mu) -           &
                                self_inductance(a1, b1a, mu) -  self_inductance(a2, b2b, mu))
              mutual_1a_2b = mutual_1a_2b / (a1*a2)
              if (abs(a1-a2) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          else if (abs(inductor1%v2%x - inductor2%v2%x) < small .AND.                       &
                          abs(inductor1%v2%y - inductor2%v2%y) < small .AND.                &
                                          abs(inductor1%v2%z - inductor2%v2%z) < small) then
              mutual_1a_2b = - 0.5_LONGreal * (self_inductance(a1 + a2, b1a, mu) -          &
                                self_inductance(a1, b1a, mu) -  self_inductance(a2, b2b, mu))
              mutual_1a_2b = mutual_1a_2b / (a1*a2)
              if (abs(b1a - b2b) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          else if (abs(inductor1%v4%x - inductor2%v4%x) < small .AND.                       &
                          abs(inductor1%v4%y - inductor2%v4%y) < small .AND.                &
                                         abs(inductor1%v4%z - inductor2%v4%z) < small) then
              mutual_1a_2b = - 0.5_LONGreal * (self_inductance(a1 + a2, b1a, mu) -          &
                                self_inductance(a1, b1a, mu) -  self_inductance(a2, b2b, mu))
              mutual_1a_2b = mutual_1a_2b / (a1*a2)
              if (abs(b1a - b2b) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          end if
!
          if (.NOT. touch) then
!
!        do the calculation numerically
!
!
!        build rotation matrix for upper half of 2nd inductor
!
              xxvec = inductor2%v2%x - inductor2%v4%x
              xyvec = inductor2%v2%y - inductor2%v4%y
              xzvec = inductor2%v2%z - inductor2%v4%z
              magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              xxvec = xxvec / magnitude
              xyvec = xyvec / magnitude
              xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
              yxvec = inductor2%current_vector2%x
              yyvec = inductor2%current_vector2%y
              yzvec = inductor2%current_vector2%z
!
!        define the z axis as the cross-product of the x and y axes
!
              zxvec = xyvec*yzvec - xzvec*yyvec
              zyvec = xzvec*yxvec - xxvec*yzvec
              zzvec = xxvec*yyvec - xyvec*yxvec
              magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              zxvec = zxvec / magnitude
              zyvec = zyvec / magnitude
              zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
              rotate2(1,1) = xxvec
              rotate2(1,2) = xyvec
              rotate2(1,3) = xzvec
              rotate2(2,1) = yxvec
              rotate2(2,2) = yyvec
              rotate2(2,3) = yzvec
              rotate2(3,1) = zxvec
              rotate2(3,2) = zyvec
              rotate2(3,3) = zzvec
              rotate2 = transpose(rotate2)
!
!        compute sgn for transforming the integrations into the same orientation for each
!        inductor
!
              if (dot_product(current1a,current2b) > 0.0_longreal) then
                  sgn = 1.0_longreal
              else
                  sgn = -1.0_longreal
              end if
!
!        do numerical integrations to compute mutual inductances
!
              coefficient = (mu * b1a * b2b * sgn)/(4.0_longreal * pi)
              mutual_1a_2b = 0.0_longreal
              do i = 1, 9
                  r1_vec(1) = 0.5_longreal * a1 * (z1g(i) + 1.0_longreal)
                  r1_vec(2) = 0.5_longreal * b1a * (z2g(i) - 1.0_longreal)
                  r1_vec(3) = 0.0_LONGreal
                  r1_rot_vec(1) = dot_product(rotate1(1,:),r1_vec(:))
                  r1_rot_vec(2) = dot_product(rotate1(2,:),r1_vec(:))
                  r1_rot_vec(3) = dot_product(rotate1(3,:),r1_vec(:))
                  do j = 1, 9
                      r2_vec(1) = 0.5_longreal * a2 * (z1g(j) + 1.0_longreal) 
                      r2_vec(2) = 0.5_longreal * b2b * (z2g(j) + 1.0_longreal)
                      r2_vec(3) = 0.0_LONGreal
                      r2_rot_vec(1) = dot_product(rotate2(1,:),r2_vec(:)) + dx
                      r2_rot_vec(2) = dot_product(rotate2(2,:),r2_vec(:)) + dy
                      r2_rot_vec(3) = dot_product(rotate2(3,:),r2_vec(:)) + dz
                      mutual_1a_2b = mutual_1a_2b  + (zw(i)*zw(j))/                         &
                        sqrt(dot_product(r1_rot_vec-r2_rot_vec, r1_rot_vec-r2_rot_vec))
                  end do
              end do
              mutual_1a_2b = coefficient * mutual_1a_2b
          end if
!
      else if (abs(dot_product(current1a,current2b)) > small) then
          print *," "
          print *,"non-planar and non-orthogonal rectangle in quad_mutual_inductance, abort"
          print *," "
          stop
      end if
!
!       mutual inductance of 1b with 2a
!
!          currents are parallel:
!
      if (abs(abs(dot_product(current1b,current2a)) - 1.0_longreal) < small) then
!
!        see if these rectangles share a common edge.  If so, do the mutual
!        inductance analytically. 
!
          touch = .FALSE.
          if (abs(inductor1%v3%x - inductor2%v1%x) < small .AND.                            &
                          abs(inductor1%v3%y - inductor2%v1%y) < small .AND.                &
                                          abs(inductor1%v3%z - inductor2%v1%z) < small) then
              mutual_1b_2a = 0.5_LONGreal * (self_inductance(a1, b1b + b2a, mu) -           &
                                self_inductance(a1, b1b, mu) -  self_inductance(a2, b2a, mu))
              mutual_1b_2a = mutual_1b_2a / (a1*a2)
              if (abs(a1-a2) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          else if (abs(inductor1%v2%x - inductor2%v2%x) < small .AND.                       &
                          abs(inductor1%v2%y - inductor2%v2%y) < small .AND.                &
                                         abs(inductor1%v2%z - inductor2%v2%z) < small) then
              mutual_1b_2a = - 0.5_LONGreal * (self_inductance(a1 + a2, b1b, mu) -          &
                                self_inductance(a1, b1b, mu) -  self_inductance(a2, b2a, mu))
              mutual_1b_2a = mutual_1b_2a / (a1*a2)
              if (abs(b1b - b2a) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          else if (abs(inductor1%v4%x - inductor2%v4%x) < small .AND.                       &
                          abs(inductor1%v4%y - inductor2%v4%y) < small .AND.                &
                                          abs(inductor1%v4%z - inductor2%v4%z) < small) then
              mutual_1b_2a = - 0.5_LONGreal * (self_inductance(a1 + a2, b1b, mu) -          &
                                self_inductance(a1, b1b, mu) -  self_inductance(a2, b2a, mu))
              mutual_1b_2a = mutual_1b_2a / (a1*a2)
              if (abs(b1b - b2a) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          end if
!
          if (.NOT. touch) then
!
!        first build the rotation matrix for the upper half of the 1st quad
!
!        define a coordinate system for which this quad is in the xy plane.
!        first define the x axis as radius vector of node 2 minus radius vector of
!        node 4
!
              xxvec = inductor1%v2%x - inductor1%v4%x
              xyvec = inductor1%v2%y - inductor1%v4%y
              xzvec = inductor1%v2%z - inductor1%v4%z
              magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              xxvec = xxvec / magnitude
              xyvec = xyvec / magnitude
              xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
              yxvec = inductor1%current_vector2%x
              yyvec = inductor1%current_vector2%y
              yzvec = inductor1%current_vector2%z
!
!        define the z axis as the cross-product of the x and y axes
!
             zxvec = xyvec*yzvec - xzvec*yyvec
              zyvec = xzvec*yxvec - xxvec*yzvec
              zzvec = xxvec*yyvec - xyvec*yxvec
              magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              zxvec = zxvec / magnitude
              zyvec = zyvec / magnitude
              zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
              rotate1(1,1) = xxvec
              rotate1(1,2) = xyvec
              rotate1(1,3) = xzvec
              rotate1(2,1) = yxvec
              rotate1(2,2) = yyvec
              rotate1(2,3) = yzvec
              rotate1(3,1) = zxvec
              rotate1(3,2) = zyvec
              rotate1(3,3) = zzvec
              rotate1 = transpose(rotate1)
!
!        build rotation matrix for lower half of 2nd inductor
!
              xxvec = inductor2%v2%x - inductor2%v4%x
              xyvec = inductor2%v2%y - inductor2%v4%y
              xzvec = inductor2%v2%z - inductor2%v4%z
              magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              xxvec = xxvec / magnitude
              xyvec = xyvec / magnitude
              xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!  
              yxvec = inductor2%current_vector1%x
              yyvec = inductor2%current_vector1%y
              yzvec = inductor2%current_vector1%z
!
!        define the z axis as the cross-product of the x and y axes
!
              zxvec = xyvec*yzvec - xzvec*yyvec
              zyvec = xzvec*yxvec - xxvec*yzvec
              zzvec = xxvec*yyvec - xyvec*yxvec
              magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              zxvec = zxvec / magnitude
              zyvec = zyvec / magnitude
              zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
              rotate2(1,1) = xxvec
              rotate2(1,2) = xyvec
              rotate2(1,3) = xzvec
              rotate2(2,1) = yxvec
              rotate2(2,2) = yyvec
              rotate2(2,3) = yzvec
              rotate2(3,1) = zxvec
              rotate2(3,2) = zyvec
              rotate2(3,3) = zzvec
              rotate2 = transpose(rotate2)
!
!        compute sgn for transforming the integrations into the same orientation for each
!        inductor
!
              if (dot_product(current1b,current2a) > 0.0_longreal) then
                  sgn = 1.0_longreal
              else
                  sgn = -1.0_longreal
              end if
!
!        do numerical integrations to compute mutual inductances
!
              coefficient = (mu * b1b * b2a * sgn)/(4.0_longreal * pi)
              mutual_1b_2a = 0.0_longreal
              do i = 1, 9
                  r1_vec(1) = 0.5_longreal * a1 * (z1g(i) + 1.0_longreal)
                  r1_vec(2) = 0.5_longreal * b1b * (z2g(i) + 1.0_longreal)
                  r1_vec(3) = 0.0_LONGreal
                  r1_rot_vec(1) = dot_product(rotate1(1,:),r1_vec(:))
                  r1_rot_vec(2) = dot_product(rotate1(2,:),r1_vec(:))
                  r1_rot_vec(3) = dot_product(rotate1(3,:),r1_vec(:))
                  do j = 1, 9
                      r2_vec(1) = 0.5_longreal * a2 * (z1g(j) + 1.0_longreal) 
                      r2_vec(2) = 0.5_longreal * b2a * (z2g(j) - 1.0_longreal)
                      r2_vec(3) = 0.0_LONGreal
                      r2_rot_vec(1) = dot_product(rotate2(1,:),r2_vec(:)) + dx
                      r2_rot_vec(2) = dot_product(rotate2(2,:),r2_vec(:)) + dy
                      r2_rot_vec(3) = dot_product(rotate2(3,:),r2_vec(:)) + dz
                      mutual_1b_2a = mutual_1b_2a  + (zw(i)*zw(j))/                         &
                            sqrt(dot_product(r1_rot_vec-r2_rot_vec, r1_rot_vec-r2_rot_vec))
                  end do
              end do
              mutual_1b_2a = coefficient * mutual_1b_2a
          end if
!
      else if (abs(dot_product(current1b,current2a)) > small) then
          print *," "
          print *,"non-planar and non-orthogonal rectangle in quad_mutual_inductance, abort"
          print *," "
          stop
      end if
!
!       mutual inductance of 1b with 2b
!
!          currents are parallel:
!
      if (abs(abs(dot_product(current1b,current2b)) - 1.0_longreal) < small) then
!
!        see if these rectangles share a common edge.  If so, do the mutual
!        inductance analytically. 
!
          touch = .FALSE.
          if (abs(inductor1%v3%x - inductor2%v3%x) < small .AND.                            &
                               abs(inductor1%v3%y - inductor2%v3%y) < small .AND.           &
                                           abs(inductor1%v3%z - inductor2%v3%z) < small) then
              mutual_1b_2b = - 0.5_LONGreal * (self_inductance(a1, b1b + b2b, mu) -         &
                                self_inductance(a1, b1b, mu) -  self_inductance(a2, b2b, mu))
              mutual_1b_2b = mutual_1b_2b / (a1*a2)
              if (abs(a1-a2) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          else if (abs(inductor1%v2%x - inductor2%v4%x) < small .AND.                       &
                               abs(inductor1%v2%y - inductor2%v4%y) < small .AND.           &
                                           abs(inductor1%v2%z - inductor2%v4%z) < small) then
              mutual_1b_2b = 0.5_LONGreal * (self_inductance(a1 + a2, b1b, mu) -            &
                                self_inductance(a1, b1b, mu) -  self_inductance(a2, b2b, mu))
              mutual_1b_2b = mutual_1b_2b / (a1*a2)
              if (abs(b1b - b2b) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          else if (abs(inductor1%v4%x - inductor2%v2%x) < small .AND.                       &
                               abs(inductor1%v4%y - inductor2%v2%y) < small .AND.           &
                                           abs(inductor1%v4%z - inductor2%v2%z) < small) then
              mutual_1b_2b = 0.5_LONGreal * (self_inductance(a1 + a2, b1b, mu) -            &
                                self_inductance(a1, b1b, mu) -  self_inductance(a2, b2b, mu))
              mutual_1b_2b = mutual_1b_2b / (a1*a2)
              if (abs(b1b - b2b) > small) then
                  print *,"bad common rectangle inductance, abort"
                  stop
              end if
              touch = .TRUE.
          end if
!
          if (.NOT. touch) then
!
!        do the calculation numerically
!
!
!        build rotation matrix for upper half of 2nd inductor
!
              xxvec = inductor2%v2%x - inductor2%v4%x
              xyvec = inductor2%v2%y - inductor2%v4%y
              xzvec = inductor2%v2%z - inductor2%v4%z
              magnitude = sqrt(xxvec**2 + xyvec**2 + xzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length x axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              xxvec = xxvec / magnitude
              xyvec = xyvec / magnitude
              xzvec = xzvec / magnitude
!
!        next define the y axis as the current vector direction
!
              yxvec = inductor2%current_vector2%x
              yyvec = inductor2%current_vector2%y
              yzvec = inductor2%current_vector2%z
!
!        define the z axis as the cross-product of the x and y axes
!
              zxvec = xyvec*yzvec - xzvec*yyvec
              zyvec = xzvec*yxvec - xxvec*yzvec
              zzvec = xxvec*yyvec - xyvec*yxvec
              magnitude = sqrt(zxvec**2 + zyvec**2 + zzvec**2)
              if (magnitude <= epsilon(1.0_longreal)) then
                  print *," "
                  print *,"zero length z axis in subroutine mutual_ind_quad_cir_coil, abort."
                  print *," "
                  stop
              end if
              zxvec = zxvec / magnitude
              zyvec = zyvec / magnitude
              zzvec = zzvec / magnitude
!
!        generate the rotation matrix with which to rotate the quad into the xy plane
!
              rotate2(1,1) = xxvec
              rotate2(1,2) = xyvec
              rotate2(1,3) = xzvec
              rotate2(2,1) = yxvec
              rotate2(2,2) = yyvec
              rotate2(2,3) = yzvec
              rotate2(3,1) = zxvec
              rotate2(3,2) = zyvec
              rotate2(3,3) = zzvec
              rotate2 = transpose(rotate2)
!
!        compute sgn for transforming the integrations into the same orientation for each
!        inductor
!
              if (dot_product(current1b,current2b) > 0.0_longreal) then
                  sgn = 1.0_longreal
              else
                  sgn = -1.0_longreal
              end if
!
!        do numerical integrations to compute mutual inductances
!
              coefficient = (mu * b1b * b2b * sgn)/(4.0_longreal * pi)
              mutual_1b_2b = 0.0_longreal
              do i = 1, 9
                  r1_vec(1) = 0.5_longreal * a1 * (z1g(i) + 1.0_longreal)
                  r1_vec(2) = 0.5_longreal * b1b * (z2g(i) + 1.0_longreal)
                  r1_vec(3) = 0.0_LONGreal
                  r1_rot_vec(1) = dot_product(rotate1(1,:),r1_vec(:))
                  r1_rot_vec(2) = dot_product(rotate1(2,:),r1_vec(:))
                  r1_rot_vec(3) = dot_product(rotate1(3,:),r1_vec(:))
                  do j = 1, 9
                      r2_vec(1) = 0.5_longreal * a2 * (z1g(j) + 1.0_longreal) 
                      r2_vec(2) = 0.5_longreal * b2b * (z2g(j) + 1.0_longreal)
                      r2_vec(3) = 0.0_LONGreal
                      r2_rot_vec(1) = dot_product(rotate2(1,:),r2_vec(:)) + dx
                      r2_rot_vec(2) = dot_product(rotate2(2,:),r2_vec(:)) + dy
                      r2_rot_vec(3) = dot_product(rotate2(3,:),r2_vec(:)) + dz
                      mutual_1b_2b = mutual_1b_2b  + (zw(i)*zw(j))/                         &
                           sqrt(dot_product(r1_rot_vec-r2_rot_vec, r1_rot_vec-r2_rot_vec))
                  end do
              end do
              mutual_1b_2b = coefficient * mutual_1b_2b
          end if
!
      else if (abs(dot_product(current1b,current2b)) > small) then
          print *," "
          print *,"non-planar and non-orthogonal rectangle in quad_mutual_inductance, abort"
          print *," "
          stop
      end if
!
!        finally, calculate the mutual inductance of the two rectangles
!
      mutual_inductance = mutual_1a_2a + mutual_1a_2b + mutual_1b_2a + mutual_1b_2b
!
      end subroutine quad_mutual_inductance

      function self_inductance (a, b, mu) result (inductance)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        11 october 1997
!
!
!=========== formal variables =============
!
      real (kind = longreal), intent(in) :: a, b, mu
      real (kind = longreal) :: inductance
!
!========== internal variables ============
!
      real (kind = longreal) :: coefficient
!
      coefficient = mu/(2.0_longreal * pi)
!
      inductance = coefficient * (a**2 * b * asinh(b/a) + a * b**2 * asinh(a/b) + (a**3 +   &
                                            b**3 - (a**2 + b**2)**1.5_longreal)/3.0_longreal)
!
      end function self_inductance
!
      function asinh(x) result(arcsinh)
!
      real (kind = longreal), intent(in) :: x
      real (kind = longreal) :: arcsinh
!
      arcsinh = log(x + sqrt(x**2 + 1.0_longreal))
!
      end function asinh

end module m_quad_m
module print_input_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use free_input

implicit none

private
public :: print_input

integer, parameter, private :: longreal = selected_real_kind(15,90)

contains

      subroutine print_input (iout, input_file)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        12 october 1997
!
!      purpose:      print a copy of the input to unit iout
!
!############################################################################################
!
!=========== formal variables =============
!
      integer, intent(in) :: iout
      character (len=80), intent(in) :: input_file
!
!========== internal variables ============
!
      integer :: n, m
!
      open (unit=nin,file=input_file,status="old",form="formatted",action="read")
      icpnt = 9999
!
!
!        read through input until eof hit.  Print output to unit iout in a format
!        that will work as a Pspice comment line
!
resq: do
          call next
          if (.NOT. eoff) then
              do n = 80, 1, -1
                  m = n
                  if (card(m:m) /= ' ') exit
              end do
              write(iout,'(''*  '',a)') card(1:m)
              icpnt = 100
          else
              exit resq
          end if
      end do resq
!
      close (unit = nin)
!
      end subroutine print_input

end module print_input_m
module rect_r_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type

implicit none

private
public :: rect_resistance

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal


contains

      subroutine rect_resistance (rect_inductor, conductivity)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        11 october 1997
!
!      purpose:      compute the resistance of a quad
!
!############################################################################################
!
!      input:
!
!     output:
!
!
!############################################################################################
!
!=========== formal variables =============
!
      type (quad_inductor), intent(inout) :: rect_inductor
      real (kind = longreal), intent(in) :: conductivity
!
!========== internal variables ============
!
      real (kind = longreal) :: cross_sectional_area, a, b, theta, magnitude, b1a, b1b
!
      a = sqrt((rect_inductor%v2%x - rect_inductor%v4%x)**2 + (rect_inductor%v2%y -         &
                      rect_inductor%v4%y)**2 + (rect_inductor%v2%z - rect_inductor%v4%z)**2)
      cross_sectional_area = rect_inductor%thickness * a
      magnitude = sqrt((rect_inductor%v2%x - rect_inductor%v1%x)**2 + (rect_inductor%v2%y - &
                      rect_inductor%v1%y)**2 + (rect_inductor%v2%z - rect_inductor%v1%z)**2)
      theta = asin(0.5_LONGreal * a / magnitude)
      b1a = magnitude * cos(theta)
      magnitude = sqrt((rect_inductor%v3%x - rect_inductor%v2%x)**2 + (rect_inductor%v3%y - &
                      rect_inductor%v2%y)**2 + (rect_inductor%v3%z - rect_inductor%v2%z)**2)
      theta = asin(0.5_LONGreal * a / magnitude)
      b1b = magnitude * cos(theta)
      b = b1a + b1b
!
      rect_inductor%resistance = b/(conductivity*cross_sectional_area)
!
      end subroutine rect_resistance
!
end module rect_r_m
module define_resq_coil_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type
use src_m
use scc_m
use free_input

implicit none

private
public :: define_resq_coil

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal
real (kind = longreal), parameter, private :: mu0 = 4.0e-7_longreal * pi
real (kind = LONGreal), parameter, private :: degrees_to_radians = pi/180.0_LONGreal

contains

      subroutine define_resq_coil (resq_coil, no_resq_coil, input_file)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        12 october 1997
!
!      purpose:      define the geometry of the res-q 3 coil
!
!############################################################################################
!
!      input:
!
!
!        none
!
!
!     output:
!
!        resq_coil              [type(rectangular_coil)]
!                               geometry of resq coil.
!                                   resq_coil%a =       1st dimension of coil
!                                   resq_coil%b =       2nd dimension of coil
!                                   resq_coil%h =       height of coil
!                                   resq_coil%turns =   turns per unit length
!                                   resq_coil%theta =   theta euler angle relative to can
!                                   resq_coil%phi =     phi euler angle relative to can
!                                   resq_coil%psi =     psi euler angle relative to can
!                                   resq_coil%self_ind = self inductance of coil
!                                   resq_coil%origin%x = x coordinate of coil center
!                                                        relative to origin of can.
!                                   resq_coil%origin%y = y coordinate of coil center
!                                                        relative to origin of can.
!                                   resq_coil%origin%z = z coordinate of coil center
!                                                        relative to origin of can.
!
!############################################################################################
!
!=========== formal variables =============
!
      type(coil), intent(out) :: resq_coil
      logical, intent(out) :: no_resq_coil
      character (len=80), intent(in) :: input_file
!
!========== internal variables ============
!
      integer :: n
!
      open (unit=nin,file=input_file,status="old",form="formatted",action="read")
      icpnt = 9999
!
!        set defaults
!
      no_resq_coil = .FALSE.
      resq_coil%a = -1.e20_longreal
      resq_coil%b = -1.e20_longreal
      resq_coil%r = -1.e20_LONGreal
      resq_coil%h = -1.e20_longreal
      resq_coil%turns = -1.e20_longreal
      resq_coil%theta = 0.0_longreal
      resq_coil%phi = 0.0_longreal
      resq_coil%psi = 0.0_longreal
      resq_coil%origin%x = -1.e20_longreal
      resq_coil%origin%y = -1.e20_longreal
      resq_coil%origin%z = -1.e20_longreal
      resq_coil%circular = .FALSE.
      resq_coil%rectangular = .FALSE.
!
!
!        read through input until resq coil input is located
!
resq: do
          call next
          if (.NOT. eoff) then
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field /= 'resq_coil_definition:') cycle resq
!
              do 
                  call next
                  if (eoff) then
                      print *," "
                      print *,"unexpected end of file while reading resq coil ",        &
                                                                        "definition, abort."
                      print *," "
                      stop
                  end if
                  if (len(field) > 0) then
                      do n = 1, len(field)
                          field(n:n) = convert_lower_case(field(n:n))  
                      end do
                  end if
!
                  if (field == 'end_of_resq_coil_definition') then
                      exit resq
                  else if (field == 'x_length') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%b = real_variable
                      resq_coil%rectangular = .TRUE.
                  else if (field == 'y_length') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%a = real_variable
                      resq_coil%rectangular = .TRUE.
                  else if (field == 'radius') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%r = real_variable
                      resq_coil%circular = .TRUE.
                  else if (field == 'height') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%h = real_variable
                  else if (field == 'turns') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%turns = real_variable
                  else if (field == 'theta') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%theta = real_variable * degrees_to_radians
                  else if (field == 'phi') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%phi = real_variable * degrees_to_radians
                  else if (field == 'psi') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%psi = real_variable * degrees_to_radians
                  else if (field == 'x_location_relative_to_case') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%origin%x = real_variable
                  else if (field == 'y_location_relative_to_case') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%origin%y = real_variable
                  else if (field == 'z_location_relative_to_case') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_coil%origin%z = real_variable
                  else if (field == 'no_resq_coil_in_simulation') then
                      no_resq_coil = .TRUE.
                      return
                  else
                      print *," "
                      print *,"unrecognized word in resq coil definition, abort."
                      print *," "
                      print *,"The unrecognized word was: ",field
                      print *," "
                      stop
                  end if
              end do
          else
              print *," "
              print *,"resq coil definition not found in input, abort."
              print *," "
              stop
          end if
      end do resq
!
      close (unit = nin)
!
!        check input to catch obvious errors
!
      if (.NOT. resq_coil%circular .AND. .NOT. resq_coil%rectangular) then
          print *," "
          print *,"resq coil dimensions not defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%circular .AND. resq_coil%rectangular) then
          print *," "
          print *,"dimensions for both a circular and a rectangular resq coil were "
          print *,"defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%rectangular .AND. resq_coil%a <= 0.0_LONGreal) then
          print *," "
          print *,"x length of resq coil not defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%rectangular .AND. resq_coil%b <= 0.0_LONGreal) then
          print *," "
          print *,"y length of resq coil not defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%circular .AND. resq_coil%r <= 0.0_LONGreal) then
          print *," "
          print *,"radius of resq coil not defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%h <= 0.0_LONGreal) then
          print *," "
          print *,"Height of resq coil not defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%turns <= 0.0_LONGreal) then
          print *," "
          print *,"Number of turns of resq coil not defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%origin%x <= -1.0e10_LONGreal) then
          print *," "
          print *,"x coordinate of resq coil center not defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%origin%y <= -1.0e10_LONGreal) then
          print *," "
          print *,"y coordinate of resq coil center not defined in resq_coil_definition, abort."
          print *," "
          stop
      else if (resq_coil%origin%z <= -1.0e10_LONGreal) then
          print *," "
          print *,"z coordinate of resq coil center not defined in resq_coil_definition, abort."
          print *," "
          stop
      end if
!
      resq_coil%turns = resq_coil%turns/resq_coil%h
!
      if (resq_coil%circular) then
          call self_ind_cir_coil (resq_coil%r, resq_coil%h, resq_coil%turns, mu0,           &
                                                                          resq_coil%self_ind)
      else
          call self_ind_rec_coil (resq_coil%a, resq_coil%b, resq_coil%h, resq_coil%turns,   &
                                                                     mu0, resq_coil%self_ind)
      end if
!
      end subroutine define_resq_coil

end module define_resq_coil_m
module gen_resq_mesh_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type
use free_input

implicit none

private
public :: gen_resq_mesh, gen_resq_mesh_size

integer, parameter, private :: longreal = selected_real_kind(15,90)

contains

      subroutine gen_resq_mesh_size (nx, ny, nz, number_resq_inductors, no_can, debug_can,  &
                                     input_file)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        11 october. 1997
!
!      purpose:      read in the dimensions of the mesh representing the device enclosure
!                    and return this information to the calling routine.
!
!############################################################################################
!
!      input:
!
!        none
!
!     output:
!
!
!############################################################################################
!
!
!=========== formal variables =============
!
      integer, intent(out) :: nx, ny, nz
      integer, intent(out) :: number_resq_inductors
      logical, intent(out) :: no_can, debug_can
      character (len=80), intent(in) :: input_file
!
!========== internal variables ============
!
      integer :: n
!
      open (unit=nin,file=input_file,status="old",form="formatted",action="read")
      icpnt = 9999
!
!        set defaults
!
      nx = 0
      ny = 0
      nz = 0
      no_can = .FALSE.
      debug_can = .FALSE.
!
!
!        read through input until can input is located
!
resq: do
          call next
          if (.NOT. eoff) then
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field /= 'can_definition:') cycle resq
!
              do 
                  call next
                  if (eoff) then
                      print *," "
                      print *,"unexpected end of file while can definition, abort."
                      print *," "
                      stop
                  end if
                  if (len(field) > 0) then
                      do n = 1, len(field)
                          field(n:n) = convert_lower_case(field(n:n))  
                      end do
                  end if
!
                  if (field == 'end_of_can_definition') then
                      exit resq
                  else if (field == 'number_of_elements_along_x_axis_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      nx = int(real_variable + 0.5_LONGreal)
                  else if (field == 'number_of_elements_along_y_axis_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      ny = int(real_variable + 0.5_LONGreal)
                  else if (field == 'number_of_elements_along_height_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      nz = int(real_variable + 0.5_LONGreal)
                  else if (field == 'no_can_in_simulation') then
                      no_can = .TRUE.
                      close (unit = nin)
                      return
                  else if (field == 'print_can_mesh_debug_information') then
                      debug_can = .TRUE.
                  end if
              end do
          else
              print *," "
              print *,"can definition not found in input, abort."
              print *," "
              stop
          end if
      end do resq
!
      close (unit = nin)
!
      if (nx <= 0) then
          print *," "
          print *,"Number of elements along x axis of can not defined in input, abort."
          print *," "
          stop
      else if (ny <= 0) then
          print *," "
          print *,"Number of elements along y axis of can not defined in input, abort."
          print *," "
          stop
      else if (nz <= 0) then
          print *," "
          print *,"Number of elements along height of can not defined in input, abort."
          print *," "
          stop
      end if
!
      nx = nx + 1
      ny = ny + 1
      nz = nz + 1
!
      number_resq_inductors = (4*nx-8+4*ny)*nz+(4*ny-8)*nx+12-8*ny
!
      end subroutine gen_resq_mesh_size

      subroutine gen_resq_mesh (number_of_resq_inductors, resq_inductors, conductivity,     &
                                nx, ny, nz, debug_can, build_vrml_of_inductance, input_file)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        11 october. 1997
!
!      purpose:      read in the mesh representing the device enclosure.
!
!############################################################################################
!
!      input:
!
!        none
!
!     output:
!
!
!
!############################################################################################
!
!=========== formal variables =============
!
      integer, intent(in) :: nx, ny, nz
      logical, intent(in) :: debug_can
      integer, intent(out) :: number_of_resq_inductors
      type (quad_inductor), dimension(:), intent(inout) :: resq_inductors
      real (kind = longreal), intent(out) :: conductivity
      logical, intent(out) :: build_vrml_of_inductance
      character (len=80), intent(in) :: input_file
!
!========== internal variables ============
!
      integer :: max_resq_inductors, i, j, k, nxmin, nxmax, nymin, nymax, nzmin, nzmax,     &
                 nmax, n
      real (kind = longreal) :: xmin, ymin, zmin, dx, dy, dz, xmax, ymax, zmax, dx2, dy2,   &
                                dz2, thickness, resq_can_dx, resq_can_dy, resq_can_dz
!
      open (unit=nin,file=input_file,status="old",form="formatted",action="read")
      icpnt = 9999
!
!        set defaults
!
      conductivity = -1.e20_LONGreal
      thickness = -1.e20_LONGreal
      resq_can_dx = -1.e20_LONGreal
      resq_can_dy = -1.e20_LONGreal
      resq_can_dz = -1.e20_LONGreal
      build_vrml_of_inductance = .FALSE.
!
!        read through input until can input is located
!
resq: do
          call next
          if (.NOT. eoff) then
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field /= 'can_definition:') cycle resq
!
              do 
                  call next
                  if (eoff) then
                      print *," "
                      print *,"unexpected end of file while reading can definition, abort."
                      print *," "
                      stop
                  end if
                  if (len(field) > 0) then
                      do n = 1, len(field)
                          field(n:n) = convert_lower_case(field(n:n))  
                      end do
                  end if
!
                  if (field == 'end_of_can_definition') then
                      exit resq
                  else if (field == 'x_length_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_can_dx = real_variable
                  else if (field == 'y_length_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_can_dy = real_variable
                  else if (field == 'height_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      resq_can_dz = real_variable
                  else if (field == 'thickness_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      thickness = real_variable
                  else if (field == 'conductivity_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      conductivity = real_variable
                  else if (field == 'number_of_elements_along_x_axis_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                  else if (field == 'number_of_elements_along_y_axis_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                  else if (field == 'number_of_elements_along_height_of_can') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                  else if (field == 'print_can_mesh_debug_information') then
                      cycle
                  else if (field == 'build_data_for_vrml_of_mesh_inductance') then
                      build_vrml_of_inductance = .TRUE.
                  else
                      print *," "
                      print *,"unrecognized word in can definition, abort."
                      print *," "
                      print *,"The unrecognized word was: ",field
                      print *," "
                      print *,"Last resq.in line that was read was: "
                      print *,card
                      print *," "
                      stop
                  end if
              end do
          else
              print *," "
              print *,"can definition not found in input, abort."
              print *," "
              stop
          end if
      end do resq
!
      close (unit = nin)
!
      if (resq_can_dx <= 0.0_LONGreal) then
          print *," "
          print *,"x length of can not defined in input, abort."
          print *," "
          stop
      else if (resq_can_dy <= 0.0_LONGreal) then
          print *," "
          print *,"y length of can not defined in input, abort."
          print *," "
          stop
      else if (resq_can_dz <= 0.0_LONGreal) then
          print *," "
          print *,"Height of can not defined in input, abort."
          print *," "
          stop
      else if (thickness <= 0.0_LONGreal) then
          print *," "
          print *,"Thickness of can not defined in input, abort."
          print *," "
          stop
      else if (conductivity <= 0.0_LONGreal) then
          print *," "
          print *,"Conductivity of can not defined in input, abort."
          print *," "
          stop
      end if
!
      max_resq_inductors = size(resq_inductors,1)
!
      number_of_resq_inductors = 0
      xmin = -0.5_longreal * resq_can_dx
      ymin = -0.5_longreal * resq_can_dy
      zmin = -resq_can_dz
      xmax = 0.5_longreal * resq_can_dx
      ymax = 0.5_longreal * resq_can_dy
      zmax = 0.0_longreal
      dx = resq_can_dx/real(nx-1,longreal)
      dy = resq_can_dy/real(ny-1,longreal)
      dz = resq_can_dz/real(nz-1,longreal)
      dx2 = 0.5_longreal * dx
      dy2 = 0.5_longreal * dy
      dz2 = 0.5_longreal * dz
!
!        define element offsets for each face of the mesh
!
      nmax = 10**(1+max(0,int(log10(real(max(nx*ny, nx*nz, ny*nz),longreal)))))
      nxmin = 0
      nxmax = nmax
      nymin = 2*nmax
      nymax = 3*nmax
      nzmin = 4*nmax
      nzmax = 5*nmax
      if (debug_can) then
          print *,"xmin starts at node ",nxmin
          print *,"xmax starts at node ",nxmax
          print *,"ymin starts at node ",nymin
          print *,"ymax starts at node ",nymax
          print *,"zmin starts at node ",nzmin
          print *,"zmax starts at node ",nzmax
      end if
!
      do i = 0, nx-2
!
!        build elements along y=ymin corner of top of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmin + real(i,longreal)*dx + dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymin
          resq_inductors(number_of_resq_inductors)%v1%z = zmax - dz2
          resq_inductors(number_of_resq_inductors)%v2%x = xmin + real(i+1,longreal) * dx
          resq_inductors(number_of_resq_inductors)%v2%y = ymin
          resq_inductors(number_of_resq_inductors)%v2%z = zmax
          resq_inductors(number_of_resq_inductors)%v3%x = xmin + real(i,longreal)*dx + dx2
          resq_inductors(number_of_resq_inductors)%v3%y = ymin + dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmax
          resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i,longreal) * dx
          resq_inductors(number_of_resq_inductors)%v4%y = ymin
          resq_inductors(number_of_resq_inductors)%v4%z = zmax
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = (nz-2)*(nx-1)+i+1+nymin
          resq_inductors(number_of_resq_inductors)%i2 = i+1+nzmax
!
!        build elements along y=ymax corner of top of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmin + real(i,longreal)*dx + dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymax - dy2
          resq_inductors(number_of_resq_inductors)%v1%z = zmax
          resq_inductors(number_of_resq_inductors)%v2%x = xmin + real(i+1,longreal) * dx
          resq_inductors(number_of_resq_inductors)%v2%y = ymax
          resq_inductors(number_of_resq_inductors)%v2%z = zmax
          resq_inductors(number_of_resq_inductors)%v3%x = xmin + real(i,longreal)*dx + dx2
          resq_inductors(number_of_resq_inductors)%v3%y = ymax
          resq_inductors(number_of_resq_inductors)%v3%z = zmax - dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i,longreal) * dx
          resq_inductors(number_of_resq_inductors)%v4%y = ymax
          resq_inductors(number_of_resq_inductors)%v4%z = zmax
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = (ny-2)*(nx-1)+i+1+nzmax
          resq_inductors(number_of_resq_inductors)%i2 = (nz-2)*(nx-1)+i+1+nymax
      end do
!
!        build elements along x=xmin corner of top of case
!
      do j = 0, ny-2
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmin + dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymin + real(j,longreal)*dy + dy2
          resq_inductors(number_of_resq_inductors)%v1%z = zmax
          resq_inductors(number_of_resq_inductors)%v2%x = xmin
          resq_inductors(number_of_resq_inductors)%v2%y = ymin + real(j+1,longreal) * dy
          resq_inductors(number_of_resq_inductors)%v2%z = zmax
          resq_inductors(number_of_resq_inductors)%v3%x = xmin
          resq_inductors(number_of_resq_inductors)%v3%y = ymin + real(j,longreal)*dy + dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmax - dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmin
          resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j,longreal) * dy
          resq_inductors(number_of_resq_inductors)%v4%z = zmax
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = j*(nx-1)+1+nzmax
          resq_inductors(number_of_resq_inductors)%i2 = (nz-2)*(ny-1)+j+1+nxmin
!
!        build elements along x=xmax corner of top of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmax - dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymin + real(j,longreal)*dy + dy2
          resq_inductors(number_of_resq_inductors)%v1%z = zmax
          resq_inductors(number_of_resq_inductors)%v2%x = xmax
          resq_inductors(number_of_resq_inductors)%v2%y = ymin + real(j,longreal) * dy
          resq_inductors(number_of_resq_inductors)%v2%z = zmax
          resq_inductors(number_of_resq_inductors)%v3%x = xmax
          resq_inductors(number_of_resq_inductors)%v3%y = ymin + real(j,longreal)*dy + dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmax - dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmax
          resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j+1,longreal) * dy
          resq_inductors(number_of_resq_inductors)%v4%z = zmax
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = j*(nx-1)+nx-1+nzmax
          resq_inductors(number_of_resq_inductors)%i2 = (nz-2)*(ny-1)+j+1+nxmax
      end do
!
      do i = 0, nx-2
!
!        build elements along y=ymin corner of bottom of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmin + real(i,longreal)*dx + dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymin
          resq_inductors(number_of_resq_inductors)%v1%z = zmin + dz2
          resq_inductors(number_of_resq_inductors)%v2%x = xmin + real(i,longreal) * dx
          resq_inductors(number_of_resq_inductors)%v2%y = ymin
          resq_inductors(number_of_resq_inductors)%v2%z = zmin
          resq_inductors(number_of_resq_inductors)%v3%x = xmin + real(i,longreal)*dx + dx2
          resq_inductors(number_of_resq_inductors)%v3%y = ymin + dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmin
          resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i+1,longreal) * dx
          resq_inductors(number_of_resq_inductors)%v4%y = ymin
          resq_inductors(number_of_resq_inductors)%v4%z = zmin
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = i+1+nymin
          resq_inductors(number_of_resq_inductors)%i2 = i+1+nzmin
!
!        build elements along y=ymax corner of bottom of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmin + real(i,longreal)*dx + dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymax - dy2
          resq_inductors(number_of_resq_inductors)%v1%z = zmin
          resq_inductors(number_of_resq_inductors)%v2%x = xmin + real(i,longreal) * dx
          resq_inductors(number_of_resq_inductors)%v2%y = ymax
          resq_inductors(number_of_resq_inductors)%v2%z = zmin
          resq_inductors(number_of_resq_inductors)%v3%x = xmin + real(i,longreal)*dx + dx2
          resq_inductors(number_of_resq_inductors)%v3%y = ymax
          resq_inductors(number_of_resq_inductors)%v3%z = zmin + dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i+1,longreal) * dx
          resq_inductors(number_of_resq_inductors)%v4%y = ymax
          resq_inductors(number_of_resq_inductors)%v4%z = zmin
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = (ny-2)*(nx-1)+i+1+nzmin
          resq_inductors(number_of_resq_inductors)%i2 = i+1+nymax
      end do
!
!        build elements along x=xmin corner of bottom of case
!
      do j = 0, ny-2
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmin + dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymin + real(j,longreal)*dy + dy2
          resq_inductors(number_of_resq_inductors)%v1%z = zmin
          resq_inductors(number_of_resq_inductors)%v2%x = xmin
          resq_inductors(number_of_resq_inductors)%v2%y = ymin + real(j,longreal) * dy
          resq_inductors(number_of_resq_inductors)%v2%z = zmin
          resq_inductors(number_of_resq_inductors)%v3%x = xmin
          resq_inductors(number_of_resq_inductors)%v3%y = ymin + real(j,longreal)*dy + dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmin + dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmin
          resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j+1,longreal) * dy
          resq_inductors(number_of_resq_inductors)%v4%z = zmin
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = j*(nx-1)+1+nzmin
          resq_inductors(number_of_resq_inductors)%i2 = j+1+nxmin
!
!        build elements along x=xmax corner of bottom of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmax - dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymin + real(j,longreal)*dy + dy2
          resq_inductors(number_of_resq_inductors)%v1%z = zmin
          resq_inductors(number_of_resq_inductors)%v2%x = xmax
          resq_inductors(number_of_resq_inductors)%v2%y = ymin + real(j+1,longreal) * dy
          resq_inductors(number_of_resq_inductors)%v2%z = zmin
          resq_inductors(number_of_resq_inductors)%v3%x = xmax
          resq_inductors(number_of_resq_inductors)%v3%y = ymin + real(j,longreal)*dy + dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmin + dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmax
          resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j,longreal) * dy
          resq_inductors(number_of_resq_inductors)%v4%z = zmin
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = j*(nx-1)+nx-1+nzmin
          resq_inductors(number_of_resq_inductors)%i2 = j+1+nxmax
      end do
!
      do k = 0, nz-2
!
!        build elements along x=xmin, y=ymin corner of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmin + dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymin
          resq_inductors(number_of_resq_inductors)%v1%z = zmin + real(k,longreal)*dz + dz2
          resq_inductors(number_of_resq_inductors)%v2%x = xmin
          resq_inductors(number_of_resq_inductors)%v2%y = ymin
          resq_inductors(number_of_resq_inductors)%v2%z = zmin + real(k+1,longreal) * dz
          resq_inductors(number_of_resq_inductors)%v3%x = xmin
          resq_inductors(number_of_resq_inductors)%v3%y = ymin + dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmin + real(k,longreal)*dz + dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmin
          resq_inductors(number_of_resq_inductors)%v4%y = ymin
          resq_inductors(number_of_resq_inductors)%v4%z = zmin + real(k,longreal) * dz
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = k*(nx-1)+1+nymin
          resq_inductors(number_of_resq_inductors)%i2 = k*(ny-1)+1+nxmin
!
!        build elements along x=xmax, y=ymin corner of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmax - dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymin
          resq_inductors(number_of_resq_inductors)%v1%z = zmin + real(k,longreal)*dz + dz2
          resq_inductors(number_of_resq_inductors)%v2%x = xmax
          resq_inductors(number_of_resq_inductors)%v2%y = ymin
          resq_inductors(number_of_resq_inductors)%v2%z = zmin + real(k,longreal) * dz
          resq_inductors(number_of_resq_inductors)%v3%x = xmax
          resq_inductors(number_of_resq_inductors)%v3%y = ymin + dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmin + real(k,longreal)*dz + dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmax
          resq_inductors(number_of_resq_inductors)%v4%y = ymin
          resq_inductors(number_of_resq_inductors)%v4%z = zmin + real(k+1,longreal) * dz
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = (k+1)*(nx-1) +  nymin
          resq_inductors(number_of_resq_inductors)%i2 = k*(ny-1) + 1 + nxmax
!
!        build elements along x=xmin, y=ymax corner of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmin + dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymax
          resq_inductors(number_of_resq_inductors)%v1%z = zmin + real(k,longreal)*dz + dz2
          resq_inductors(number_of_resq_inductors)%v2%x = xmin
          resq_inductors(number_of_resq_inductors)%v2%y = ymax
          resq_inductors(number_of_resq_inductors)%v2%z = zmin + real(k,longreal) * dz
          resq_inductors(number_of_resq_inductors)%v3%x = xmin
          resq_inductors(number_of_resq_inductors)%v3%y = ymax - dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmin + real(k,longreal)*dz + dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmin
          resq_inductors(number_of_resq_inductors)%v4%y = ymax
          resq_inductors(number_of_resq_inductors)%v4%z = zmin + real(k+1,longreal) * dz
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = k*(nx-1) + 1 + nymax
          resq_inductors(number_of_resq_inductors)%i2 = (k+1)*(ny-1) + nxmin
!
!        build elements along x=xmax, y=ymax corner of case
!
          number_of_resq_inductors = number_of_resq_inductors + 1
          if (number_of_resq_inductors > max_resq_inductors) then
              print *," "
              print *,"maximum number of can inductors exceeded, abort."
              print *," "
              stop
          end if
          resq_inductors(number_of_resq_inductors)%v1%x = xmax - dx2
          resq_inductors(number_of_resq_inductors)%v1%y = ymax
          resq_inductors(number_of_resq_inductors)%v1%z = zmin + real(k,longreal)*dz + dz2
          resq_inductors(number_of_resq_inductors)%v2%x = xmax
          resq_inductors(number_of_resq_inductors)%v2%y = ymax
          resq_inductors(number_of_resq_inductors)%v2%z = zmin + real(k+1,longreal) * dz
          resq_inductors(number_of_resq_inductors)%v3%x = xmax
          resq_inductors(number_of_resq_inductors)%v3%y = ymax - dy2
          resq_inductors(number_of_resq_inductors)%v3%z = zmin + real(k,longreal)*dz + dz2
          resq_inductors(number_of_resq_inductors)%v4%x = xmax
          resq_inductors(number_of_resq_inductors)%v4%y = ymax
          resq_inductors(number_of_resq_inductors)%v4%z = zmin + real(k,longreal) * dz
          resq_inductors(number_of_resq_inductors)%thickness = thickness
          resq_inductors(number_of_resq_inductors)%current_vector1%x = 1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%y = -1.0_longreal
          resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
          resq_inductors(number_of_resq_inductors)%i1 = (k+1)*(nx-1) + nymax
          resq_inductors(number_of_resq_inductors)%i2 = (k+1)*(ny-1) + nxmax
      end do
!
!        build remaining elements along top of case
!
      do j = 0, ny-3
          do i = 0, nx-2

              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%x = xmin + real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v1%y = ymin + real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v1%z = zmax
              resq_inductors(number_of_resq_inductors)%v2%x = xmin + real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v2%y = ymin + real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v2%z = zmax
              resq_inductors(number_of_resq_inductors)%v3%x = xmin + real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v3%y = ymin+real(j+1,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v3%z = zmax
              resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v4%z = zmax
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = j*(nx-1)+i+1+nzmax
              resq_inductors(number_of_resq_inductors)%i2 = (j+1)*(nx-1)+i+1+nzmax
          end do
      end do
!
      do j = 0, ny-2
          do i = 0, nx-3
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%x = xmin+real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v1%y = ymin+real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v1%z = zmax
              resq_inductors(number_of_resq_inductors)%v2%x = xmin+real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v2%y = ymin+real(j,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v2%z = zmax
              resq_inductors(number_of_resq_inductors)%v3%x = xmin+real(i+1,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v3%y = ymin+real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v3%z = zmax
              resq_inductors(number_of_resq_inductors)%v4%x = xmin+real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v4%y = ymin+real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v4%z = zmax
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = j*(nx-1)+i+1+nzmax
              resq_inductors(number_of_resq_inductors)%i2 = j*(nx-1)+i+2+nzmax
          end do
      end do
!
!        build remaining elements along bottom of case
!
      do j = 0, ny-3
          do i = 0, nx-2
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%x = xmin + real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v1%y = ymin + real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin
              resq_inductors(number_of_resq_inductors)%v2%x = xmin + real(i,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v2%y = ymin + real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v2%z = zmin
              resq_inductors(number_of_resq_inductors)%v3%x = xmin + real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v3%y = ymin+real(j+1,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin
              resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v4%z = zmin
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = j*(nx-1)+i+1+nzmin
              resq_inductors(number_of_resq_inductors)%i2 = (j+1)*(nx-1)+i+1+nzmin
          end do
      end do
!
      do j = 0, ny-2
          do i = 0, nx-3
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%x = xmin+real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v1%y = ymin+real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin
              resq_inductors(number_of_resq_inductors)%v2%x = xmin+real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v2%y = ymin+real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v2%z = zmin
              resq_inductors(number_of_resq_inductors)%v3%x = xmin+real(i+1,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v3%y = ymin+real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin
              resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v4%y = ymin+real(j,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v4%z = zmin
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = j*(nx-1)+i+1+nzmin
              resq_inductors(number_of_resq_inductors)%i2 = j*(nx-1)+i+2+nzmin
          end do
      end do
!
!        build remaining elements along x=xmin side of case
!
      do k = 0, nz-3
          do j = 0, ny-2
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%y = ymin + real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin + real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v1%x = xmin
              resq_inductors(number_of_resq_inductors)%v2%y = ymin + real(j,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v2%z = zmin + real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v2%x = xmin
              resq_inductors(number_of_resq_inductors)%v3%y = ymin + real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin+real(k+1,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v3%x = xmin
              resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v4%z = zmin + real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v4%x = xmin
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = k*(ny-1)+j+1+nxmin
              resq_inductors(number_of_resq_inductors)%i2 = (k+1)*(ny-1)+j+1+nxmin
          end do
      end do
!
      do k = 0, nz-2
          do j = 0, ny-3

              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%y = ymin+real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin+real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v1%x = xmin
              resq_inductors(number_of_resq_inductors)%v2%y = ymin +real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v2%z = zmin +real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v2%x = xmin
              resq_inductors(number_of_resq_inductors)%v3%y = ymin+real(j+1,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin+real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v3%x = xmin
              resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v4%z = zmin+real(k,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v4%x = xmin
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = k*(ny-1)+j+1+nxmin
              resq_inductors(number_of_resq_inductors)%i2 = k*(ny-1)+j+2+nxmin
          end do
      end do
!
!        build remaining elements along x=xmax side of case
!
      do k = 0, nz-3
          do j = 0, ny-2
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%y = ymin + real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin + real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v1%x = xmax
              resq_inductors(number_of_resq_inductors)%v2%y = ymin + real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v2%z = zmin + real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v2%x = xmax
              resq_inductors(number_of_resq_inductors)%v3%y = ymin + real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin+real(k+1,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v3%x = xmax
              resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v4%z = zmin + real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v4%x = xmax
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = k*(ny-1)+j+1+nxmax
              resq_inductors(number_of_resq_inductors)%i2 = (k+1)*(ny-1)+j+1+nxmax
          end do
      end do
!
      do k = 0, nz-2
          do j = 0, ny-3
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%y = ymin+real(j,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin+real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v1%x = xmax
              resq_inductors(number_of_resq_inductors)%v2%y = ymin +real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v2%z = zmin +real(k,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v2%x = xmax
              resq_inductors(number_of_resq_inductors)%v3%y = ymin+real(j+1,longreal)*dy+dy2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin+real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v3%x = xmax
              resq_inductors(number_of_resq_inductors)%v4%y = ymin + real(j+1,longreal)*dy
              resq_inductors(number_of_resq_inductors)%v4%z = zmin+real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v4%x = xmax
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = k*(ny-1)+j+1+nxmax
              resq_inductors(number_of_resq_inductors)%i2 = k*(ny-1)+j+2+nxmax
          end do
      end do
!
!        build remaining elements along y=ymin side of case
!
      do k = 0, nz-3
          do i = 0, nx-2
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%x = xmin + real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin + real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v1%y = ymin
              resq_inductors(number_of_resq_inductors)%v2%x = xmin + real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v2%z = zmin + real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v2%y = ymin
              resq_inductors(number_of_resq_inductors)%v3%x = xmin + real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin+real(k+1,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v3%y = ymin
              resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v4%z = zmin + real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v4%y = ymin
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = k*(nx-1)+i+1+nymin
              resq_inductors(number_of_resq_inductors)%i2 = (k+1)*(nx-1)+i+1+nymin
          end do
      end do
!
      do k = 0, nz-2
          do i = 0, nx-3
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%x = xmin+real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin+real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v1%y = ymin
              resq_inductors(number_of_resq_inductors)%v2%x = xmin +real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v2%z = zmin +real(k,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v2%y = ymin
              resq_inductors(number_of_resq_inductors)%v3%x = xmin+real(i+1,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin+real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v3%y = ymin
              resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v4%z = zmin+real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v4%y = ymin
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = k*(nx-1)+i+1+nymin
              resq_inductors(number_of_resq_inductors)%i2 = k*(nx-1)+i+2+nymin
          end do
      end do
!
!        build remaining elements along y=ymax side of case
!
      do k = 0, nz-3
          do i = 0, nx-2
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%x = xmin + real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin + real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v1%y = ymax
              resq_inductors(number_of_resq_inductors)%v2%x = xmin + real(i,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v2%z = zmin + real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v2%y = ymax
              resq_inductors(number_of_resq_inductors)%v3%x = xmin + real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin+real(k+1,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v3%y = ymax
              resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v4%z = zmin + real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v4%y = ymax
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = k*(nx-1)+i+1+nymax
              resq_inductors(number_of_resq_inductors)%i2 = (k+1)*(nx-1)+i+1+nymax
          end do
      end do
!
      do k = 0, nz-2
          do i = 0, nx-3
              number_of_resq_inductors = number_of_resq_inductors + 1
              if (number_of_resq_inductors > max_resq_inductors) then
                  print *," "
                  print *,"maximum number of can inductors exceeded, abort."
                  print *," "
                  stop
              end if
              resq_inductors(number_of_resq_inductors)%v1%x = xmin+real(i,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v1%z = zmin+real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v1%y = ymax
              resq_inductors(number_of_resq_inductors)%v2%x = xmin +real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v2%z = zmin +real(k+1,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v2%y = ymax
              resq_inductors(number_of_resq_inductors)%v3%x = xmin+real(i+1,longreal)*dx+dx2
              resq_inductors(number_of_resq_inductors)%v3%z = zmin+real(k,longreal)*dz+dz2
              resq_inductors(number_of_resq_inductors)%v3%y = ymax
              resq_inductors(number_of_resq_inductors)%v4%x = xmin + real(i+1,longreal)*dx
              resq_inductors(number_of_resq_inductors)%v4%z = zmin+real(k,longreal)*dz
              resq_inductors(number_of_resq_inductors)%v4%y = ymax
              resq_inductors(number_of_resq_inductors)%thickness = thickness
              resq_inductors(number_of_resq_inductors)%current_vector1%x = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector1%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%x = 1.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%z = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%current_vector2%y = 0.0_longreal
              resq_inductors(number_of_resq_inductors)%i1 = k*(nx-1)+i+1+nymax
              resq_inductors(number_of_resq_inductors)%i2 = k*(nx-1)+i+2+nymax
          end do
      end do
!
      end subroutine gen_resq_mesh


end module gen_resq_mesh_m
module s_rect_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type

implicit none

private
public :: rect_self_inductance
private :: asinh

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal
real (kind = longreal), parameter, private :: small = 1.0e-10_longreal

contains

      subroutine rect_self_inductance (rect_inductor, mu)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        11 october 1997
!
!      purpose:      compute the mutual inductance between two arbitrary non-planar
!                    quadralaterals in three-dimensions
!
!############################################################################################
!
!      input:
!
!     output:
!
!
!############################################################################################
!
!=========== formal variables =============
!
      type (quad_inductor), intent(inout) :: rect_inductor
      real (kind = longreal), intent(in) :: mu
!
!========== internal variables ============
!
      real (kind = longreal) :: coefficient, a, b, b1, b2, theta, magnitude
      real (kind = longreal), dimension(3) :: normal1, normal2
!
      normal1(1) = rect_inductor%current_vector1%x
      normal1(2) = rect_inductor%current_vector1%y
      normal1(3) = rect_inductor%current_vector1%z
      normal2(1) = rect_inductor%current_vector2%x
      normal2(2) = rect_inductor%current_vector2%y
      normal2(3) = rect_inductor%current_vector2%z
!
      a = sqrt((rect_inductor%v2%x - rect_inductor%v4%x)**2 + (rect_inductor%v2%y -         &
                      rect_inductor%v4%y)**2 + (rect_inductor%v2%z - rect_inductor%v4%z)**2)
      magnitude = sqrt((rect_inductor%v2%x - rect_inductor%v1%x)**2 + (rect_inductor%v2%y - &
                      rect_inductor%v1%y)**2 + (rect_inductor%v2%z - rect_inductor%v1%z)**2)
      theta = asin(0.5_LONGreal * a / magnitude)
      b1 = magnitude * cos(theta)
      magnitude = sqrt((rect_inductor%v3%x - rect_inductor%v2%x)**2 + (rect_inductor%v3%y - &
                      rect_inductor%v2%y)**2 + (rect_inductor%v3%z - rect_inductor%v2%z)**2)
      theta = asin(0.5_LONGreal * a / magnitude)
      b2 = magnitude * cos(theta)
      b = b1 + b2
!
      coefficient = mu/(2.0_longreal * pi * a**2)
!
!        planar rectangle
!
      if (abs(dot_product(normal1,normal2) - 1.0_longreal) < small) then
          rect_inductor%self_ind = coefficient * (a**2 * b * asinh(b/a) + a * b**2 *        &
                       asinh(a/b) + (a**3 + b**3 - (a**2 + b**2)**1.5_longreal)/3.0_longreal)
!
!        rectangle over a 90 degree edge
!
      else if (abs(dot_product(normal1,normal2)) < small) then
          rect_inductor%self_ind = coefficient * ((a**2 * b1 * asinh(b1/a) + a * b1**2 *    &
                              asinh(a/b1) + (a**3 + b1**3 - (a**2 + b1**2)**1.5_longreal)/  &
                              3.0_longreal) + (a**2 * b2 * asinh(b2/a) + a * b2**2 *        &
                              asinh(a/b2) + (a**3 + b2**3 - (a**2 + b2**2)**1.5_longreal)/  &
                              3.0_longreal))
!
      else
          print *," "
          print *,"non-planar and non-orthogonal rectangle in rect_self_inductance, abort"
          print *," "
          stop
      end if
!
      end subroutine rect_self_inductance
!
      function asinh(x) result(arcsinh)
!
      real (kind = longreal), intent(in) :: x
      real (kind = longreal) :: arcsinh
!
      arcsinh = log(x + sqrt(x**2 + 1.0_longreal))
!
      end function asinh
!
end module s_rect_m
module build_vrml_data_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type

implicit none

private
public :: build_vrml_data

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = LONGreal), parameter, private :: small = 1.e-10_LONGreal


contains

      subroutine build_vrml_data (nout, inductor, nx, ny, nz, debug_can)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        11 october 1997
!
!      purpose:      generate a file with data for building VRMLs of can mesh
!                    with self-indutances colored in
!
!############################################################################################
!
!      input:
!
!
!     output:
!
!
!
!############################################################################################
!
!
!=========== formal variables =============
!
      type (quad_inductor), dimension(:), intent(in) :: inductor
      integer, intent(in) :: nout, nx, ny, nz
      logical :: debug_can
!
!========== internal variables ============
!
      real (kind = LONGreal), dimension(:,:), allocatable :: nodes
      integer, dimension(:), allocatable :: node_number, inductor_number
      real (kind = LONGreal) :: x, y, z, max_inductance, inductance, a, magnitude, theta,   &
                                b1, xmin, xmax, ymin, ymax, zmin, zmax
      integer :: m, n, number_of_inductors, number_of_nodes, max_node_number, e1, e2,       &
                 correct_number_of_nodes, virtual_node
!
      number_of_inductors = size(inductor)
      max_node_number = max(maxval(inductor(:)%i1),maxval(inductor(:)%i2))
      allocate (nodes(3,max_node_number))
      correct_number_of_nodes = 2*((nx-1)*(ny-1) + (nx-1)*(nz-1) + (ny-1)*(nz-1))
      allocate (node_number(max_node_number))
      allocate (inductor_number(correct_number_of_nodes))
!
!        build node array
!
      nodes(:,:) = -1.e20_LONGreal
!
      number_of_nodes = 0
      do m = 1, number_of_inductors
!
!        vertex 1 node
!
          x = inductor(m)%v1%x
          y = inductor(m)%v1%y
          z = inductor(m)%v1%z
          n = inductor(m)%i1
          if (nodes(1,n) < -1.e10_LONGreal) then
              nodes(1,n) = x
              nodes(2,n) = y
              nodes(3,n) = z
              number_of_nodes = number_of_nodes + 1
              if (number_of_nodes > correct_number_of_nodes) then
                  print *," "
                  print *,"number of nodes too large in build_vrml_data, abort."
                  print *," "
                  stop
              end if
              node_number(n) = number_of_nodes
              inductor_number(number_of_nodes) = n
          else
              if (abs(nodes(1,n) - x) > small) then
                  print *," "
                  print *,"Bad node in subroutine build_vrml_data, abort."
                  print *,"Offending node number = ",n,", associated with inductor ",m
                  print *,"Coordinates in node array      = ",nodes(1,n), nodes(2,n),       &
                                                                                   nodes(3,n)
                  print *,"Coordinate from inductor array = ",x, y, z
                  print *," "
                  stop
              else if (abs(nodes(2,n) - y) > small) then
                  print *," "
                  print *,"Bad node in subroutine build_vrml_data, abort."
                  print *,"Offending node number = ",n,", associated with inductor ",m
                  print *,"Coordinates in node array      = ",nodes(1,n), nodes(2,n),       &
                                                                                   nodes(3,n)
                  print *,"Coordinate from inductor array = ",x, y, z
                  print *," "
                  stop
              else if (abs(nodes(3,n) - z) > small) then
                  print *," "
                  print *,"Bad node in subroutine build_vrml_data, abort."
                  print *,"Offending node number = ",n,", associated with inductor ",m
                  print *,"Coordinates in node array      = ",nodes(1,n), nodes(2,n),       &
                                                                                   nodes(3,n)
                  print *,"Coordinate from inductor array = ",x, y, z
                  print *," "
                  stop
              end if
          end if
!
!        vertex 3 node
!
          x = inductor(m)%v3%x
          y = inductor(m)%v3%y
          z = inductor(m)%v3%z
          n = inductor(m)%i2
          if (nodes(1,n) < -1.e10_LONGreal) then
              nodes(1,n) = x
              nodes(2,n) = y
              nodes(3,n) = z
              number_of_nodes = number_of_nodes + 1
              if (number_of_nodes > correct_number_of_nodes) then
                  print *," "
                  print *,"number of nodes too large in build_vrml_data, abort."
                  print *," "
                  stop
              end if
              node_number(n) = number_of_nodes
              inductor_number(number_of_nodes) = n
          else
              if (abs(nodes(1,n) - x) > small) then
                  print *," "
                  print *,"Bad node in subroutine build_vrml_data, abort."
                  print *,"Offending node number = ",n,", associated with inductor ",m
                  print *,"Coordinates in node array      = ",nodes(1,n), nodes(2,n),       &
                                                                                   nodes(3,n)
                  print *,"Coordinate from inductor array = ",x, y, z
                  print *," "
                  stop
              else if (abs(nodes(2,n) - y) > small) then
                  print *," "
                  print *,"Bad node in subroutine build_vrml_data, abort."
                  print *,"Offending node number = ",n,", associated with inductor ",m
                  print *,"Coordinates in node array      = ",nodes(1,n), nodes(2,n),       &
                                                                                   nodes(3,n)
                  print *,"Coordinate from inductor array = ",x, y, z
                  print *," "
                  stop
              else if (abs(nodes(3,n) - z) > small) then
                  print *," "
                  print *,"Bad node in subroutine build_vrml_data, abort."
                  print *,"Offending node number = ",n,", associated with inductor ",m
                  print *,"Coordinates in node array      = ",nodes(1,n), nodes(2,n),       &
                                                                                   nodes(3,n)
                  print *,"Coordinate from inductor array = ",x, y, z
                  print *," "
                  stop
              end if
          end if
!
      end do
!
!        make sure we got all the nodes
!
      if (number_of_nodes /= correct_number_of_nodes) then
              print *," "
              print *,"Wrong number of nodes in build_vrml_data, abort."
              print *,"Number of nodes encountered = ",number_of_nodes
              print *,"Correct number of nodes     = ",(nx-1)*(ny-1)*(nz-1)
              print *," "
              stop
      end if
!
!        generate node location list
!
      open (unit=nout,file="inductance.dat",status="unknown",form="formatted")
      write (nout,'(i5)') number_of_nodes + number_of_inductors
!
!        real nodes:
!
      do m = 1, number_of_nodes
          n = inductor_number(m)
          write (nout,'(i5,1p,3e15.5)') m, nodes(1,n), nodes(2,n), nodes(3,n)
      end do
!
!        virtual nodes
!
      do m = 1, number_of_inductors
          a = sqrt((inductor(m)%v2%x - inductor(m)%v4%x)**2 + (inductor(m)%v2%y -           &
                             inductor(m)%v4%y)**2 + (inductor(m)%v2%z - inductor(m)%v4%z)**2)
          magnitude = sqrt((inductor(m)%v2%x - inductor(m)%v1%x)**2 + (inductor(m)%v2%y -   &
                             inductor(m)%v1%y)**2 + (inductor(m)%v2%z - inductor(m)%v1%z)**2)
          theta = asin(0.5_LONGreal * a / magnitude)
          b1 = magnitude * cos(theta)
          n = node_number(inductor(m)%i1)
          virtual_node = m + number_of_nodes
          x = inductor(m)%v1%x + b1 * inductor(m)%current_vector1%x
          y = inductor(m)%v1%y + b1 * inductor(m)%current_vector1%y
          z = inductor(m)%v1%z + b1 * inductor(m)%current_vector1%z         
          write (nout,'(i5,1p,3e15.5)') virtual_node, x, y, z
      end do     
!
      max_inductance = maxval(abs(inductor(:)%self_ind))
!
!        generate node connectivity
!
      write (nout,'(i5)') 2*number_of_inductors
      do m = 1, number_of_inductors
          inductance = abs(inductor(m)%self_ind)/max_inductance
          e1 = inductor(m)%i1
          e2 = inductor(m)%i2
          virtual_node = number_of_nodes + m
          write (nout,'(2i10,1pe15.5)') node_number(e1), virtual_node, inductance
          write (nout,'(2i10,1pe15.5)') virtual_node, node_number(e2), inductance
      end do
!
!        print out locations for each face of the can
!
      if (debug_can) then
        xmin = minval(nodes(1,inductor_number(1:number_of_nodes)))
        xmax = maxval(nodes(1,inductor_number(1:number_of_nodes)))
        ymin = minval(nodes(2,inductor_number(1:number_of_nodes)))
        ymax = maxval(nodes(2,inductor_number(1:number_of_nodes)))
        zmin = minval(nodes(3,inductor_number(1:number_of_nodes)))
        zmax = maxval(nodes(3,inductor_number(1:number_of_nodes)))
        write (12,'(''*'')')
        write (12,'(''*'')')
        do m = 1, number_of_nodes
          n = inductor_number(m)
          if (abs(nodes(1,n) - xmin) < small) then
              write(12,'(''* Node '',i5,'' on xmin face at x,y,z='',1p,3e14.5)')  n,         &
                                                           nodes(1,n), nodes(2,n), nodes(3,n)
          end if
        end do
        do m = 1, number_of_nodes
          n = inductor_number(m)
          if (abs(nodes(1,n) - xmax) < small) then
              write(12,'(''* Node '',i5,'' on xmax face at x,y,z='',1p,3e14.5)')  n,         &
                                                           nodes(1,n), nodes(2,n), nodes(3,n)
          end if
        end do
        do m = 1, number_of_nodes
          n = inductor_number(m)
          if (abs(nodes(2,n) - ymin) < small) then
              write(12,'(''* Node '',i5,'' on ymin face at x,y,z='',1p,3e14.5)')  n,         &
                                                           nodes(1,n), nodes(2,n), nodes(3,n)
          end if
        end do
        do m = 1, number_of_nodes
          n = inductor_number(m)
          if (abs(nodes(2,n) - ymax) < small) then
              write(12,'(''* Node '',i5,'' on ymax face at x,y,z='',1p,3e14.5)')  n,         &
                                                           nodes(1,n), nodes(2,n), nodes(3,n)
          end if
        end do
        do m = 1, number_of_nodes
          n = inductor_number(m)
          if (abs(nodes(3,n) - zmin) < small) then
              write(12,'(''* Node '',i5,'' on zmin face at x,y,z='',1p,3e14.5)')  n,         &
                                                           nodes(1,n), nodes(2,n), nodes(3,n)
          end if
        end do
        do m = 1, number_of_nodes
          n = inductor_number(m)
          if (abs(nodes(3,n) - zmax) < small) then
              write(12,'(''* Node '',i5,'' on zmax face at x,y,z='',1p,3e14.5)')  n,         &
                                                           nodes(1,n), nodes(2,n), nodes(3,n)
          end if
        end do
        write (12,'(''*'')')
        write (12,'(''*'')')
      end if
!
      close (unit=nout)
!
      deallocate (nodes)
      deallocate (node_number)
!
      end subroutine build_vrml_data

end module build_vrml_data_m
module define_wand_coils_m

!
!      copyright (c) 1997 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!

use define_type
use scc_m
use free_input

implicit none

private
public :: define_wand_coils

integer, parameter, private :: longreal = selected_real_kind(15,90)
real (kind = longreal), parameter, private :: pi = 3.141592653589793_longreal
real (kind = longreal), parameter, private :: mu0 = 4.0e-7_longreal * pi
real (kind = LONGreal), parameter, private :: degrees_to_radians = pi/180.0_LONGreal

contains

      subroutine define_wand_coils (transmit_coil, receive_coil_1, receive_coil_2,          &
                                    input_file)
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        12 october 1997
!
!      purpose:      define the geometry of the transmit wand coils
!
!############################################################################################
!
!      input:
!
!
!        none
!
!
!     output:
!
!        transmit_coil          [type(circular_coil)]
!                               geometry of wand transmit coil.
!                                   transmit_coil%r =       radius of coil
!                                   transmit_coil%h =       height of coil
!                                   transmit_coil%turns =   turns per unit length
!                                   transmit_coil%theta =   theta euler angle relative to can
!                                   transmit_coil%phi =     phi euler angle relative to can
!                                   transmit_coil%psi =     psi euler angle relative to can
!                                   transmit_coil%self_ind = self inductance of coil
!                                   transmit_coil%origin%x = x coordinate of coil center
!                                                            relative to the resq can
!                                                            reference point
!                                   transmit_coil%origin%y = y coordinate of coil center
!                                                            relative to the resq can
!                                                            reference point
!                                   transmit_coil%origin%z = z coordinate of coil center
!                                                            relative to the resq can
!                                                            reference point
!
!        receive_coil_1         [type(circular_coil)]
!                               geometry of 1st wand receive coil.
!                                   receive_coil_1%r =      radius of coil
!                                   receive_coil_1%h =      height of coil
!                                   receive_coil_1%turns =  turns per unit length
!                                   receive_coil_1%theta =  theta euler angle relative to can
!                                   receive_coil_1%phi =    phi euler angle relative to can
!                                   receive_coil_1%psi =    psi euler angle relative to can
!                                   receive_coil_1%self_ind = self inductance of coil
!                                   receive_coil_1%origin%x = x coordinate of coil center
!                                                             relative to the resq can
!                                                             reference point
!                                   receive_coil_1%origin%y = y coordinate of coil center
!                                                             relative to the resq can
!                                                             reference point
!                                   receive_coil_1%origin%z = z coordinate of coil center
!                                                             relative to the resq can
!                                                             reference point
!
!        receive_coil_2         [type(circular_coil)]
!                               geometry of 2nd wand receive coil.
!                                   receive_coil_2%r =      radius of coil
!                                   receive_coil_2%h =      height of coil
!                                   receive_coil_2%turns =  turns per unit length
!                                   receive_coil_2%theta =  theta euler angle relative to can
!                                   receive_coil_2%phi =    phi euler angle relative to can
!                                   receive_coil_2%psi =    psi euler angle relative to can
!                                   receive_coil_2%self_ind = self inductance of coil
!                                   receive_coil_2%origin%x = x coordinate of coil center
!                                                             relative to the resq can
!                                                             reference point
!                                   receive_coil_2%origin%y = y coordinate of coil center
!                                                             relative to the resq can
!                                                             reference point
!                                   receive_coil_2%origin%z = z coordinate of coil center
!                                                             relative to the resq can
!                                                             reference point
!
!
!############################################################################################
!
!
!=========== formal variables =============
!
      type(coil), intent(out) :: transmit_coil, receive_coil_1, receive_coil_2
      character (len=80), intent(in) :: input_file
!
!========== internal variables ============
!
      type (vector) :: wand_location
      integer :: n
      real (kind = LONGreal) :: x, y, z, theta, phi, psi
!
!        Wand transmit coil definition
!
      open (unit=nin,file=input_file,status="old",form="formatted",action="read")
!
!        set defaults
!
      transmit_coil%r = -1.e20_longreal
      transmit_coil%h = -1.e20_longreal
      transmit_coil%turns = -1.e20_longreal
      transmit_coil%origin%x = -1.e20_longreal
      transmit_coil%origin%y = -1.e20_longreal
      transmit_coil%origin%z = -1.e20_longreal
      transmit_coil%circular = .TRUE.
      transmit_coil%rectangular = .FALSE.
!
!        read through input until wand coil input is located
!
      icpnt = 9999
trans :do
          call next
          if (.NOT. eoff) then
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field /= 'wand_transmit_coil_definition:') cycle trans 
!
              do 
                  call next
                  if (eoff) then
                      print *," "
                      print *,"unexpected end of file while reading transmit coil ",        &
                                                                        "definition, abort."
                      print *," "
                      stop
                  end if
                  if (len(field) > 0) then
                      do n = 1, len(field)
                          field(n:n) = convert_lower_case(field(n:n))  
                      end do
                  end if
!
                  if (field == 'end_of_wand_transmit_coil_definition') then
                      exit trans 
                  else if (field == 'radius') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      transmit_coil%r = real_variable
                  else if (field == 'height') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      transmit_coil%h = real_variable
                  else if (field == 'turns') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      transmit_coil%turns = real_variable
                  else if (field == 'x_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      transmit_coil%origin%x = real_variable
                  else if (field == 'y_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      transmit_coil%origin%y = real_variable
                  else if (field == 'z_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      transmit_coil%origin%z = real_variable
                  else
                      print *," "
                      print *,"unrecognized word in wand coil definition, abort."
                      print *," "
                      print *,"The unrecognized word was: ",field
                      print *," "
                      stop
                  end if
              end do
          else
              print *," "
              print *,"wand transmit coil definition not found in input, abort."
              print *," "
              stop
          end if
      end do trans 
!
!        check input to catch obvious errors
!
      if (transmit_coil%r <= 0.0_LONGreal) then
          print *," "
          print *,"Radius of wand transmit coil not defined in input, abort."
          print *," "
          stop
      else if (transmit_coil%h <= 0.0_LONGreal) then
          print *," "
          print *,"Heigth of wand transmit coil not defined in input, abort."
          print *," "
          stop
      else if (transmit_coil%turns <= 0.0_LONGreal) then
          print *," "
          print *,"Number of turns of wand transmit coil not defined in input, abort."
          print *," "
          stop
      else if (transmit_coil%origin%x <= -1.0e10_LONGreal) then
          print *," "
          print *,"x coordinate of wand transmit coil center not defined in input, abort."
          print *," "
          stop
      else if (transmit_coil%origin%y <= -1.0e10_LONGreal) then
          print *," "
          print *,"y coordinate of wand transmit coil center not defined in input, abort."
          print *," "
          stop
      else if (transmit_coil%origin%z <= -1.0e10_LONGreal) then
          print *," "
          print *,"z coordinate of wand transmit coil center not defined in input, abort."
          print *," "
          stop
      end if
!
      transmit_coil%turns = transmit_coil%turns/transmit_coil%h
      call self_ind_cir_coil (transmit_coil%r, transmit_coil%h, transmit_coil%turns, mu0,   &
                                                                      transmit_coil%self_ind)
!
!        Wand receiver coil 1 definition
!
!        set defaults
!
      receive_coil_1%r = -1.e20_longreal
      receive_coil_1%h = -1.e20_longreal
      receive_coil_1%turns = -1.e20_longreal
      receive_coil_1%origin%x = -1.e20_longreal
      receive_coil_1%origin%y = -1.e20_longreal
      receive_coil_1%origin%z = -1.e20_longreal
      receive_coil_1%circular = .TRUE.
      receive_coil_1%rectangular = .FALSE.
!
!        read through input until wand coil input is located
!
      icpnt = 9999
      rewind nin
wand1:do
          call next
          if (.NOT. eoff) then
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field /= 'wand_receive_coil_1_definition:') cycle wand1
!
              do 
                  call next
                  if (eoff) then
                      print *," "
                      print *,"unexpected end of file while reading receive coil 1 ",       &
                                                                        "definition, abort."
                      print *," "
                      stop
                  end if
                  if (len(field) > 0) then
                      do n = 1, len(field)
                          field(n:n) = convert_lower_case(field(n:n))  
                      end do
                  end if
!
                  if (field == 'end_of_wand_receive_coil_1_definition') then
                      exit wand1
                  else if (field == 'radius') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_1%r = real_variable
                  else if (field == 'height') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_1%h = real_variable
                  else if (field == 'turns') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_1%turns = real_variable
                  else if (field == 'x_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_1%origin%x = real_variable
                  else if (field == 'y_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_1%origin%y = real_variable
                  else if (field == 'z_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_1%origin%z = real_variable
                  else
                      print *," "
                      print *,"unrecognized word in wand coil definition, abort."
                      print *," "
                      print *,"The unrecognized word was: ",field
                      print *," "
                      stop
                  end if
              end do
          else
              print *," "
              print *,"wand receive coil 1 definition not found in input, abort."
              print *," "
              stop
          end if
      end do wand1
!
!        check input to catch obvious errors
!
      if (receive_coil_1%r <= 0.0_LONGreal) then
          print *," "
          print *,"Radius of wand receive coil 1 not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_1%h <= 0.0_LONGreal) then
          print *," "
          print *,"Heigth of wand receive coil 1 not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_1%turns <= 0.0_LONGreal) then
          print *," "
          print *,"Number of turns of receive coil 1 not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_1%origin%x <= -1.0e10_LONGreal) then
          print *," "
          print *,"x coordinate of wand receive coil 1 center not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_1%origin%y <= -1.0e10_LONGreal) then
          print *," "
          print *,"y coordinate of wand receive coil 1 center not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_1%origin%z <= -1.0e10_LONGreal) then
          print *," "
          print *,"z coordinate of wand receive coil 1 center not defined in input, abort."
          print *," "
          stop
      end if
!
      receive_coil_1%turns = receive_coil_1%turns/receive_coil_1%h
      call self_ind_cir_coil (receive_coil_1%r, receive_coil_1%h, receive_coil_1%turns,     &
                                                                mu0, receive_coil_1%self_ind)
!
!        Wand receiver coil 2 definition
!
!        set defaults
!
      receive_coil_2%r = -1.e20_longreal
      receive_coil_2%h = -1.e20_longreal
      receive_coil_2%turns = -1.e20_longreal
      receive_coil_2%origin%x = -1.e20_longreal
      receive_coil_2%origin%y = -1.e20_longreal
      receive_coil_2%origin%z = -1.e20_longreal
      receive_coil_2%circular = .TRUE.
      receive_coil_2%rectangular = .FALSE.
!
!        read through input until wand coil input is located
!
      icpnt = 9999
      rewind nin
wand2:do
          call next
          if (.NOT. eoff) then
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field /= 'wand_receive_coil_2_definition:') cycle wand2
!
              do 
                  call next
                  if (eoff) then
                      print *," "
                      print *,"unexpected end of file while reading receive coil 2 ",       &
                                                                        "definition, abort."
                      print *," "
                      stop
                  end if
                  if (len(field) > 0) then
                      do n = 1, len(field)
                          field(n:n) = convert_lower_case(field(n:n))  
                      end do
                  end if
!
                  if (field == 'end_of_wand_receive_coil_2_definition') then
                      exit wand2
                  else if (field == 'radius') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_2%r = real_variable
                  else if (field == 'height') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_2%h = real_variable
                  else if (field == 'turns') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_2%turns = real_variable
                  else if (field == 'x_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_2%origin%x = real_variable
                  else if (field == 'y_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_2%origin%y = real_variable
                  else if (field == 'z_location_in_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      receive_coil_2%origin%z = real_variable
                  else
                      print *," "
                      print *,"unrecognized word in wand coil definition, abort."
                      print *," "
                      print *,"The unrecognized word was: ",field
                      print *," "
                      stop
                  end if
              end do
          else
              print *," "
              print *,"wand receive coil 2 definition not found in input, abort."
              print *," "
              stop
          end if
      end do wand2
!
!        check input to catch obvious errors
!
      if (receive_coil_2%r <= 0.0_LONGreal) then
          print *," "
          print *,"Radius of wand receive coil 2 not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_2%h <= 0.0_LONGreal) then
          print *," "
          print *,"Heigth of wand receive coil 2 not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_2%turns <= 0.0_LONGreal) then
          print *," "
          print *,"Number of turns of receive coil 2 not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_2%origin%x <= -1.0e10_LONGreal) then
          print *," "
          print *,"x coordinate of wand receive coil 2 center not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_1%origin%y <= -1.0e10_LONGreal) then
          print *," "
          print *,"y coordinate of wand receive coil 2 center not defined in input, abort."
          print *," "
          stop
      else if (receive_coil_1%origin%z <= -1.0e10_LONGreal) then
          print *," "
          print *,"z coordinate of wand receive coil 2 center not defined in input, abort."
          print *," "
          stop
      end if
!
      receive_coil_2%turns = receive_coil_2%turns/receive_coil_2%h
      call self_ind_cir_coil (receive_coil_2%r, receive_coil_2%h, receive_coil_2%turns,     &
                                                                mu0, receive_coil_2%self_ind)
!
!        read through input until wand input is located
!
      x = -1.e20_LONGreal
      y = -1.e20_LONGreal
      z = -1.e20_LONGreal
      theta = 0.0_LONGreal
      phi = 0.0_LONGreal
      psi = 0.0_LONGreal
!
      icpnt = 9999
      rewind nin
wand: do
          call next
          if (.NOT. eoff) then
              if (len(field) > 0) then
                  do n = 1, len(field)
                      field(n:n) = convert_lower_case(field(n:n))  
                  end do
              end if
!
              if (field /= 'wand_location_definition:') cycle wand
!
              do 
                  call next
                  if (eoff) then
                      print *," "
                      print *,"unexpected end of file while reading wand location, abort."
                      print *," "
                      stop
                  end if
                  if (len(field) > 0) then
                      do n = 1, len(field)
                          field(n:n) = convert_lower_case(field(n:n))  
                      end do
                  end if
!
                  if (field == 'end_of_wand_location_definition') then
                      exit wand
                  else if (field == 'theta_angle_of_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      theta = real_variable * degrees_to_radians
                  else if (field == 'phi_angle_of_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      phi = real_variable * degrees_to_radians
                  else if (field == 'psi_angle_of_wand') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      psi = real_variable * degrees_to_radians
                  else if (field == 'x_location_of_wand_relative_to_case') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      x = real_variable
                  else if (field == 'y_location_of_wand_relative_to_case') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      y = real_variable
                  else if (field == 'z_location_of_wand_relative_to_case') then
                      call value (real_variable, free_format_error_flag) 
                      call check_eof
                      call check_number
                      z = real_variable
                  else
                      print *," "
                      print *,"unrecognized word in wand location definition, abort."
                      print *," "
                      print *,"The unrecognized word was: ",field
                      print *," "
                      stop
                  end if
              end do
          else
              print *," "
              print *,"wand location definition not found in input, abort."
              print *," "
              stop
          end if
      end do wand
!
!        check input to catch obvious errors
!
      if (x <= -1.0e10_LONGreal) then
          print *," "
          print *,"x coordinate of wand not defined in input, abort."
          print *," "
          stop
      else if (y <= -1.0e10_LONGreal) then
          print *," "
          print *,"y coordinate of wand not defined in input, abort."
          print *," "
          stop
      else if (z <= -1.0e10_LONGreal) then
          print *," "
          print *,"z coordinate of wand not defined in input, abort."
          print *," "
          stop
      end if
!
      transmit_coil%origin%x = transmit_coil%origin%x + x
      transmit_coil%origin%y = transmit_coil%origin%y + y
      transmit_coil%origin%z = transmit_coil%origin%z + z
      transmit_coil%theta = theta
      transmit_coil%phi = phi
      transmit_coil%psi = psi
      receive_coil_1%origin%x = receive_coil_1%origin%x + x
      receive_coil_1%origin%y = receive_coil_1%origin%y + y
      receive_coil_1%origin%z = receive_coil_1%origin%z + z
      receive_coil_1%theta = theta
      receive_coil_1%phi = phi
      receive_coil_1%psi = psi
      receive_coil_2%origin%x = receive_coil_2%origin%x + x
      receive_coil_2%origin%y = receive_coil_2%origin%y + y
      receive_coil_2%origin%z = receive_coil_2%origin%z + z
      receive_coil_2%theta = theta
      receive_coil_2%phi = phi
      receive_coil_2%psi = psi
!
      close (unit = nin)
!
      end subroutine define_wand_coils

end module define_wand_coils_m
      subroutine induct
!
!      copyright (c) 1997, 1998 by quetzal computational associates, inc.
!      address any questions about this copyright notice to:
!
!         john k. prentice
!         quetzal computational associates, incorporated
!         3455 main avenue, suite 4
!         durango, co   81301
!         usa
!
!         phone:  970-382-8979
!         e-mail: quetzal@quetzalcoatl.com
!
!
!      author:       john k. prentice
!      affiliation:  quetzal computational associates, inc.
!      dates:        11 october 1997
!
!      purpose:      this program generates pspice input for modeling the low-frequency
!                    electromagnetic properties of a res-q communications system.
!
      use define_type
      use gen_resq_mesh_m
      use s_rect_m
      use m_quad_m
      use rect_r_m
      use define_wand_coils_m
      use define_resq_coil_m
      use mcc_m
      use mcr_m
      use mqc_m
      use mqr_m
      use computer_time_m
      use misc_input_m
      use print_input_m
      use build_vrml_data_m
!
      implicit none
!
!============== parameters ================
!
      integer, parameter :: longreal = selected_real_kind(15,90)
      real (kind = longreal), parameter :: pi = 3.141592653589793_longreal
      real (kind = longreal), parameter :: mu0 = 4.0e-7_longreal * pi
      real (kind = longreal), parameter :: small = 1.0e-10_longreal
!
!========== internal variables ============
!
      integer :: number_of_resq_inductors, m, n, e1, e2, number_of_quad_quad_mutuals,       &
                 max_resq_inductors, nx, ny, nz, no_of_quad_quad_mutuals_used,              &
                 no_of_wand_quad_mutuals, no_of_wand_quad_mutuals_used,                     &
                 no_of_resq_quad_mutuals, no_of_resq_quad_mutuals_used,                     &
                 theta_integral_quad_points
      type (quad_inductor), dimension(:), allocatable :: resq_inductors
      real (kind = longreal), dimension(6) :: coil_coil_mutuals
      type(coil) :: transmit_coil, receive_coil_1, receive_coil_2
      type(coil) :: resq_coil
      real (kind = longreal), dimension(3,3) :: wand_rotation_matrix, resq_rotation_matrix, &
                                                rotation_matrix
      real (kind = longreal) :: computer_time_now, computer_time_before,                    &
                                elapsed_computer_time, inductance, resq_conductivity,       &
                                theta, psi, phi, qq_mutual_inductance_cutoff,               &
                                min_qq_mutual_inductance, max_qq_mutual_inductance,         &
                                min_cq_mutual_inductance, max_cq_mutual_inductance,         &
                                min_rq_mutual_inductance, max_rq_mutual_inductance,         &
                                wq_mutual_inductance_cutoff, rq_mutual_inductance_cutoff,   &
                                real_inductance, min_qq_real_mutuals, max_qq_real_mutuals,  &
                                min_quad_self_ind, max_quad_self_ind, min_resistance,       &
                                max_resistance, inductor_to_ground_capacitance
      logical :: no_can, debug_can, build_vrml_of_inductance, no_resq_coil, exists, do_output
      integer, dimension(:), allocatable :: node_map
      character (len=80) :: input_file, output_file, wand_include_file, coil_include_file
      character (len=1) :: answer
!
       number_of_quad_quad_mutuals=0  !JRA
      do_output = .FALSE.
      input_file = "induct.in"
!
      inquire (file=input_file, exist=exists)
      if (.NOT. exists) then
          print *," "
          print *,"Input file does not exist, abort."
          print *," "
          stop
      end if
!
      output_file = "induct.out"
!
!        open circuits file for the main Pspice input
!
      if (do_output) then
      open (unit=12,file=output_file,form="formatted",status="unknown",action="write")
!
      write (12,'(''*'')')
      write (12,'(''*  Quetzal Computational Associates, Incorporated.'')')
      write (12,'(''*  Spice input for low-frequency electromagnetic modeling of '')')
      write (12,'(''*  communications between shielding coils.'')')
      write (12,'(''*  Version 2.2'')')
      write (12,'(''*'')')
      write (12,'(''*'')')
      write (12,'(''*  Incorporate this file into a Pspice input file containing '')')
      write (12,'(''*  a valid transmit wand and Res-Q coil circuit description '')')
      write (12,'(''*  and commands for executing Pspice.'')')
      write (12,'(''*'')')
      write (12,'(''*  Note:  All units must be MKS.'')')
      write (12,'(''*'')')
!
!        make a copy of the induct input in the Pspice input
!
      call print_input (12, input_file)
      end if
!
!        read miscellaneous input
!
      call misc_input (qq_mutual_inductance_cutoff, wq_mutual_inductance_cutoff,            &
                       rq_mutual_inductance_cutoff, inductor_to_ground_capacitance,         &
                       theta_integral_quad_points, input_file)
!        
       print *," "
       print *," "
       print *," "
       print *,"==================== Induct calculation statistics ===================="
       print *," "
!
      min_qq_mutual_inductance = 1.e20_LONGreal
      min_cq_mutual_inductance = 1.e20_LONGreal
      min_rq_mutual_inductance = 1.e20_LONGreal
      max_qq_mutual_inductance = -1.e20_LONGreal
      max_cq_mutual_inductance = -1.e20_LONGreal
      max_rq_mutual_inductance = -1.e20_LONGreal
      no_of_wand_quad_mutuals = 0
      no_of_wand_quad_mutuals_used = 0
      no_of_resq_quad_mutuals = 0
      no_of_resq_quad_mutuals_used = 0
      min_qq_real_mutuals = 1.e20_LONGreal
      max_qq_real_mutuals = -1.e20_LONGreal
      min_quad_self_ind = 1.e20_LONGreal
      max_quad_self_ind = -1.e20_LONGreal
      min_resistance = 1.e20_LONGreal
      max_resistance = -1.e20_LONGreal
!
!############################################################################################
!
!        see if there is a can, and if so, what model to use
!
!############################################################################################
!
      call gen_resq_mesh_size (nx, ny, nz, max_resq_inductors, no_can, debug_can,           &
                               input_file)
!
      call computer_time (computer_time_before)
!
!############################################################################################!
!        define geometry and self-inductance of coils in the wand
!
!############################################################################################!
      call define_wand_coils (transmit_coil, receive_coil_1, receive_coil_2, input_file)
!
!        define mutual inductances of coils in the wand
!
      theta = transmit_coil%theta
      phi = transmit_coil%phi
      psi = transmit_coil%psi
!
!        define rotation matrix for the wand (see Goldstein, page 147)
!
      wand_rotation_matrix(1,1) = cos(psi)*cos(phi)-cos(theta)*sin(phi)*sin(psi)
      wand_rotation_matrix(1,2) = cos(psi)*sin(phi)+cos(theta)*cos(phi)*sin(psi)
      wand_rotation_matrix(1,3) = sin(psi)*sin(theta)
      wand_rotation_matrix(2,1) = -sin(psi)*cos(phi) - cos(theta)*sin(phi)*cos(psi)
      wand_rotation_matrix(2,2) = -sin(psi)*sin(phi) + cos(theta)*cos(phi)*cos(psi)
      wand_rotation_matrix(2,3) = cos(psi)*sin(theta)
      wand_rotation_matrix(3,1) = sin(theta)*sin(phi)
      wand_rotation_matrix(3,2) = -sin(theta)*cos(phi)
      wand_rotation_matrix(3,3) = cos(theta)
!
      rotation_matrix(:,:) = 0.0_LONGreal
      rotation_matrix(1,1) = 1.0_LONGreal
      rotation_matrix(2,2) = 1.0_LONGreal
      rotation_matrix(3,3) = 1.0_LONGreal
      call mutual_ind_cir_cir_coils (transmit_coil%r, receive_coil_1%r,                     &
                                     transmit_coil%origin%x - receive_coil_1%origin%x,      &
                                     transmit_coil%origin%y - receive_coil_1%origin%y,      &
                                     transmit_coil%origin%z - receive_coil_1%origin%z,      &
                                     transmit_coil%h, receive_coil_1%h,                     &
                                     transmit_coil%turns, receive_coil_1%turns, mu0,        &
                                     rotation_matrix, theta_integral_quad_points,           &
                                     coil_coil_mutuals(1))
      coil_coil_mutuals(1) = coil_coil_mutuals(1) / sqrt(transmit_coil%self_ind *           &
                                                                     receive_coil_1%self_ind)
!
      call mutual_ind_cir_cir_coils (transmit_coil%r, receive_coil_2%r,                     &
                                     transmit_coil%origin%x - receive_coil_2%origin%x,      &
                                     transmit_coil%origin%y - receive_coil_2%origin%y,      &
                                     transmit_coil%origin%z - receive_coil_2%origin%z,      &
                                     transmit_coil%h, receive_coil_2%h,                     &
                                     transmit_coil%turns, receive_coil_2%turns, mu0,        &
                                     rotation_matrix, theta_integral_quad_points,           &
                                     coil_coil_mutuals(2))
      coil_coil_mutuals(2) = coil_coil_mutuals(2) / sqrt(transmit_coil%self_ind *           &
                                                                     receive_coil_2%self_ind)
!
      call mutual_ind_cir_cir_coils (receive_coil_1%r, receive_coil_2%r,                    &
                                     receive_coil_1%origin%x - receive_coil_2%origin%x,     &
                                     receive_coil_1%origin%y - receive_coil_2%origin%y,     &
                                     receive_coil_1%origin%z - receive_coil_2%origin%z,     &
                                     receive_coil_1%h, receive_coil_2%h,                    &
                                     receive_coil_1%turns, receive_coil_2%turns, mu0,       &
                                     rotation_matrix, theta_integral_quad_points,           &
                                     coil_coil_mutuals(3))
      coil_coil_mutuals(3) = coil_coil_mutuals(3) / sqrt(receive_coil_1%self_ind *          &
                                                                     receive_coil_2%self_ind)
!
      call computer_time (computer_time_now)
      elapsed_computer_time = computer_time_now - computer_time_before
      computer_time_before = computer_time_now
      write(unit=*,fmt="(a,f7.3)") "cpu time to define wand geometries/inductances = ",     &
                                                                        elapsed_computer_time
!
!############################################################################################
!
!        define geometry and self-inductance of the res-q coil
!
!############################################################################################
!
      call define_resq_coil (resq_coil, no_resq_coil, input_file)
      if (.NOT. no_resq_coil) then
!
!        define mutual inductances of coils in the wand
!
          theta = resq_coil%theta
          phi = resq_coil%phi
          psi = resq_coil%psi
!
!        define rotation matrix for the coil (see Goldstein, page 147)
!
          resq_rotation_matrix(1,1) = cos(psi)*cos(phi)-cos(theta)*sin(phi)*sin(psi)
          resq_rotation_matrix(1,2) = cos(psi)*sin(phi)+cos(theta)*cos(phi)*sin(psi)
          resq_rotation_matrix(1,3) = sin(psi)*sin(theta)
          resq_rotation_matrix(2,1) = -sin(psi)*cos(phi) - cos(theta)*sin(phi)*cos(psi)
          resq_rotation_matrix(2,2) = -sin(psi)*sin(phi) + cos(theta)*cos(phi)*cos(psi)
          resq_rotation_matrix(2,3) = cos(psi)*sin(theta)
          resq_rotation_matrix(3,1) = sin(theta)*sin(phi)
          resq_rotation_matrix(3,2) = -sin(theta)*cos(phi)
          resq_rotation_matrix(3,3) = cos(theta)
!
!        define mutual inductances of wand and resq coils
!
          if (resq_coil%rectangular) then
              call mutual_ind_cir_rec_coils (transmit_coil%r, resq_coil%a, resq_coil%b,     &
                                     transmit_coil%origin%x - resq_coil%origin%x,           &
                                     transmit_coil%origin%y - resq_coil%origin%y,           &
                                     transmit_coil%origin%z - resq_coil%origin%z,           &
                                     transmit_coil%h, resq_coil%h, transmit_coil%turns,     &
                                     resq_coil%turns, mu0, wand_rotation_matrix,            &
                                     resq_rotation_matrix, theta_integral_quad_points,      &
                                     coil_coil_mutuals(4))
          else
              rotation_matrix = matmul(wand_rotation_matrix,transpose(resq_rotation_matrix))
              call mutual_ind_cir_cir_coils (transmit_coil%r, resq_coil%r,                  &
                                          transmit_coil%origin%x - resq_coil%origin%x,      &
                                          transmit_coil%origin%y - resq_coil%origin%y,      &
                                          transmit_coil%origin%z - resq_coil%origin%z,      &
                                          transmit_coil%h, resq_coil%h,                     &
                                          transmit_coil%turns, resq_coil%turns, mu0,        &
                                          rotation_matrix, theta_integral_quad_points,      &
                                          coil_coil_mutuals(4))
          end if
          coil_coil_mutuals(4) = coil_coil_mutuals(4) / sqrt(transmit_coil%self_ind *       &
                                                                          resq_coil%self_ind)
!
          if (resq_coil%rectangular) then
              call mutual_ind_cir_rec_coils (receive_coil_1%r, resq_coil%a, resq_coil%b,    &
                                     receive_coil_1%origin%x - resq_coil%origin%x,          &
                                     receive_coil_1%origin%y - resq_coil%origin%y,          &
                                     receive_coil_1%origin%z - resq_coil%origin%z,          &
                                     receive_coil_1%h, resq_coil%h, receive_coil_1%turns,   &
                                     resq_coil%turns, mu0, wand_rotation_matrix,            &
                                     resq_rotation_matrix, theta_integral_quad_points,      &
                                     coil_coil_mutuals(5))
          else
              rotation_matrix = matmul(wand_rotation_matrix,transpose(resq_rotation_matrix))
              call mutual_ind_cir_cir_coils (receive_coil_1%r, resq_coil%r,                 &
                                         receive_coil_1%origin%x - resq_coil%origin%x,      &
                                         receive_coil_1%origin%y - resq_coil%origin%y,      &
                                         receive_coil_1%origin%z - resq_coil%origin%z,      &
                                         receive_coil_1%h, resq_coil%h,                     &
                                         receive_coil_1%turns, resq_coil%turns, mu0,        &
                                         rotation_matrix, theta_integral_quad_points,       &
                                         coil_coil_mutuals(5))
          end if
          coil_coil_mutuals(5) = coil_coil_mutuals(5) / sqrt(receive_coil_1%self_ind *      &
                                                                          resq_coil%self_ind)
!
          if (resq_coil%rectangular) then
              call mutual_ind_cir_rec_coils (receive_coil_2%r, resq_coil%a, resq_coil%b,    &
                                     receive_coil_2%origin%x - resq_coil%origin%x,          &
                                     receive_coil_2%origin%y - resq_coil%origin%y,          &
                                     receive_coil_2%origin%z - resq_coil%origin%z,          &
                                     receive_coil_2%h, resq_coil%h, receive_coil_2%turns,   &
                                     resq_coil%turns, mu0, wand_rotation_matrix,            & 
                                     resq_rotation_matrix, theta_integral_quad_points,      &
                                     coil_coil_mutuals(6))
          else
              rotation_matrix = matmul(wand_rotation_matrix,transpose(resq_rotation_matrix))
              call mutual_ind_cir_cir_coils (receive_coil_2%r, resq_coil%r,                 &
                                         receive_coil_2%origin%x - resq_coil%origin%x,      &
                                         receive_coil_2%origin%y - resq_coil%origin%y,      &
                                         receive_coil_2%origin%z - resq_coil%origin%z,      &
                                         receive_coil_2%h, resq_coil%h,                     &
                                         receive_coil_2%turns, resq_coil%turns, mu0,        &
                                         rotation_matrix, theta_integral_quad_points,       &
                                         coil_coil_mutuals(6))
          end if
          coil_coil_mutuals(6) = coil_coil_mutuals(6) / sqrt(receive_coil_2%self_ind *      &
                                                                          resq_coil%self_ind)
!

          call computer_time (computer_time_now)
          elapsed_computer_time = computer_time_now - computer_time_before
          computer_time_before = computer_time_now
          write(unit=*,fmt="(a,a,f7.3)") "cpu time to define res-q coil ",                  &
                                             "geometry/inductances = ", elapsed_computer_time
      end if
!
!############################################################################################
!
!        construct spice input for coils
!
!############################################################################################
!
      if (do_output) then
      write (12,'(''*'')')
      write (12,'(''********* Induct diagnostic information regarding the wand'')')
      write (12,'(''*'')')
      write (12,'(''*  inductance of the transmit coil = '', 1pe12.5)') transmit_coil%self_ind
      write (12,'(''*  inductance of receive coil 1 = '', 1pe12.5)') receive_coil_1%self_ind
      write (12,'(''*  inductance of receive coil 2 = '', 1pe12.5)') receive_coil_2%self_ind
      write (12,'(''*  mutual inductance between transmit coil and receive coil 1 = '',      &
&                                                             1pe12.5)') coil_coil_mutuals(1)
      write (12,'(''*  mutual inductance between transmit coil and receive coil 2 = '',      &
&                                                             1pe12.5)') coil_coil_mutuals(2)
      write (12,'(''*  mutual inductance between receive coil 1 and receive coil 2 = '',     &
&                                                             1pe12.5)') coil_coil_mutuals(3)
      write (12,'(''*'')')
!
      if (.NOT. no_resq_coil) then
          write (12,'(''*'')')
          write (12,'(''********* Induct diagnostic information regarding the Res-Q coil'')')
          write (12,'(''*'')')
          write (12,'(''*  inductance of the Res-Q receive coil = '',1pe12.5)')              &
                                                                           resq_coil%self_ind
          write (12,'(''*'')')
          write (12,'(''*********'')')
          write (12,'(''*'')')
!
          write (12,'(''*'')')
          write (12,'(''*  wand to receive coil mutuals'')')
          write (12,'(''*'')')
          write (12,'(''kw_1r_1  lw_1  lr_1  '',1pe12.5)') coil_coil_mutuals(4)
          write (12,'(''kw_2r_2  lw_2  lr_1  '',1pe12.5)') coil_coil_mutuals(5)
          write (12,'(''kw_3r_3  lw_3  lr_1  '',1pe12.5)') coil_coil_mutuals(6)
      end if
      end if
!
!############################################################################################
!
!        construct resq can mesh
!
!############################################################################################
!
!
      if (.NOT. no_can) then
          call computer_time (computer_time_before)
          max_resq_inductors = (4*nx-8+4*ny)*nz+(4*ny-8)*nx+12-8*ny
          allocate (resq_inductors(max_resq_inductors))
          call gen_resq_mesh (number_of_resq_inductors, resq_inductors, resq_conductivity,  &
                              nx, ny, nz, debug_can, build_vrml_of_inductance, input_file)
!
!        compute self-inductance and resistance of the inductors.  generate pspice
!        input for the inductors
!
          do m = 1, number_of_resq_inductors
!   
!        compute the resistance of the quad
!
              call rect_resistance (resq_inductors(m), resq_conductivity)
!
!        compute the self-inductance of the quad
!
              call rect_self_inductance (resq_inductors(m), mu0)
              min_quad_self_ind = min(min_quad_self_ind,abs(resq_inductors(m)%self_ind))
              max_quad_self_ind = max(max_quad_self_ind,abs(resq_inductors(m)%self_ind))
          end do
!
!        build vrml files, if requested
!
          if (build_vrml_of_inductance .OR. debug_can) then
              call build_vrml_data (3, resq_inductors, nx, ny, nz, debug_can)
          end if
!
!        build Pspice input include file
!
          no_of_quad_quad_mutuals_used = 0   !JRA
          if (do_output) then
          write(12,'(''*'')')
          write(12,'(''*  can inductors'')')
          write(12,'(''*'')')
          do m = 1, number_of_resq_inductors
!
              e1 = resq_inductors(m)%i1
              e2 = resq_inductors(m)%i2
!
              if (debug_can) then
                  write (12,'(''*'')')
                  write (12,'(''* inductor '',i5)') m
                  write (12,'(''*     nodes '',i5,'' to '',i5)') e1, e2
                  write (12,'(''*     vertex 1 coordinates = '',1p,3e15.5)')                 &
                       resq_inductors(m)%v1%x, resq_inductors(m)%v1%y, resq_inductors(m)%v1%z
                  write (12,'(''*     vertex 2 coordinates = '',1p,3e15.5)')                 &
                       resq_inductors(m)%v2%x, resq_inductors(m)%v2%y, resq_inductors(m)%v2%z
                  write (12,'(''*     vertex 3 coordinates = '',1p,3e15.5)')                 &
                       resq_inductors(m)%v3%x, resq_inductors(m)%v3%y, resq_inductors(m)%v3%z
                  write (12,'(''*     vertex 4 coordinates = '',1p,3e15.5)')                 &
                       resq_inductors(m)%v4%x, resq_inductors(m)%v4%y, resq_inductors(m)%v4%z
                  write (12,'(''*     current vector 1 = '',1p,3e15.5)')                     &
                                                      resq_inductors(m)%current_vector1%x,  &
                                                      resq_inductors(m)%current_vector1%y,  &
                                                      resq_inductors(m)%current_vector1%z
                   write (12,'(''*     current vector 2 = '',1p,3e15.5)')                    &
                                                      resq_inductors(m)%current_vector2%x,  &
                                                      resq_inductors(m)%current_vector2%y,  &
                                                      resq_inductors(m)%current_vector2%z
                  write (12,'(''*'')')
              end if
!
              write(12,'(''rq'',i10.10,''  '',i10.10,''q  '',i10.10,''rq  '',1pe12.5)') m, e1&
                                                             ,m, resq_inductors(m)%resistance
!
              write(12,'(''lq'',i10.10,''  '',i10.10,''rq  '',i10.10,''q   '',1pe12.5)') m, m&
                                                              ,e2, resq_inductors(m)%self_ind
          end do
!
!        add a resistor to ground with a very large impedance to make pspice happy
!
          write(12,'(''rq'',i10.10,''  '',i10.10,''q  0  1.0e6'')')                          &
                                             number_of_resq_inductors+1, resq_inductors(1)%i1
!
          call computer_time (computer_time_now)
          elapsed_computer_time = computer_time_now - computer_time_before
          computer_time_before = computer_time_now
          write(unit=*,fmt="(a,a,f7.3)") "cpu time to define res-q can ",                   &
                                        "geometry/self-inductances = ", elapsed_computer_time
!
          if (max_resq_inductors /= number_of_resq_inductors) then
              print *,"number of resq_inductors wrong, abort."
              write(unit=12,fmt="(a)") "number of resq_inductors wrong, abort."
              stop
          end if
!
!        compute the mutual inductances between the elements of the res-q can
!
          write(12,'(''*'')')
          write(12,'(''*  can mutual inductances'')')
          write(12,'(''*'')')
          number_of_quad_quad_mutuals = 0
          no_of_quad_quad_mutuals_used = 0
          do m = 1, number_of_resq_inductors-1
              do n = m+1, number_of_resq_inductors
                  number_of_quad_quad_mutuals = number_of_quad_quad_mutuals + 1
                  call quad_mutual_inductance (resq_inductors(m), resq_inductors(n), mu0,   &
                                                                             real_inductance)
                  min_qq_real_mutuals = min(min_qq_real_mutuals,abs(real_inductance))
                  max_qq_real_mutuals = max(max_qq_real_mutuals,abs(real_inductance))
                  inductance = real_inductance /                                            &
                                  sqrt(resq_inductors(m)%self_ind*resq_inductors(n)%self_ind)
                  min_qq_mutual_inductance = min(min_qq_mutual_inductance, abs(inductance))
                  max_qq_mutual_inductance = max(max_qq_mutual_inductance, abs(inductance))
                  if (abs(inductance) >= qq_mutual_inductance_cutoff) then
                      no_of_quad_quad_mutuals_used = no_of_quad_quad_mutuals_used + 1
                      if (abs(inductance) > 1.0_LONGreal) then
                          print *," "
                          print *,"bad mutual inductance between quads ",m," and ",n
                          print *,"computed mutual inductance = ",inductance
                          print *,"self inductance of quad ",m," is ",                      &
                                                                   resq_inductors(m)%self_ind
                          print *,"self inductance of quad ",n," is ",                      &
                                                                   resq_inductors(n)%self_ind
                          print *,"mutual inductance of quads is ",real_inductance
                          print *," "
                          write (unit=12,fmt="(a,i10,a,i10)")                                &
                                          "bad mutual inductance between quads ",m," and ",n
                          write (unit=12,fmt="(a,i10)") "computed mutual inductance = ",     &
                                                                                   inductance
                          write (unit=12,fmt="(a,i10,a,1pe15.5)")                            &
                                        "self inductance of quad ",m," is ",                &
                                                                   resq_inductors(m)%self_ind
                          write (unit=12,fmt="(a,i10,a,1pe15.5)")                            &
                                        "self inductance of quad ",n," is ",                &
                                                                   resq_inductors(n)%self_ind
                          print *,"geometry of inductor ",m
                          e1 = resq_inductors(m)%i1
                          e2 = resq_inductors(m)%i2
                          print *,"     nodes ",e1," to ",e2
                          print *,"     vertex 1 coordinates = ",resq_inductors(m)%v1%x,    &
                                               resq_inductors(m)%v1%y, resq_inductors(m)%v1%z
                          print *,"     vertex 2 coordinates = ",resq_inductors(m)%v2%x,    &
                                               resq_inductors(m)%v2%y, resq_inductors(m)%v2%z
                          print *,"     vertex 3 coordinates = ",resq_inductors(m)%v3%x,    &
                                               resq_inductors(m)%v3%y, resq_inductors(m)%v3%z
                          print *,"     vertex 4 coordinates = ",resq_inductors(m)%v4%x,    &
                                               resq_inductors(m)%v4%y, resq_inductors(m)%v4%z
                          print *,"     current vector 1 = ",                               &
                                                      resq_inductors(m)%current_vector1%x,  &
                                                      resq_inductors(m)%current_vector1%y,  &
                                                      resq_inductors(m)%current_vector1%z
                          print *,"     current vector 2 = ",                               &
                                                      resq_inductors(m)%current_vector2%x,  &
                                                      resq_inductors(m)%current_vector2%y,  &
                                                      resq_inductors(m)%current_vector2%z
                          print *," "
                          print *,"geometry of inductor ",n
                          e1 = resq_inductors(n)%i1
                          e2 = resq_inductors(n)%i2
                          print *,"     nodes ",e1," to ",e2
                          print *,"     vertex 1 coordinates = ",resq_inductors(n)%v1%x,    &
                                               resq_inductors(n)%v1%y, resq_inductors(n)%v1%z
                          print *,"     vertex 2 coordinates = ",resq_inductors(n)%v2%x,    &
                                               resq_inductors(n)%v2%y, resq_inductors(n)%v2%z
                          print *,"     vertex 3 coordinates = ",resq_inductors(n)%v3%x,    &
                                               resq_inductors(n)%v3%y, resq_inductors(n)%v3%z
                          print *,"     vertex 4 coordinates = ",resq_inductors(n)%v4%x,    &
                                               resq_inductors(n)%v4%y, resq_inductors(n)%v4%z
                          print *,"     current vector 1 = ",                               &
                                                      resq_inductors(n)%current_vector1%x,  &
                                                      resq_inductors(n)%current_vector1%y,  &
                                                      resq_inductors(n)%current_vector1%z
                          print *,"     current vector 2 = ",                               &
                                                      resq_inductors(n)%current_vector2%x,  &
                                                      resq_inductors(n)%current_vector2%y,  &
                                                      resq_inductors(n)%current_vector2%z
                          stop
                      end if
                      write(12,'(''kq'',i10.10,''_'',i10.10,'' lq'',i10.10,'' lq'',i10.10,'' &
&                                                         '',1pe12.5)') m, n, m, n, inductance
                  end if
              end do
          end do
!
!        add a low capacitance capacitor to each inductor node
!
          if (inductor_to_ground_capacitance > EPSILON(1.0_LONGreal)) then
              allocate (node_map(number_of_resq_inductors))
              node_map = -1
              do m = 1, number_of_resq_inductors
                  e1 = resq_inductors(m)%i1
                  if (node_map(e1) <= 0) then
                      write(12,'(''cq'',i10.10,''  '',i10.10,''q  0 '',1pe12.5)') e1, e1,    &
                                                               inductor_to_ground_capacitance
                      node_map(e1) = e1
                  end if
                  e2 = resq_inductors(m)%i2
                  if (node_map(e2) <= 0) then
                      write(12,'(''cq'',i10.10,''  '',i10.10,''q  0 '',1pe12.5)') e2, e2,    &
                                                               inductor_to_ground_capacitance
                      node_map(e2) = e2
                  end if
              end do
              deallocate (node_map)
          end if
!
          call computer_time (computer_time_now)
          elapsed_computer_time = computer_time_now - computer_time_before
          computer_time_before = computer_time_now
          write(unit=*,fmt="(a,a,f7.3)") "cpu time to define res-q can mutual ",            &
                                                      "inductances = ", elapsed_computer_time
!
          if (number_of_quad_quad_mutuals /=                                                &
                              (number_of_resq_inductors*(number_of_resq_inductors-1))/2) then
              print *,"miscount of quad mutual inductances, abort."
              write(unit=12,fmt="(a)") "miscount of quad mutual inductances, abort."
              stop
          end if
          end if
!
!
!############################################################################################
!
!        compute mutual inductances between the various coils and the res-q can.  generate
!        spice input for these mutual inductances
!
!############################################################################################
!
          if (do_output) then
          write(12,'(''*'')')
          write(12,'(''*  mutual inductances between coils and can inductors'')')
          write(12,'(''*'')')
          end if
          do m = 1, number_of_resq_inductors
!
!       inductance between the transmit coil in the wand and this quad
!
              call mutual_ind_quad_cir_coil (transmit_coil%r, transmit_coil%origin%x,       &
                                         transmit_coil%origin%y, transmit_coil%origin%z,    &
                                         transmit_coil%h, transmit_coil%turns,              &
                                         wand_rotation_matrix, resq_inductors(m),           &
                                         theta_integral_quad_points, mu0, inductance)
!
              inductance = inductance / sqrt(transmit_coil%self_ind *                       &
                                                                  resq_inductors(m)%self_ind)
              if (abs(inductance) > 1.0_LONGreal) then
                  print *,"bad mutual inductance between quad ",m," and the wand ",        &
                                                                      "transmit coil, abort."
                  print *,"computed mutual inductance = ",inductance
                  stop
              end if
              min_cq_mutual_inductance = min(min_cq_mutual_inductance, abs(inductance))
              max_cq_mutual_inductance = max(max_cq_mutual_inductance, abs(inductance))
              no_of_wand_quad_mutuals = no_of_wand_quad_mutuals + 1
              if (abs(inductance) >= wq_mutual_inductance_cutoff) then
                no_of_wand_quad_mutuals_used = no_of_wand_quad_mutuals_used + 1
                if (do_output) then
                write(12,'(''kw_1_'',i10.10,'' lw_1 lq'',i10.10,''  '',1pe12.5)') m, m,      &
&                                                                                 inductance
                end if
              end if
!
!       inductance between the 1st receiver coil in the wand and this quad
!    
              call mutual_ind_quad_cir_coil (receive_coil_1%r, receive_coil_1%origin%x,     &
                                         receive_coil_1%origin%y, receive_coil_1%origin%z,  &
                                         receive_coil_1%h, receive_coil_1%turns,            &
                                         wand_rotation_matrix, resq_inductors(m),           &
                                         theta_integral_quad_points, mu0, inductance)
!
              inductance = inductance/sqrt(receive_coil_1%self_ind *                    &
                                                                 resq_inductors(m)%self_ind)
              if (abs(inductance) > 1.0_LONGreal) then
                   print *," "
                   print *,"bad mutual inductance between quad ",m," and the wand ",       &
                                                                     "receive coil 1, abort."
                   print *,"computed mutual inductance = ",inductance
                   print *," "
                   stop
              end if
              min_cq_mutual_inductance = min(min_cq_mutual_inductance, abs(inductance))
              max_cq_mutual_inductance = max(max_cq_mutual_inductance, abs(inductance))
              no_of_wand_quad_mutuals = no_of_wand_quad_mutuals + 1
              if (abs(inductance) >= wq_mutual_inductance_cutoff) then
                  no_of_wand_quad_mutuals_used = no_of_wand_quad_mutuals_used + 1
                  if (do_output) then
                  write(12,'(''kw_2_'',i10.10,'' lw_2 lq'',i10.10,''  '',1pe12.5)') m, m,    &
                                                                                 inductance
                  end if
              end if
!
!       inductance between the 2nd receiver coil in the wand and this quad
!
              call mutual_ind_quad_cir_coil (receive_coil_2%r, receive_coil_2%origin%x,     &
                                         receive_coil_2%origin%y, receive_coil_2%origin%z,  &
                                         receive_coil_2%h, receive_coil_2%turns,            &
                                         wand_rotation_matrix, resq_inductors(m),           &
                                         theta_integral_quad_points, mu0, inductance)
!
             inductance = inductance/sqrt(receive_coil_2%self_ind *                         &
                                                                  resq_inductors(m)%self_ind)
              if (abs(inductance) > 1.0_LONGreal) then
                   print *," "
                   print *,"bad mutual inductance between quad ",m," and the wand ",       &
                                                                     "receive coil 2, abort."
                   print *,"computed mutual inductance = ",inductance
                   print *," "
                   stop
              end if
              min_cq_mutual_inductance = min(min_cq_mutual_inductance, abs(inductance))
              max_cq_mutual_inductance = max(max_cq_mutual_inductance, abs(inductance))
              no_of_wand_quad_mutuals = no_of_wand_quad_mutuals + 1
              if (abs(inductance) >= wq_mutual_inductance_cutoff) then
                  no_of_wand_quad_mutuals_used = no_of_wand_quad_mutuals_used + 1
                  if (do_output) then
                  write(12,'(''kw_3_'',i10.10,'' lw_3 lq'',i10.10,''  '',1pe12.5)') m, m,    &
                                                                                   inductance
                  end if
              end if
!
!       inductance between the res-q coil and this quad
!
              if (.NOT. no_resq_coil) then
                  if (resq_coil%rectangular) then
                      call mutual_ind_quad_rec_coil (resq_coil%a, resq_coil%b,              &
                               resq_coil%origin%x, resq_coil%origin%y, resq_coil%origin%z,  &
                               resq_coil%h, resq_coil%turns, resq_rotation_matrix,          &
                               resq_inductors(m), theta_integral_quad_points, mu0,          &
                               inductance)
                  else
                      call mutual_ind_quad_cir_coil (resq_coil%r, resq_coil%origin%x,       &
                                         resq_coil%origin%y, resq_coil%origin%z,            &
                                         resq_coil%h, resq_coil%turns,                      &
                                         resq_rotation_matrix, resq_inductors(m),           &
                                         theta_integral_quad_points, mu0, inductance)
                  end if
!
                  inductance = inductance/sqrt(resq_coil%self_ind*resq_inductors(m)%self_ind)
                  if (abs(inductance) > 1.0_LONGreal) then
                       print *," "
                       print *,"bad mutual inductance between quad ",m," and the Res-Q ",   &
                                                                       "receive coil, abort."
                       print *,"computed mutual inductance = ",inductance
                       print *," "
                       stop
                  end if
                  min_rq_mutual_inductance = min(min_rq_mutual_inductance, abs(inductance))
                  max_rq_mutual_inductance = max(max_rq_mutual_inductance, abs(inductance))
                  no_of_resq_quad_mutuals = no_of_resq_quad_mutuals + 1
                  if (abs(inductance) >= rq_mutual_inductance_cutoff) then
                      no_of_resq_quad_mutuals_used = no_of_resq_quad_mutuals_used + 1
                      if (do_output) then
                      write(12,'(''kr_2_lq'',i10.10,'' lr_1 lq'',i10.10,''  '',1pe12.5)') m, &
                                                                                m, inductance
                      end if
                  end if
              end if
!
          end do
!
          call computer_time (computer_time_now)
          elapsed_computer_time = computer_time_now - computer_time_before
          computer_time_before = computer_time_now
          write(unit=*,fmt="(a,a,f7.3)") "cpu time to define res-q can/coil mutual ",       &
                                                      "inductances = ", elapsed_computer_time
      end if
!
!############################################################################################
!
!        print some statistics
!
!############################################################################################
!
!
      print *," "
      print *,"Number of theta quadrature points = ",theta_integral_quad_points
      if (.NOT. no_can) then
          min_resistance = minval(resq_inductors(:)%resistance)
          max_resistance = maxval(resq_inductors(:)%resistance)
          print *," "
          print *,"Minimum quad resistance = ",min_resistance
          print *,"Maximum quad resistance = ",max_resistance
          print *,"Minimum quad abs self-inductance = ",min_quad_self_ind
          print *,"Maximum quad abs self-inductance = ",max_quad_self_ind
          print *,"Minimum quad/quad abs rel mutual inductance = ",min_qq_mutual_inductance
          print *,"Maximum quad/quad abs rel mutual inductance = ",max_qq_mutual_inductance
          print *,"Minimum quad/quad abs mutual inductance = ",min_qq_real_mutuals
          print *,"Maximum quad/quad abs mutual inductance = ",max_qq_real_mutuals
          print *,"Minimum wand/quad abs rel mutual inductance = ",min_cq_mutual_inductance
          print *,"Maximum wand/quad abs rel mutual inductance = ",max_cq_mutual_inductance
          if (.NOT. no_resq_coil) then
              print *,"Minimum resq coil/quad abs rel mutual inductance = ",                &
                                                                    min_rq_mutual_inductance
              print *,"Maximum resq coil/quad abs rel mutual inductance = ",                &
                                                                    max_rq_mutual_inductance
          end if
          print *," "
          print *,"Number of inductors in res-q 3 can mesh = ",number_of_resq_inductors
          print *,"Number of mutual inductances calculated for res-q 3 can = ",             &
                                                                  number_of_quad_quad_mutuals
          print *,"Number of mutual inductances actually used for res-q 3 can = ",          &
                                                                 no_of_quad_quad_mutuals_used
          print *,"Number of wand/quad mutuals computed = ", no_of_wand_quad_mutuals
          print *,"Number of wand/quad mutuals actually used = ",no_of_wand_quad_mutuals_used
          if (.NOT. no_resq_coil) then
              print *,"Number of resq coil/quad mutuals computed = ", no_of_resq_quad_mutuals
              print *,"Number of resq coil/quad mutuals actually used = ",                  &
                                                                no_of_resq_quad_mutuals_used
          end if
!
          if (do_output) then
          write (12,'(''*'')')
          write (12,'(''*    Simulation statistics:'')')
          write (12,'(''*'')')
          write (12,'(''* Minimum quad resistance = '',1pe13.5)') min_resistance
          write (12,'(''* Maximum quad resistance = '',1pe13.5)') max_resistance
          write (12,'(''* Minimum quad abs self-inductance = '',1pe13.5)') min_quad_self_ind
          write (12,'(''* Maximum quad abs self-inductance = '',1pe13.5)') max_quad_self_ind
          write (12,'(''* Minimum quad/quad abs rel mutual inductance = '',1pe13.5)')                &
                                                                     min_qq_mutual_inductance
          write (12,'(''* Maximum quad/quad abs rel mutual inductance = '',1pe13.5)')                &
                                                                     max_qq_mutual_inductance
          write (12,'(''* Minimum quad/quad abs mutual inductance = '',1pe13.5)')            &
                                                                          min_qq_real_mutuals
          write (12,'(''* Maximum quad/quad abs mutual inductance = '',1pe13.5)')            &
                                                                          max_qq_real_mutuals
          write (12,'(''* Minimum wand/quad abs rel mutual inductance = '',1pe13.5)')        &
                                                                     min_cq_mutual_inductance
          write (12,'(''* Maximum wand/quad abs rel mutual inductance = '',1pe13.5)')        &
                                                                     max_cq_mutual_inductance
          if (.NOT. no_resq_coil) then
              write (12,'(''* Minimum resq coil/quad abs rel mutual inductance = '',         &
&                                                          1pe13.5)') min_rq_mutual_inductance
              write (12,'(''* Maximum resq coil/quad abs rel mutual inductance = '',         &
&                                                          1pe13.5)') max_rq_mutual_inductance
          end if
          write (12,'(''*'')')
          write (12,'(''*'')')
          write (12,'(''* Number of inductors in res-q 3 can mesh = '',i10)')                &
&                                                                     number_of_resq_inductors
          write (12,'(''* Number of mutual inductances calculated for res-q 3 can = '',i10)')&
&                                                                  number_of_quad_quad_mutuals
          write (12,'(''* Number of mutual inductances actually used for res-q 3 can = '',   &
&                                                          i10)') no_of_quad_quad_mutuals_used
          write (12,'(''* Number of wand/quad mutuals computed = '',i10)')                   &
                                                                      no_of_wand_quad_mutuals
          write (12,'(''* Number of wand/quad mutuals actually used = '',i10)')              &
                                                                 no_of_wand_quad_mutuals_used
          if (.NOT. no_resq_coil) then
              write (12,'(''* Number of resq coil/quad mutuals computed = '',i10)')          &
                                                                      no_of_resq_quad_mutuals
              write (12,'(''* Number of resq coil/quad mutuals actually used = '',i10)')     &
                                                                no_of_resq_quad_mutuals_used
          end if
          write (12,'(''*'')')
          write (12,'(''*'')')
      end if
      end if
!
      if (.NOT. no_resq_coil) deallocate (resq_inductors)
!
      close (unit=12)
!
      print *," "
!
      end subroutine induct
        program runit
!
!        Wrapper for benchmark codes to do timing
!
!
    implicit none
!
    integer, parameter :: LONGreal = selected_real_kind(15,90)

    real (kind = LONGreal) :: tstart, tend, tdelta
!
    call time_now(tstart)
        call induct
        call time_now(tend)
    tdelta = tend - tstart
!       open (unit = 99, file = "timing_info", form="formatted", status="unknown")
!       write (unit = 99, fmt = "(a,f9.3)") "Execution time for this program = ",tdelta
!       close (unit = 99)

contains

subroutine time_now (tnow)
!
    implicit none
!
    real (kind=LONGreal) :: tnow
    integer :: count, count_rate
!    real (kind=LONGreal) :: total_conformation_time, initial_conformation_time, final_conformation_time,     &
!&            percent_conformation_time
!    real (kind=LONGreal) :: total_energy_time, initial_energy_time, final_energy_time, percent_energy_time
!    real (kind=LONGreal) :: total_time, initial_time, final_time    
!
    call system_clock (count, count_rate)
    if (count_rate == 0) then
        print *,"no system clock, abort."
        stop
    else 
        tnow = real(count,LONGreal)/real(count_rate,LONGreal)
    end if
!
end subroutine time_now

end program runit
