implicit none
complex(kind=8) x(2) 
complex a(2,2)
character*4 z
character z1(4)
character*4 z2(2,2)
character*80 line
integer i
logical l
real r
character*8 c


call test (8h   hello)
end

subroutine test (h)
integer(kind=8) h
character*80 line

write (line, '(8a)') h
if (line .ne. '   hello') call abort
end subroutine

