C RUN: %dragonegg -S %s -o - | FileCheck %s
        subroutine sincos(nx, ny)
C CHECK: (i32* noalias %nx, i32* noalias %ny)
        
        implicit none
        
        integer, intent(inout) :: nx
        integer, intent(in) :: ny
        
        nx = nx + ny
        nx = nx + ny
        
        end subroutine sincos
