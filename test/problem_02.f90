program problem_2

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit, istat, score
character(len=3) :: str
character(len=1) :: x,y

open(newunit=iunit, file='inputs/day2.txt', status='OLD')

score = 0
do
    read(iunit,'(A3)', iostat=istat) str
    if (istat==iostat_end) exit

    x = str(1:1)
    y = str(3:3)

    select case (y)
    case('X') !rock
        score = score + 1
    case('Y') ! paper
        score = score + 2
    case('Z') ! sissors
        score = score + 3
    end select

    select case (x)
    case('A') !rock
        if (y=='X') then
            score = score + 3 !draw
        elseif (y=='Y') then
            score = score + 6 !win
        elseif (y=='Z') then
            score = score + 0 !lost
        end if
    case('B') ! paper
        if (y=='X') then
            score = score + 0 !lost
        elseif (y=='Y') then
            score = score + 3 !draw
        elseif (y=='Z') then
            score = score + 6 !win
        end if
    case('C') ! sissors
        if (y=='X') then
            score = score + 6 !win
        elseif (y=='Y') then
            score = score + 0 !lost
        elseif (y=='Z') then
            score = score + 3 !draw
        end if
    end select

end do
write(*,*) '2a: score = ', score

end program problem_2