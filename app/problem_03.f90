program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

integer :: i, j, iunit, priorities, n_lines, n
character(len=:),allocatable :: line, line1, line2, line3, first, second
character(len=1) :: c
character(len=52),parameter :: items='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

priorities = 0
open(newunit=iunit, file='inputs/day3.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
do i = 1, n_lines
    line = read_line(iunit)
    n = len(line)/2
   ! associate (first => line(1:n), second => line(n+1:)) ! compiler bug in ifort
        first = line(1:n); second = line(n+1:)            ! just do this
        do j = 1, n
            c = first(j:j) ! check this character
            if (index(second,c)>0)  then
                priorities = priorities + index(items,c)
                exit
            end if
        end do
   ! end associate
end do
close(iunit)
write(*,*) '3a: ', priorities

priorities = 0
open(newunit=iunit, file='inputs/day3.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
i = 0
do
    line1 = read_line(iunit); i=i+1
    line2 = read_line(iunit); i=i+1
    line3 = read_line(iunit); i=i+1
    do j = 1, len(line1)
        c = line1(j:j) ! check this character
        if (index(line2,c)>0 .and. index(line3,c)>0 )  then
            priorities = priorities + index(items,c)
            exit
        end if
    end do
    if (i==n_lines) exit ! done
end do
close(iunit)
write(*,*) '3b: ', priorities

end program problem_3