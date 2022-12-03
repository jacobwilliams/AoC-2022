program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

integer :: i, j, k, iunit, priorities, n_lines, n
character(len=:),allocatable :: line, first, second, line1, line2, line3
character(len=1) :: c
logical :: ok
character(len=52),parameter :: items='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

priorities = 0
open(newunit=iunit, file='inputs/day3.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
do i = 1, n_lines
    call read_line_from_file(iunit,line,ok); if (.not. ok) error stop
    n = len(line)/2
    first  = line(1:n)
    second = line(n+1:)
    main: do j = 1, n
        do k = 1, n
            if (first(j:j) == second(k:k)) then
                priorities = priorities + index(items,first(j:j))
                exit main
            end if
        end do
    end do main
end do
close(iunit)
write(*,*) '3a: ', priorities

priorities = 0
open(newunit=iunit, file='inputs/day3.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
i = 0
do
    i=i+1; call read_line_from_file(iunit,line1,ok); if (.not. ok) error stop
    i=i+1; call read_line_from_file(iunit,line2,ok); if (.not. ok) error stop
    i=i+1; call read_line_from_file(iunit,line3,ok); if (.not. ok) error stop
    n = len(line1)
    do j = 1, n
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