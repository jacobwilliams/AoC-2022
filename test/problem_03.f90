program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

integer :: i, j, k, iunit, priorities, n_lines, n
character(len=:),allocatable :: line, first, second
logical :: ok, found
character(len=52),parameter :: items='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

priorities = 0
open(newunit=iunit, file='inputs/day3.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
do i = 1, n_lines
    call read_line_from_file(iunit,line,ok); if (.not. ok) error stop
    n = len(line)/2
    first  = line(1:n)
    second = line(n+1:)

    !write(*,*) first, ' ', second

    ! find common one:
    found = .false.
    main: do j = 1, n
        do k = 1, n
            if (first(j:j) == second(k:k)) then
                write(*,*) index(items,first(j:j))
                priorities = priorities + index(items,first(j:j))
                found = .true.
                exit main
            end if
        end do
    end do main
    if (.not. found) error stop 'common item not found'

end do

write(*,*) '3a: ', priorities

end program problem_3