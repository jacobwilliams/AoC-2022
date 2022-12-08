program problem_6

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer :: i, iunit
    character(len=:),allocatable :: line

    open(newunit=iunit, file='inputs/day6.txt', status='OLD')
    line = read_line(iunit)
    close(iunit)

    i = go(4);  write(*,*) '6a: number characters processed', i
    i = go(14); write(*,*) '6b: number characters processed', i

    contains

    function go(step) result(i)
    integer,intent(in) :: step
    integer :: i
    integer :: j, k, n
    n = len(line)
    main : do i = step, n
        do j = i-(step-1), i
            do k = i-(step-1), i
                if (j==k) cycle ! skip this
                if (line(j:j)==line(k:k)) cycle main ! not it
            end do
        end do
        exit ! found it
    end do main
    end function go

end program problem_6