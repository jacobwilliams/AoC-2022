program problem_6

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer :: i, j, k, n, iunit
    character(len=:),allocatable :: line

    open(newunit=iunit, file='inputs/day6.txt', status='OLD')
    line = read_line(iunit)
    close(iunit)
    n = len(line)

    main : do i = 4, n
        do j = i-3, i
            do k = i-3, i
                if (j==k) cycle ! skip this
                if (line(j:j)==line(k:k)) then
                    cycle main ! not it
                end if
            end do
        end do
        exit
    end do main
    write(*,*) '6a: number characters processed', i

    main2 : do i = 14, n
        do j = i-13, i
            do k = i-13, i
                if (j==k) cycle ! skip this
                if (line(j:j)==line(k:k)) then
                    cycle main2 ! not it
                end if
            end do
        end do
        exit
    end do main2
    write(*,*) '6b: number characters processed', i

end program problem_6