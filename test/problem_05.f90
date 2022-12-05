program problem_5

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer,parameter :: n_stacks = 9

    integer :: i, j, n, iunit, n_lines, num_to_move, move_from, move_to, n_from
    character(len=:),allocatable :: line
    type(string),dimension(n_stacks) :: stacks, stacks2, vals

    !     [M]             [Z]     [V]
    !     [Z]     [P]     [L]     [Z] [J]
    ! [S] [D]     [W]     [W]     [H] [Q]
    ! [P] [V] [N] [D]     [P]     [C] [V]
    ! [H] [B] [J] [V] [B] [M]     [N] [P]
    ! [V] [F] [L] [Z] [C] [S] [P] [S] [G]
    ! [F] [J] [M] [G] [R] [R] [H] [R] [L]
    ! [G] [G] [G] [N] [V] [V] [T] [Q] [F]
    !  1   2   3   4   5   6   7   8   9

    ! just type them:
    stacks(1)%str = 'GFVHPS'
    stacks(2)%str = 'GJFBVDZM'
    stacks(3)%str = 'GMLJN'
    stacks(4)%str = 'NGZVDWP'
    stacks(5)%str = 'VRCB'
    stacks(6)%str = 'VRSMPWLZ'
    stacks(7)%str = 'THP'
    stacks(8)%str = 'QRSNCHZV'
    stacks(9)%str = 'FLGPVQJ'
    stacks2 = stacks

    open(newunit=iunit, file='inputs/day5.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    do i = 1, n_lines

        ! get star/end of each pair as integers:
        line = read_line(iunit)    ! move 1 from 2 to 1
        if (index(line, 'move')>0) then
            vals = split(line,' ')
            num_to_move = int(vals(2))
            move_from = int(vals(4))
            move_to = int(vals(6))

            ! part a:
            do j = 1, num_to_move
                n_from = len(stacks(move_from)%str)
                stacks(move_to)%str   = stacks(move_to)%str // stacks(move_from)%str(n_from:n_from)
                stacks(move_from)%str = stacks(move_from)%str(1:n_from-1)
            end do

            ! part b:
            n_from = len(stacks2(move_from)%str)
            stacks2(move_to)%str = stacks2(move_to)%str // stacks2(move_from)%str(n_from-num_to_move+1:n_from)
            stacks2(move_from)%str = stacks2(move_from)%str(1:n_from-num_to_move)

        end if
    end do
    close(iunit)

    write(*,'(A)',advance='NO') '5a: '
    do i = 1, n_stacks
        n = len(stacks(i)%str)
        write(*,'(A)',advance='NO') stacks(i)%str(n:n)
    end do
    write(*,*) ''

    write(*,'(A)',advance='NO') '5b: '
    do i = 1, n_stacks
        n = len(stacks2(i)%str)
        write(*,'(A)',advance='NO') stacks2(i)%str(n:n)
    end do
    write(*,*) ''

end program problem_5