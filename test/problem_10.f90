program problem_10

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer :: i, j, iunit, n_lines, v, x, icycle, signal_strength, signal_strength_sum
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals

    open(newunit=iunit, file='inputs/day10.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    signal_strength_sum = 0
    icycle = 0 ! first cycle start
    x = 1 ! start
    do i = 1, n_lines
        line = read_line(iunit)

        vals = split(line,' ')

        select case (vals(1)%str)
        case ('noop')
            icycle = icycle + 1 ! start of cycle
            if (any(icycle==[20, 60, 100, 140, 180, 220])) then
                signal_strength = icycle * x
                signal_strength_sum = signal_strength_sum + signal_strength
                !write(*,*) 'cycle', icycle, ' : signal_strength=', signal_strength
            end if
        case ('addx')
            icycle = icycle + 1 ! start of cycle
            if (any(icycle==[20, 60, 100, 140, 180, 220])) then
                signal_strength = icycle * x
                signal_strength_sum = signal_strength_sum + signal_strength
                !write(*,*) 'cycle', icycle, ' : signal_strength=', signal_strength
            end if
            icycle = icycle + 1 ! start of cycle
            if (any(icycle==[20, 60, 100, 140, 180, 220])) then
                signal_strength = icycle * x
                signal_strength_sum = signal_strength_sum + signal_strength
                !write(*,*) 'cycle', icycle, ' : signal_strength=', signal_strength
            end if
            v = int(vals(2))
            x = x + v
        end select

    end do
    close(iunit)

    write(*,*) '10a : ', signal_strength_sum

end program problem_10