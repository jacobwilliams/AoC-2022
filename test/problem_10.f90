program problem_10

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer :: i, j, iunit, n_lines, x, icycle, signal_strength, signal_strength_sum
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals
    character(len=1),dimension(6,0:39) :: crt
    integer :: ipixel  ! location of pixel
    integer :: irow    ! row we are currently on

    open(newunit=iunit, file='inputs/day10.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    signal_strength_sum = 0
    icycle = 0 ! first cycle start
    x = 1 ! start
    irow = 1
    crt = '.'
    ipixel = 0
    do i = 1, n_lines
        line = read_line(iunit)
        vals = split(line,' ')

        select case (vals(1)%str)
        case ('noop')
            icycle = icycle + 1 ! start of cycle
            call check()
            call update()
        case ('addx')
            icycle = icycle + 1 ! start of cycle
            call check()
            call update()
            icycle = icycle + 1 ! start of cycle
            call check()
            call update()
            x = x + int(vals(2))
        end select

    end do
    close(iunit)

    write(*,*) '10a : ', signal_strength_sum

    write(*,*) '10b : '
    do i = 1, 6
        write(*,'(*(A1))') crt(i,:)
    end do

    contains

    subroutine check() ! for part a
        if (any(icycle==[20, 60, 100, 140, 180, 220])) then
            signal_strength = icycle * x
            signal_strength_sum = signal_strength_sum + signal_strength
        end if
    end subroutine check

    subroutine update() ! for part b
        if (any(ipixel==[x-1,x,x+1])) crt(irow,ipixel) = '#'
        ipixel = ipixel + 1
        if (ipixel>39) then
            irow = irow + 1
            ipixel = 0
        end if
    end subroutine update

end program problem_10