program problem_22

    use aoc_utilities
    use iso_fortran_env, ip => int64, wp => real64

    implicit none

    integer :: i, j, k, iunit, n_lines, n_rows, n_cols, x, y, facing
    character(len=:),allocatable :: line, direction_string
    character(len=1),dimension(:,:),allocatable :: grid

    character(len=*),parameter :: input = 'inputs/day22.txt' ! file to read

    open(newunit=iunit,file=input,status='OLD')
    n_lines = number_of_lines_in_file(iunit)
    n_rows = n_lines - 2

    ! first, get the number of cols in the grid (max line length):
    n_cols = 0
    do i = 1, n_lines
        line = read_line(iunit)
        if (line=='') exit ! end of grid
        if (len(line)>n_cols) n_cols = len(line)
    end do
    allocate(grid(n_rows, n_cols))
    grid = ' '

    ! now read it in:
    rewind(iunit)
    do i = 1, n_lines
        line = read_line(iunit)
        if (line=='') then
            direction_string = read_line(iunit)
            exit
        else
            ! read the grid:
            do j = 1, len(line)
                grid(i,j) = line(j:j)
            end do
        end if
    end do
    close(iunit)

    ! first the starting x (leftmost open tile of the top row of tiles)
    do x = 1, n_cols
        if (grid(1,x)=='.') exit
    end do
    y = 1 ! first row
    facing = 0 ! right

    call go(x,y,facing)

    ! The final password is the sum of 1000 times the row, 4 times the column, and the facing
    write(*,*) '22a: ', 1000*y + 4*x + facing

    contains

    subroutine go(x,y,facing)
        integer,intent(inout) :: x,y  ! initial point
        integer,intent(inout) :: facing  ! Facing is
                                         ! 0 for right (>)
                                         ! 1 for down (v)
                                         ! 2 for left (<)
                                         ! 3 for up (^)
        integer :: xmin,xmax,ymin,ymax, tx,ty
        integer :: i, imove
        character(len=:),allocatable :: str
        character(len=1) :: c

        ! A number indicates the number of tiles to move in the direction you are facing.
        ! If you run into a wall, you stop moving forward and continue with the next instruction.
        !
        ! A letter indicates whether to turn 90 degrees clockwise (R) or counterclockwise (L).
        ! Turning happens in-place; it does not change your current tile.

        i = 1
        str = ''
        do
            if (i>len(direction_string)) exit ! done
            c = direction_string(i:i)
            if (c=='L') then ! rotate left
                facing = modulo(facing-1,4)
                str = ''
            else if (c=='R') then ! rotate right
                facing = modulo(facing+1,4)
                str = ''
            else
                str = str//c
                ! accumulate a number (until we hit an R or L or end of string)
                do
                    if (i+1>len(direction_string)) exit ! done
                    if (direction_string(i+1:i+1)=='R' .or. &
                        direction_string(i+1:i+1)=='L') exit ! done
                    i = i + 1
                    str = str//direction_string(i:i)
                end do
                imove = int(str)
                str = ''

                call get_bounds(x,y,xmin,xmax,ymin,ymax)

                ! now move:
                do j = 1, imove
                    tx = x
                    ty = y
                    select case (facing)
                    case(0); tx = x+1; if (tx>xmax) tx = xmin ! right
                    case(1); ty = y+1; if (ty>ymax) ty = ymin ! down
                    case(2); tx = x-1; if (tx<xmin) tx = xmax ! left
                    case(3); ty = y-1; if (ty<ymin) ty = ymax ! up
                    end select
                    if (grid(ty,tx)=='#') exit ! hit a wall, stop this turn
                    x = tx
                    y = ty
                end do
            end if
            i = i + 1
        end do

    end subroutine go

    subroutine get_bounds(x,y,xmin,xmax,ymin,ymax)
        integer,intent(in) :: x,y ! current point
        integer,intent(out) :: xmin,xmax,ymin,ymax
        integer :: i ! counter
        i = x
        do
            if (i-1<1) exit
            if (grid(y,i-1)==' ') exit
            i = i - 1
        end do
        xmin = i
        i = x
        do
            if (i+1>n_cols) exit
            if (grid(y,i+1)==' ') exit
            i = i + 1
        end do
        xmax = i
        i = y
        do
            if (i-1<1) exit
            if (grid(i-1,x)==' ') exit
            i = i - 1
        end do
        ymin = i
        i = y
        do
            if (i+1>n_rows) exit
            if (grid(i+1,x)==' ') exit
            i = i + 1
        end do
        ymax = i
    end subroutine get_bounds

end program problem_22