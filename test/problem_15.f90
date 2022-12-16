
program problem_15

    use aoc_utilities
    use iso_fortran_env, ip => int64

    implicit none

    integer :: iunit, n_rows
    integer(ip) :: i, k
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals, v
    integer(ip),dimension(:),allocatable :: sx, sy, bx, by
    integer(ip) :: m, delta, minx,maxx

   !integer,parameter :: ytarg = 10 ! for test data
    integer(ip),parameter :: ytarg = 2000000

    open(newunit=iunit,file='inputs/day15.txt', status='OLD')
    n_rows = number_of_lines_in_file(iunit)
    allocate(sx(n_rows))
    allocate(sy(n_rows))
    allocate(bx(n_rows))
    allocate(by(n_rows))

    do i = 1, n_rows
        ! get sensor and beacon coords:
        line = read_line(iunit)
        vals = split(line,' ')
        v = split(vals(3),'=');  sx(i) = int(v(2)%str(1:len(v(2)%str)-1))
        v = split(vals(4),'=');  sy(i) = int(v(2)%str(1:len(v(2)%str)-1))
        v = split(vals(9),'=');  bx(i) = int(v(2)%str(1:len(v(2)%str)-1))
        v = split(vals(10),'='); by(i) = int(v(2))
    end do

    ! ... this assumes the exclusion region on the row is continuous
    !     (minus the known beacons). this is not necessarily true, but
    !     seems to work for part a

    minx = huge(1)
    maxx = -huge(1)
    do i = 1, n_rows
        m = abs(sx(i) - bx(i)) + abs(sy(i) - by(i)) ! Manhattan distance to beacon
        delta = abs(sy(i) - ytarg)
        if (delta>m) cycle  ! skip if we know it doesn't intersect
        k = m - delta ! how many +/- on row ytarg
        if (sx(i)-k<minx) minx = sx(i)-k  ! get the min/max range on this row
        if (sx(i)+k>maxx) maxx = sx(i)+k
    end do
    ! exclude the beacons on this row:
    write(*,*) '15a: ', 1 + maxx - minx - size(unique(pack(bx, mask=by==ytarg)))

end program problem_15
