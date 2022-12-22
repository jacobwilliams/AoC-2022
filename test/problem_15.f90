
program problem_15

    use aoc_utilities
    use iso_fortran_env, ip => int64

    implicit none

    integer :: iunit, n_rows
    integer(ip) :: i,j,k
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals, v
    integer(ip),dimension(:),allocatable :: sx, sy, bx, by
    integer(ip) :: m, delta !, minx,maxx
    integer(ip),dimension(2) :: minmax
    integer(ip),dimension(:,:),allocatable :: minmax2
    logical,dimension(:),allocatable :: irow
    integer(ip),dimension(:,:),allocatable :: minmax_all
    integer,dimension(:),allocatable :: ib
    integer(ip) :: ilow, ihigh
    integer(ip) :: xmin, xmax, ymin, ymax, im

    ! test data:
    ! integer(ip),parameter      :: ytarg = 10 ! for test data
    ! character(len=*),parameter :: input = 'inputs/day15-test.txt'
    ! integer(ip),parameter      :: maxb  = 20 ! max size for part b

    integer(ip),parameter      :: ytarg = 2000000
    character(len=*),parameter :: input = 'inputs/day15.txt'
    integer(ip),parameter      :: maxb  = 4000000 ! max size for part b

    open(newunit=iunit,file=input,status='OLD')
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

    minmax = compute_minmax_for_row(ytarg)
    ! note: have to exclude the beacons on this row:
    write(*,*) '15a: ', 1 + minmax(2) - minmax(1) - size(unique(pack(bx, mask=by==ytarg)))

    ! 15b works, but takes a long time!
    ! so it is commented out here for the CI.
    !
    ! row               3231000   80.7750015     %
    ! 15b:        13172087230812

    stop !!!!!

    call compute_board_bounds(xmin, xmax, ymin, ymax)
    xmin = max(0,xmin)    ! reduced search space
    xmax = min(maxb,xmax)

    ! will hold [min,max] for each sensor on each row
    allocate(minmax_all(1:n_rows,2))
    allocate(irow(xmin:xmax)) ! for each row to check

    ! row loop:
    main : do i = min(maxb,ymax),0,-1 ! loop backwards
        minmax_all = compute_minmax_for_all_sensors_on_row(i)

        if (mod(i,1000)==0) write(*,*) 'row ',i, 100.0*i/min(maxb,ymax),'%'

        ! the excluded ones will be set to false,
        ! if any remains true, that is the answer
        irow = .true.

        do j = 1, n_rows

            associate (low => minmax_all(j,1), high => minmax_all(j,2))
                !    xmin     xmax
                !     *--------*
                ! [ ]
                !                 []
                !  [     ]
                !             [     ]
                !       [   ]
                if (low<xmin .and. high<xmin) then
                    cycle
                else if (low>xmax .and. high>xmax) then
                    cycle
                else if (low<xmin .and. high<xmax) then
                    irow(:high) = .false.
                else if (low<xmax .and. high>xmax) then
                    irow(low:) = .false.
                else
                    irow(low:high) = .false.
                end if
            end associate

        end do
        if (any(irow)) then
            ! exclude known beacons
            do j = 1, size(bx)
                if (by(j)==i) then
                    if (bx(j)>=xmin .and. bx(j)<=xmax) irow(bx(j)) = .false.
                end if
            end do
            if (any(irow)) then
                do j = lbound(irow,dim=1), ubound(irow,dim=1)
                    if (irow(j)) then
                        write(*,*) '15b: ', 4000000_ip*j + i
                        exit main
                    end if
                end do
            end if
        end if
    end do main

    contains

    subroutine compute_board_bounds(xmin, xmax, ymin, ymax)
        integer(ip),intent(out) :: xmin, xmax, ymin, ymax
        integer :: i
        integer(ip) :: m, delta
        xmin = huge(1)
        xmax = -huge(1)
        ymin = huge(1)
        ymax = -huge(1)
        do i = 1, n_rows
            m = abs(sx(i) - bx(i)) + abs(sy(i) - by(i)) ! Manhattan distance to beacon
            if (sx(i)-m < xmin) xmin = sx(i)-m
            if (sy(i)-m < ymin) ymin = sy(i)-m
            if (sx(i)+m > xmax) xmax = sx(i)+m
            if (sy(i)+m > ymax) ymax = sy(i)+m
        end do
    end subroutine compute_board_bounds

    function compute_minmax_for_all_sensors_on_row(ir) result(minmax)
        integer(ip),intent(in) :: ir ! row number
        integer(ip),dimension(:,:),allocatable :: minmax
        integer(ip) :: m, delta, k, i

        allocate(minmax(1:n_rows,2))
        minmax = 0_ip
        do i = 1, n_rows
            m = abs(sx(i) - bx(i)) + abs(sy(i) - by(i)) ! Manhattan distance to beacon
            delta = abs(sy(i) - ir)
            if (delta>m) cycle  ! skip if we know it doesn't intersect
            k = m - delta ! how many +/- on row ir
            minmax(i,1) = sx(i)-k ! min
            minmax(i,2) = sx(i)+k ! max
        end do
    end function compute_minmax_for_all_sensors_on_row

    function compute_minmax_for_row(ir) result(minmax)
        integer(ip),intent(in) :: ir ! row number
        integer(ip),dimension(2) :: minmax
        integer(ip) :: minx,maxx,i
        integer(ip),dimension(:,:),allocatable :: minmax_all

        allocate(minmax_all(1:n_rows,2))
        minmax_all = compute_minmax_for_all_sensors_on_row(ir)
        minx = huge(1_ip)
        maxx = -huge(1_ip)
        do i = 1, n_rows
            if (minmax_all(i,1)<minx) minx = minmax_all(i,1)  ! get the min/max range on this row
            if (minmax_all(i,2)>maxx) maxx = minmax_all(i,2)
        end do
        minmax = [minx,maxx]
    end function compute_minmax_for_row

end program problem_15
