program problem_12

    use aoc_utilities
    use iso_fortran_env, ip => int64

    !! see: https://en.wikipedia.org/wiki/Dijkstra's_algorithm#Pseudocode
    !!
    !! based on previous year's day 15 problem: https://github.com/jacobwilliams/AoC-2021/blob/master/test/problem_15.f90

    implicit none

    type :: pair
        integer,dimension(2) :: ij = -1
    end type pair

    integer :: iunit, n_rows, n_cols, min_res, res
    integer :: i, j, n_steps
    character(len=:),allocatable :: line
    integer,dimension(:,:),allocatable :: map
    logical,dimension(:,:),allocatable :: visited
    integer,dimension(:,:),allocatable :: dist
    type(pair),dimension(:,:),allocatable :: prev
    integer,dimension(2) :: iloc
    integer,dimension(2) :: istart, iend ! row, col of start and end points
    character(len=1),dimension(:),allocatable :: c

    open(newunit=iunit,file='inputs/day12.txt', status='OLD')
    n_rows = number_of_lines_in_file(iunit)
    n_steps = 0
    do i = 1, n_rows
        line = read_line(iunit)
        if (i==1) then
            n_cols = len(line)
            if (allocated(map))     deallocate(map); allocate(map(n_rows,n_cols))
            if (allocated(visited)) deallocate(visited); allocate(visited(n_rows,n_cols))
            if (allocated(dist))    deallocate(dist); allocate(dist(n_rows,n_cols))
            if (allocated(prev))    deallocate(prev); allocate(prev(n_rows,n_cols))
            if (allocated(c))       deallocate(c); allocate(c(n_cols))
        end if
        read(line,'(*(A1))') c
        map(i,1:n_cols) = iachar(c)
        ! look for beginning/end:
        do j = 1, n_cols
            if (c(j)=='S') then
                istart = [i,j]
                map(i,j) = iachar('a') - 1
            else if (c(j)=='E') then
                iend = [i,j]
                map(i,j) = iachar('z') + 1
            end if
        end do
    end do

    call go(); write(*,*) '12a: ', dist(iend(1), iend(2))

    ! call for all the 'a' cells:
    min_res = huge(1)
    map(istart(1), istart(2)) = iachar('a')
    do i = 1, n_rows
        do j = 1, n_cols
            if (map(i,j)==iachar('a')) then
                istart = [i,j] ! new starting point
                call go()
                res = dist(iend(1), iend(2))
                if (res>0 .and. res < min_res) min_res = res
            end if
        end do
    end do
    write(*,*) '12b: ', min_res

    contains

    subroutine go()

        integer :: i,j

        dist = huge(1)
        visited = .false.
        dist(istart(1),istart(2)) = 0

        do

            iloc = minloc(dist, mask=.not. visited)
            i = iloc(1)
            j = iloc(2)
            visited(i,j) = .true.

            call check([i,j], [i+1,j])
            call check([i,j], [i,j+1])
            call check([i,j], [i-1,j])
            call check([i,j], [i,j-1])

            if (all(visited)) exit ! done

        end do

    end subroutine go

    subroutine check(u, v)
        implicit none
        integer,dimension(2),intent(in) :: u ! current
        integer,dimension(2),intent(in) :: v ! neighbor
        integer :: alt, delt

        if (v(1)<1 .or. v(2)<1 .or. v(1)>n_rows .or. v(2)>n_cols) return
        if (visited(v(1),v(2))) return ! already visited this one

        delt = map(v(1),v(2)) - map(u(1),u(2)) ! step size

        if (delt>1) return ! can't go uphill
        delt = 1 ! every step counts as 1

        alt = dist(u(1),u(2)) + delt

        if (alt < dist(v(1),v(2))) then
            dist(v(1),v(2)) = alt
            prev(v(1),v(2)) = pair(u)
        end if

    end subroutine check

end program problem_12
