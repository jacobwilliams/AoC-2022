program problem_14

    use aoc_utilities
    use iso_fortran_env

    implicit none

    integer :: i, j, iunit, n_lines
    character(len=1),dimension(:,:),allocatable :: grid
    integer,dimension(2,2) :: grid_size
    integer :: xmin,xmax,ymin,ymax,x,y,tx,ty

    integer,parameter :: xsource = 500
    integer,parameter :: ysource = 0
    integer,parameter :: floor_size = 1000 ! for part b, just make a big floor
    character(len=*),parameter :: input = 'inputs/day14.txt' ! file to read

    ! populate the grid
    ! call it twice. not efficient, but who cares?
    call read_file(.false., .true.)
    allocate(grid(ymin:ymax, xmin:xmax))
    call read_file(.false., .false.)
    !call print_grid('initial')
    call go()
    !call print_grid('final')
    write(*,*) '14a: ', i

    ! for part b, just make a big floor:
    call read_file(.true., .true.)
    allocate(grid(ymin:ymax, xmin:xmax))
    call read_file(.true., .false.)
    call go()
    write(*,*) '14b: ', i

    contains

    subroutine go()

    ! run the simulation:
    i = 0 ! units of sand created
    main : do
        ! new sand particle:
        x = xsource; tx = x
        y = ysource; ty = y
        fall : do

            ty = ty + 1 ! provisional move

            if (in_void(tx,ty)) then
                exit main ! this one will fall in the void. done.
            elseif (grid(ty,tx)=='.') then ! free to fall
                y = ty
                cycle fall
            else if (grid(ty,tx)=='#' .or. grid(ty,tx)=='o') then
                if (in_void(tx-1,ty)) exit main
                if (grid(ty,tx-1)=='.') then  ! try left
                    tx = tx-1 ! ok to move left
                    y = ty
                    x = tx
                    cycle fall
                end if
                if (in_void(tx+1,ty)) exit main
                if (grid(ty,tx+1)=='.') then ! try right
                    tx = tx+1 ! ok to move right
                    y = ty
                    x = tx
                    cycle fall
                end if
                if (grid(ty,tx)=='#' .or. grid(ty,tx)=='o') then
                    ty = ty - 1  ! can't fall in this case, go back up
                end if
                exit fall ! nowhere to move, stops
            end if
            x = tx  ! update location and continue
            y = ty
        end do fall

        i = i + 1
        grid(y,x) = 'o' ! final location of this particle

        if (x==xsource .and. y==ysource) return ! source blocked (part b)

    end do main

    end subroutine go

    subroutine print_grid(name)
        character(len=*),intent(in) :: name
        integer :: i
        write(*,*) ''
        write(*,*) trim(name)//' grid:'
        do i = lbound(grid,1), ubound(grid,1)
            write(*,'(*(A1))') grid(i,:)
        end do
        write(*,*) ''
    end subroutine print_grid

    logical function in_void(x,y)
        integer,intent(in) :: x,y
        if (x<xmin .or. x>xmax .or. y>ymax) then
            in_void = .true.
        elseif (y==ymax .and. grid(y,x)=='.') then
            in_void = .true.
        else
            in_void = .false.
        end if
    end function in_void

    subroutine read_file(add_floor,size_only)
        logical,intent(in) :: add_floor
        logical,intent(in),optional :: size_only ! if true, only compute grid size. if false, populate grid
        character(len=:),allocatable :: line
        type(string),dimension(:),allocatable :: vals, start_pair, end_pair
        logical :: return_size, return_grid
        integer :: i, j, k, m, istep, jstep
        integer,dimension(2) :: istart, iend ! x,y of start and end points

        return_size = size_only
        return_grid = .not. size_only

        if (return_size) then
            xmin = huge(1); xmax = -huge(1)
            ymin = huge(1); ymax = -huge(1)
            if (allocated(grid)) deallocate(grid)
        end if
        if (return_grid) grid = '.'

        open(newunit=iunit,file=input,status='OLD')
        n_lines = number_of_lines_in_file(iunit)
        do i = 1, n_lines
            line = read_line(iunit)
            vals = split(line, ' -> ')
            do j = 1, size(vals)-1 ! beginning:end pair
                start_pair = split(vals(j),  ',')
                end_pair   = split(vals(j+1),',')
                do k = 1, 2
                    istart(k) = int(start_pair(k)) ! x,y
                    iend(k)   = int(end_pair(k))   ! x,y
                end do
                if (return_size) then
                    m = minval([istart(1),iend(1)]); if (m<xmin) xmin = m
                    m = maxval([istart(1),iend(1)]); if (m>xmax) xmax = m
                    m = minval([istart(2),iend(2)]); if (m<ymin) ymin = m
                    m = maxval([istart(2),iend(2)]); if (m>ymax) ymax = m
                end if
                if (return_grid) then
                    istep = sign(1,iend(2)-istart(2))
                    jstep = sign(1,iend(1)-istart(1))
                    grid(istart(2):iend(2):istep, istart(1):iend(1):jstep) = '#'
                end if
            end do
        end do
        if (return_size) then
            if (ymin>ysource) ymin = ysource
            if (add_floor) then
                ymax = ymax + 2
                xmin = xmin - floor_size
                xmax = xmax + floor_size
            end if
        end if
        if (return_grid) then
            grid(ysource, xsource) = '+'
            if (add_floor) grid(ymax, xmin:xmax) = '#'
        end if
        close(iunit)
    end subroutine read_file

end program problem_14