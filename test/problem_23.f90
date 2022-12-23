program problem_23

    use aoc_utilities
    use iso_fortran_env

    implicit none

    character(len=*),parameter :: input = 'inputs/day23.txt' ! file to read

    integer :: i, j ! counters
    integer :: n_elves
    integer,dimension(:),allocatable :: prev_x, prev_y ! previous elf coordinates
    integer,dimension(:),allocatable :: x, y ! elf coordinates
    integer,dimension(:),allocatable :: proposed_x, proposed_y ! proposed elf coordinates
    logical,dimension(:),allocatable :: stuck ! the ones that won't move during this round
    integer,dimension(4) :: proposals
    logical :: found
    integer :: xmin,xmax,ymin,ymax,iempty,ix,iy

    call go(10)
    write(*,*) '23a: ', iempty

    ! comment this out for the CI since it is a bit slow !!
    !call go(huge(1))

    contains

    subroutine go(max_rounds)

    integer,intent(in) :: max_rounds

    call read_file(x,y) ! get original coordinates

    ! write(*,*) 'initial grid: '
    ! call print_grid()

    prev_x = huge(1)
    prev_y = huge(1)
    proposals = [1,2,3,4] ! n, s, w, e

    do i = 1, max_rounds ! rounds
        stuck = .false. ! initialize
        call get_proposed_coords()
        do j = 1, n_elves
            if (stuck(j)) cycle
            ! Simultaneously, each Elf moves to their proposed destination tile
            ! if they were the only Elf to propose moving to that position.
            if (count(proposed_x==proposed_x(j) .and. proposed_y==proposed_y(j) .and. .not. stuck)==1) then
                !write(*,*) 'move elf ', j, ' : ', x(j), y(j), ' -> ', proposed_x(j), proposed_y(j)
                x(j) = proposed_x(j)
                y(j) = proposed_y(j)
            end if
        end do
        proposals = [proposals(2:4), proposals(1)] ! permute for next round

        if (all(x==prev_x) .and. all(y==prev_y)) then
            write(*,*) '23b: ', i
            stop
        end if

        prev_x = x
        prev_y = y

        ! write(*,*) ''
        ! write(*,*) 'round ', i
        ! call print_grid()
    end do

    ! bounding rectangle:
    xmin = minval(x)
    xmax = maxval(x)
    ymin = minval(y)
    ymax = maxval(y)
   ! write(*,*) 'xmin, xmax, ymin, ymax = ', xmin, xmax, ymin, ymax
    ! count empty cells:
    iempty = 0
    do ix = xmin, xmax
        do iy = ymin, ymax
            if (.not. any(ix==x .and. iy==y)) iempty = iempty + 1
        end do
    end do

    end subroutine go

    subroutine print_grid()
        integer :: ix,iy
        character(len=1),dimension(:),allocatable :: line
        ! bounding rectangle:
        xmin = minval(x)
        xmax = maxval(x)
        ymin = minval(y)
        ymax = maxval(y)
        allocate(line(xmin:xmax))
        do iy = ymin, ymax
            line = '.'
            do ix = xmin, xmax
                if (any(ix==x .and. iy==y)) line(ix) = '#'
            end do
            !write(*,'(*(A1))') line
        end do
    end subroutine print_grid

    subroutine get_proposed_coords()
        ! If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
        ! If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
        ! If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
        ! If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.
        integer :: i

        proposed_x = huge(1)
        proposed_y = huge(1)

        do i = 1, n_elves
            ! If no other Elves are in one of those eight positions,
            ! the Elf does not do anything during this round
            !   ***
            !   *x*
            !   ***
            stuck(i) = .not. any([x==x(i)+1 .and. y==y(i),   &
                                  x==x(i)-1 .and. y==y(i),   &
                                  x==x(i)+1 .and. y==y(i)+1, &
                                  x==x(i)+1 .and. y==y(i)-1, &
                                  x==x(i)-1 .and. y==y(i)+1, &
                                  x==x(i)-1 .and. y==y(i)-1, &
                                  x==x(i)   .and. y==y(i)+1, &
                                  x==x(i)   .and. y==y(i)-1  ])
            if (stuck(i)) then
               ! write(*,*) 'elf ', i, ' stuck'
                cycle
            end if

            ! otherwise, go through the proposals:
            found = .false.
            do j = 1, 4
                proposed_x(i) = x(i)
                proposed_y(i) = y(i)
                select case (proposals(j))
                case(1)
                    ! If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
                    if (any(x==x(i)-1 .and. y==y(i)-1) .or. &
                        any(x==x(i)   .and. y==y(i)-1) .or. &
                        any(x==x(i)+1 .and. y==y(i)-1)) cycle
                    proposed_y(i) = y(i)-1; found = .true.; exit
                case(2)
                    ! If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
                    if (any(x==x(i)-1 .and. y==y(i)+1) .or. &
                        any(x==x(i)   .and. y==y(i)+1) .or. &
                        any(x==x(i)+1 .and. y==y(i)+1)) cycle
                        proposed_y(i) = y(i)+1; found = .true.; exit
                case(3)
                    ! If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
                    if (any(x==x(i)-1 .and. y==y(i)-1) .or. &
                        any(x==x(i)-1 .and. y==y(i)  ) .or. &
                        any(x==x(i)-1 .and. y==y(i)+1)) cycle
                    proposed_x(i) = x(i)-1; found = .true.; exit
                case(4)
                    ! If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.
                    if (any(x==x(i)+1 .and. y==y(i)-1) .or. &
                        any(x==x(i)+1 .and. y==y(i)  ) .or. &
                        any(x==x(i)+1 .and. y==y(i)+1)) cycle
                    proposed_x(i) = x(i)+1; found = .true.; exit
                end select
            end do
            if (.not. found) stuck(i) = .true. ! also stuck i guess

        end do

    end subroutine get_proposed_coords

    subroutine read_file(x,y)
        integer,dimension(:),allocatable,intent(out) :: x, y ! elf coordinates
        integer :: i, j, iunit, n_lines
        character(len=:),allocatable :: line
        if (allocated(x))          deallocate(x)
        if (allocated(y))          deallocate(y)
        if (allocated(proposed_x)) deallocate(proposed_x)
        if (allocated(proposed_y)) deallocate(proposed_y)
        if (allocated(stuck))      deallocate(stuck)
        if (allocated(prev_x))     deallocate(prev_x)
        if (allocated(prev_y))     deallocate(prev_y)
        open(newunit=iunit,file=input,status='OLD')
        n_lines = number_of_lines_in_file(iunit)
        allocate(x(0)); allocate(y(0))
        do i = 1, n_lines
            line = read_line(iunit)
            do j = 1, len(line)
                if (line(j:j)=='#') then
                    x = [x, j]
                    y = [y, i]
                end if
            end do
        end do
        close(iunit)
        n_elves = size(x)
        allocate(proposed_x(n_elves))
        allocate(proposed_y(n_elves))
        allocate(stuck(n_elves))
        allocate(prev_x(n_elves))
        allocate(prev_y(n_elves))

    end subroutine read_file

end program problem_23