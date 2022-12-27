program problem_24b

    use aoc_utilities
    use iso_fortran_env

    implicit none

    character(len=*),parameter :: input = 'inputs/day24.txt' ! file to read

    integer,dimension(:),allocatable :: x     ! current location of blizzards
    integer,dimension(:),allocatable :: y
    integer,dimension(:),allocatable :: dx    ! step for blizzard in each turn
    integer,dimension(:),allocatable :: dy
    integer :: wx     ! blizzard area width (value to mod for wrapping)
    integer :: wy     ! blizzard area height
    integer :: max_wait_time ! max time to wait before the blizzard configuration is the same
    integer,dimension(2) :: xy_start, xy_end
    integer,dimension(:),allocatable :: iwait_in
    integer,dimension(:),allocatable :: ix_in
    integer,dimension(:),allocatable :: iy_in
    integer,dimension(:),allocatable :: iwait_out
    integer,dimension(:),allocatable :: ix_out
    integer,dimension(:),allocatable :: iy_out
    integer :: imove, i, n
    integer,dimension(:),allocatable :: istart_visited_in
    integer,dimension(:),allocatable :: iend_visited_in
    integer,dimension(:),allocatable :: istart_visited_out
    integer,dimension(:),allocatable :: iend_visited_out

    call read_file()
    !call print_grid(x,y)

    imove = 0
    iwait_in = [0]
    ix_in = [xy_start(1)]
    iy_in = [xy_start(2)]
    iend_visited_in = [0]
    istart_visited_in = [1]

    do ! BFS
        call advance(imove,ix_in,iy_in,iwait_in,istart_visited_in,iend_visited_in,&
                            ix_out,iy_out,iwait_out,istart_visited_out,iend_visited_out)
        ix_in = ix_out
        iy_in = iy_out
        iwait_in = iwait_out
        istart_visited_in = istart_visited_out
        iend_visited_in = iend_visited_out
        n = size(ix_in)
        !write(*,*) 'imove = ', imove, ' number of candidates = ', n
        if (n==1 .and. istart_visited_in(1)==2) then ! when there's a solution, there will only be one
            if (any(ix_in==xy_end(1) .and. iy_in==xy_end(2))) then ! a solution
                write(*,*) '24b: ', imove
                exit
            end if
        end if
    end do

    contains

    subroutine advance(imove,ix_in,iy_in,iwait_in,istart_visited_in,iend_visited_in,&
                        ix_out,iy_out,iwait_out,istart_visited_out,iend_visited_out)
        integer,intent(inout) :: imove
        integer,dimension(:),intent(in) :: ix_in
        integer,dimension(:),intent(in) :: iy_in
        integer,dimension(:),intent(in) :: iwait_in
        integer,dimension(:),intent(in)  :: istart_visited_in
        integer,dimension(:),intent(in)  :: iend_visited_in
        integer,dimension(:),allocatable,intent(out) :: ix_out
        integer,dimension(:),allocatable,intent(out) :: iy_out
        integer,dimension(:),allocatable,intent(out) :: iwait_out
        integer,dimension(:),allocatable,intent(out)  :: istart_visited_out
        integer,dimension(:),allocatable,intent(out)  :: iend_visited_out

        integer :: i, j, m

        ! output arrays:
        allocate(ix_out(0))
        allocate(iy_out(0))
        allocate(iwait_out(0))
        allocate(istart_visited_out(0))
        allocate(iend_visited_out(0))

        ! next move:
        imove = imove + 1
        call update_blizzards(x,y)

        ! process each input (BFS):
        do j = 1, size(ix_in)
            associate (ix => ix_in(j), iy => iy_in(j), iwait => iwait_in(j), &
                       iend_visited => iend_visited_in(j), &
                       istart_visited => istart_visited_in(j))
                ! compute each option to move:
                if (ix==xy_start(1) .and. iy==xy_start(2)) then
                    ! if at the start point, only two options: wait or go s
                    if (free_spot(x,y,ix,iy+1)) call add(ix,iy+1,0,istart_visited,iend_visited,&
                                                            ix_out,iy_out,iwait_out,&
                                                            istart_visited_out,iend_visited_out) ! s
                else if (ix==xy_end(1) .and. iy==xy_end(2)) then
                    ! if at the end point, only two options: wait or go n
                    if (free_spot(x,y,ix,iy-1)) call add(ix,iy-1,0,istart_visited,iend_visited,&
                                                            ix_out,iy_out,iwait_out,&
                                                            istart_visited_out,iend_visited_out) ! n
                else
                    ! are we done?
                    !
                    ! start -> end -> start -> end
                    !
                    if (ix==xy_end(1) .and. iy==xy_end(2)-1) then
                        if (istart_visited==2 .and. iend_visited==1) then
                            ix_out    = [ix]  ! solution - return it
                            iy_out    = [iy+1]
                            iwait_out = [0]
                            istart_visited_out = [2]
                            iend_visited_out   = [2]
                            return
                        else
                            ! add this point and continue only if not already visited
                            if (iend_visited==0) call add(ix,iy+1,0,istart_visited,1,&
                                                          ix_out,iy_out,iwait_out,&
                                                          istart_visited_out,iend_visited_out)
                        end if
                    elseif (ix==xy_start(1) .and. iy==xy_start(2)+1) then
                        if (istart_visited==1 .and. iend_visited==1) then
                            call add(ix,iy-1,0,2,iend_visited,&
                                     ix_out,iy_out,iwait_out,&
                                     istart_visited_out,iend_visited_out) ! add this point and continue
                        end if
                    end if
                    ! get valid modes in grid.
                    if (free_spot(x,y,ix,iy-1)) call add(ix,iy-1,0,istart_visited,iend_visited,&
                                                         ix_out,iy_out,iwait_out,&
                                                         istart_visited_out,iend_visited_out) ! n
                    if (free_spot(x,y,ix,iy+1)) call add(ix,iy+1,0,istart_visited,iend_visited,&
                                                         ix_out,iy_out,iwait_out,&
                                                         istart_visited_out,iend_visited_out) ! s
                    if (free_spot(x,y,ix-1,iy)) call add(ix-1,iy,0,istart_visited,iend_visited,&
                                                         ix_out,iy_out,iwait_out,&
                                                         istart_visited_out,iend_visited_out) ! w
                    if (free_spot(x,y,ix+1,iy)) call add(ix+1,iy,0,istart_visited,iend_visited,&
                                                         ix_out,iy_out,iwait_out,&
                                                         istart_visited_out,iend_visited_out) ! e
                end if
                ! finally, we have the option to wait, if there is no blizzard here at this time
                if (iwait < max_wait_time) then ! only if we haven't waited the max time
                    if ((ix==xy_end(1) .and. iy==xy_end(2)) .or. &
                        (ix==xy_start(1) .and. iy==xy_start(2)) .or. &
                        free_spot(x,y,ix,iy)) then
                        call add(ix,iy,iwait+1,istart_visited,iend_visited,&
                                 ix_out,iy_out,iwait_out,&
                                 istart_visited_out,iend_visited_out) ! wait at this spot
                    end if
                end if
            end associate
        end do

    end subroutine advance

    subroutine add(ix,iy,iwait,istart_visited,iend_visited,&
                   ix_out,iy_out,iwait_out,istart_visited_out,iend_visited_out)
        integer,intent(in) :: ix
        integer,intent(in) :: iy
        integer,intent(in) :: iwait
        integer,intent(in) :: istart_visited
        integer,intent(in) :: iend_visited
        integer,dimension(:),allocatable,intent(inout) :: ix_out
        integer,dimension(:),allocatable,intent(inout) :: iy_out
        integer,dimension(:),allocatable,intent(inout) :: iwait_out
        integer,dimension(:),allocatable,intent(inout) :: istart_visited_out
        integer,dimension(:),allocatable,intent(inout) :: iend_visited_out
        ! add it if if isn't already there
        if (.not. any(ix==ix_out .and. iy==iy_out .and. iwait==iwait_out .and. &
                      istart_visited==istart_visited_out .and. &
                      iend_visited==iend_visited_out )) then
            ix_out             = [ix_out,ix]
            iy_out             = [iy_out,iy]
            iwait_out          = [iwait_out,iwait]
            istart_visited_out = [istart_visited_out,istart_visited]
            iend_visited_out   = [iend_visited_out,iend_visited]
        end if
    end subroutine add

    logical function free_spot(x,y,ix,iy)
        integer,dimension(:),intent(in) :: x
        integer,dimension(:),intent(in) :: y
        integer,intent(in) :: ix,iy
        free_spot = iy>=0 .and. iy<=wy-1 .and. ix>=0 .and. ix<=wx-1
        if (free_spot) free_spot = .not. any(ix==x .and. iy==y) ! no blizzard
    end function free_spot

    ! pure integer function dist(x1,y1,x2,y2) ! Manhattan distance
    !     integer,intent(in) :: x1,y1,x2,y2
    !     dist = abs(x1 - x2) + abs(y1 - y2)
    ! end function dist

    subroutine update_blizzards(x,y)
        integer,dimension(:),intent(inout) :: x
        integer,dimension(:),intent(inout) :: y
        x = modulo(x + dx,wx)
        y = modulo(y + dy,wy)
    end subroutine update_blizzards

    subroutine print_grid(x,y)
        integer,dimension(:),intent(in) :: x
        integer,dimension(:),intent(in) :: y
        integer :: i,j,k
        character(len=:),allocatable :: str
        integer :: n_found
        character(len=10) :: istr
        write(*,*) ''
        do j = -1,wy   ! -1, 0, 1, 2, 3, 4
            if (j==-1) then
                write(*,'(A)') '#.'//repeat('#',wx)
            else if (j==wy) then
                write(*,'(A)') repeat('#',wx)//'.#'
            else
                do i = -1,wx
                    if (i==-1) then
                        str = '#'
                    else if (i==wx) then
                        str = str//'#'
                    else
                        call get_index(x,y,i,j,k,n_found)
                        if (n_found==1) then ! blizzard
                            if      (dx(k)== 1 .and. dy(k)== 0) then; str = str//'>'
                            else if (dx(k)==-1 .and. dy(k)== 0) then; str = str//'<'
                            else if (dx(k)== 0 .and. dy(k)==-1) then; str = str//'^'
                            else if (dx(k)== 0 .and. dy(k)== 1) then; str = str//'v'
                            end if
                        else if (n_found>1) then ! more than one blizzard in this spot
                            write(istr,'(I10)') n_found; istr = adjustl(istr)
                            str = str//istr(1:1)
                        else
                            str = str//'.'
                        end if
                    end if
                end do
                write(*,'(A)') str
            end if
        end do
        write(*,*) ''
    end subroutine print_grid

    subroutine get_index(x,y,i,j,k,n_found)
        integer,dimension(:),intent(in) :: x
        integer,dimension(:),intent(in) :: y
        integer,intent(in) :: i,j ! index to look for
        integer,intent(out) :: k ! location in the xy arrays
        integer,intent(out) :: n_found ! number found
        integer :: ik
        n_found = 0
        k = -huge(1)
        do ik = 1, size(x)
            if (x(ik)==i .and. y(ik)==j) then
                k = ik
                n_found = n_found + 1
            end if
        end do
    end subroutine get_index

    subroutine read_file()
    character(len=:),allocatable :: line
    integer :: n_lines, i, iunit, j, ix, iy
    character(len=1) :: c
    open(newunit=iunit,file=input,status='OLD')
    n_lines = number_of_lines_in_file(iunit)
    if (allocated(x )) deallocate(x ); allocate(x (0))
    if (allocated(y )) deallocate(y ); allocate(y (0))
    if (allocated(dx)) deallocate(dx); allocate(dx(0))
    if (allocated(dy)) deallocate(dy); allocate(dy(0))
    do i = 1, n_lines
        line = read_line(iunit)
        iy = i - 2 ! indices start at 0 inside the wall
        do j = 1, len(line)
            ix = j-2
            if (j==1) then
                wx = len(line) - 2 ! width where blizzards can be
                wy = n_lines - 2   ! height where blizzards can be
            end if
            c = line(j:j)
            select case (c)
            case('>'); x  = [x,ix]; y = [y,iy]; dx = [dx,  1];  dy = [dy,  0] ! go right
            case('<'); x  = [x,ix]; y = [y,iy]; dx = [dx, -1];  dy = [dy,  0] ! go left
            case('^'); x  = [x,ix]; y = [y,iy]; dx = [dx,  0];  dy = [dy, -1] ! go up
            case('v'); x  = [x,ix]; y = [y,iy]; dx = [dx,  0];  dy = [dy,  1] ! go down
            end select
        end do
    end do
    close(iunit)
    max_wait_time = (wx * wy) - 1
    xy_start = [0,-1]
    xy_end   = [wx-1,wy]
    end subroutine read_file

end program problem_24b