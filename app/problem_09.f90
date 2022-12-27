program problem_9

    use iso_fortran_env
    !use aoc_utilities

    implicit none

    integer :: i, j, iunit, istat
    character(len=1) :: direction
    integer :: steps
    integer,dimension(2) :: head
    integer,dimension(:),allocatable :: visited_j ! coordinates visited by tail
    integer,dimension(:),allocatable :: visited_i

    call go(1); write(*,*) '9a: ', size(visited_i)
    call go(9); write(*,*) '9b: ', size(visited_i)

    contains

    subroutine go(n_knots)

        integer,intent(in) :: n_knots ! the last one is the tail

        integer :: it
        integer,dimension(2) :: prev
        type :: tail_t
            integer,dimension(2) :: coords ! i,j coordinates of tail
        end type tail_t
        type(tail_t),dimension(:),allocatable :: tail

        open(newunit=iunit, file='inputs/day9.txt', status='OLD')

        ! start both at origin:
        head = 0
        allocate(tail(n_knots))
        do i = 1, n_knots
            tail(i)%coords = [0,0]
        end do
        visited_j = [0]; visited_i = [0]
        do
            read(iunit,'(A,1X,I2)',iostat=istat) direction, steps
            if (is_iostat_end(istat)) exit ! done

            !   j-->
            ! i s . .
            ! | . . .
            ! v . . .

            do i = 1, steps

                ! move head:
                select case (direction)
                case('U'); head(1) = head(1) - 1
                case('D'); head(1) = head(1) + 1
                case('L'); head(2) = head(2) - 1
                case('R'); head(2) = head(2) + 1
                end select

                do it = 1, n_knots
                    if (it==1) then
                        prev = head ! first one follow the head
                    else
                        prev = tail(it-1)%coords ! follow next tail knot
                    end if

                    associate (t => tail(it))
                        ! move tail:
                        if ( all(abs(prev-t%coords)<=1) ) cycle ! nothing to do
                        if (prev(1) == t%coords(1)) then ! left or right one
                            t%coords(2) = t%coords(2) + sign(1,prev(2)-t%coords(2))
                        else if (prev(2) == t%coords(2)) then ! up or down one
                            t%coords(1) = t%coords(1) + sign(1,prev(1)-t%coords(1))
                        else ! diagonal
                            t%coords = t%coords + sign(1,prev-t%coords)
                        end if

                        if (it==n_knots) then
                            ! add if not yet visited:
                            if (count(t%coords(1)==visited_i .and. t%coords(2) == visited_j)==0) then
                                visited_i = [visited_i, t%coords(1)]
                                visited_j = [visited_j, t%coords(2)]
                            end if
                        end if
                    end associate

                end do

            end do

        end do
        close(iunit)

        ! do i = 1, size(visited_i)
        !     write(*,*) 'visited ', visited_i(i), visited_j(i)
        ! end do

    end subroutine go

end program problem_9