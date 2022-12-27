program problem_20

    use aoc_utilities
    use iso_fortran_env, ip => int64

    implicit none

    integer(ip) :: i, j, k, iunit, n_rows, n
    integer(ip),dimension(:),allocatable :: iarray

    type :: intp
        integer(ip) :: i = 0_ip !! value
        type(intp),pointer :: next => null()
        type(intp),pointer :: prev => null()
    end type intp
    type :: int_list
        type(intp),pointer :: first => null()   ! maybe don't need this ?
    end type int_list
    type :: int_element
      type(intp),pointer :: p
    end type int_element

    type(int_list) :: list
    type(int_element),dimension(:),allocatable :: int_array ! an array to preserve the original order

    call read_list(1_ip)
    call mix(.false.)
    associate (x => [get_value(1000_ip),get_value(2000_ip),get_value(3000_ip)])
        write(*,*) '20a : ', sum(x)
    end associate

    call read_list(811589153_ip)
    do i = 1, 10
        call mix(rezero=.true.)
    end do
    associate (x => [get_value(1000_ip),get_value(2000_ip),get_value(3000_ip)])
        write(*,*) '20b : ', sum(x)
    end associate

    contains

    subroutine read_list(multiply)
        integer(ip),intent(in) :: multiply
        type(intp),pointer :: tmp, prev
        iarray = read_file_to_integer64_array('inputs/day20.txt') * multiply
        n = size(iarray)
        if (allocated(int_array)) deallocate(int_array); allocate(int_array(n))
        ! construct a circular linked list and index array:
        prev => null()
        do i = 1, n
            allocate(tmp)
            int_array(i)%p => tmp ! an indexing array
            tmp%i = iarray(i)
            tmp%prev => prev
            if (i==1) then ! first
                list%first => tmp
            elseif (i==n) then ! last
                tmp%next => list%first ! circular
                tmp%prev%next => tmp
                list%first%prev => tmp
            else ! in the middle
                tmp%prev%next => tmp
            end if
            prev => tmp
        end do
    end subroutine read_list

    subroutine mix(rezero)
        logical,intent(in) :: rezero
        integer(ip) :: i, ival
        type(intp),pointer :: tmp, curr, p1, p2, p3, p4
        do i = 1, n
            curr => int_array(i)%p
            tmp => curr
            ival = modulo(curr%i,n-1) ! only shift forward (always >0)
            if (ival==0) cycle
            do j = 1, abs(ival) ! just a sequence of swaps
                p1 => curr%prev
                p2 => curr
                p3 => curr%next
                p4 => curr%next%next
                if (associated(list%first, p2)) list%first => p3
                p1%next => p3
                p4%prev => p2
                p3%prev => p1
                p3%next => p2
                p2%prev => p3
                p2%next => p4
            end do
        end do
        if (rezero) then
            tmp => list%first
            do
                if (tmp%i == 0) exit
                tmp => tmp%next
            end do
            list%first => tmp
        end if
    end subroutine

    subroutine print_list(verbose)
        logical,intent(in) :: verbose
        integer(ip) :: k
        type(intp),pointer :: tmp
        character(len=50) :: str
        tmp => list%first
        do k = 1, n
            if (.not. associated(tmp)) exit
            if (verbose) then
                write(*,*) tmp%prev%i, '<->', tmp%i, tmp%next%i
            else
                write(str,'(I50)') tmp%i
                write(*,'(A,",")',advance='NO') trim(adjustl(str))
            end if
            tmp => tmp%next
        end do
        write(*,*) ''
    end subroutine print_list

    integer(ip) function get_value(i)
        integer(ip),intent(in) :: i ! index (after 0 element)
        integer(ip) :: k
        type(intp),pointer :: tmp
        tmp => list%first
        do  ! first find the 0 element
            if (tmp%i == 0) exit
            tmp => tmp%next
        end do
        do k = 1, i
            tmp => tmp%next
        end do
        get_value = tmp%i

    end function get_value

end program problem_20