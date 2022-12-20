program problem_20

    use aoc_utilities
    use iso_fortran_env, ip => int32

    implicit none

    integer :: i, j, k, iunit, n_rows, n, ival
    integer,dimension(:),allocatable :: iarray

    type :: intp
        integer :: i = 0 !! value
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
    type(intp),pointer :: tmp, curr, p1, p2, p3, p4
    type(intp),pointer :: prev !, next

    iarray = read_file_to_integer_array('inputs/day20.txt')
    n = size(iarray)
    allocate(int_array(n))

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

    ! mix them:
    do i = 1, n
        curr => int_array(i)%p
        tmp => curr
        ival = curr%i
        if (ival==0) cycle
        do j = 1, abs(ival) ! just a swap..
            if (ival>0) then ! shift curr forward

                !   p1    p2    p3     p4
                !   4 <- [1] ->  2  -> 3 -> 4 -> [1]
                !   4 <-  2  -> [1] -> 3 -> 4 -> 2
                p1 => curr%prev
                p2 => curr
                p3 => curr%next
                p4 => curr%next%next

                if (associated(list%first, p2)) list%first => p3

            else ! shift backward

                !  p1    p2     p3    p4
                !   4 <-  2  -> [1] -> 3 -> 4 -> 2
                !   4 <- [1] ->  2  -> 3 -> 4 -> [1]

                p1 => curr%prev%prev
                p2 => curr%prev
                p3 => curr
                p4 => curr%next

                if (associated(list%first, p2)) list%first => p3

            end if

            p1%next => p3
            p4%prev => p2
            p3%prev => p1
            p3%next => p2
            p2%prev => p3
            p2%next => p4

        end do

    end do

    write(*,*) '20a : ', get_value(1000)+get_value(2000)+get_value(3000)

    contains

    subroutine print_list(verbose)
        logical,intent(in) :: verbose
        integer :: k
        type(intp),pointer :: tmp
        tmp => list%first
        do k = 1, n
            if (.not. associated(tmp)) exit
            if (verbose) then
                write(*,*) tmp%prev%i, '<->', tmp%i, tmp%next%i
            else
                write(*,'(I4,",")',advance='NO') tmp%i
            end if
            tmp => tmp%next
        end do
        write(*,*) ''
    end subroutine print_list

    integer function get_value(i)
        integer,intent(in) :: i ! index (after 0 element)

        integer :: k
        type(intp),pointer :: tmp

        tmp => list%first
        ! first find the 0 element
        do
            if (tmp%i == 0) exit
            tmp => tmp%next
        end do
        do k = 1, i
            tmp => tmp%next
        end do
        get_value = tmp%i

    end function get_value

end program problem_20