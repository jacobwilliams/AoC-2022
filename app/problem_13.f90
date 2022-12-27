program problem_13

    use aoc_utilities
    use iso_fortran_env
    use json_module

    implicit none

    integer :: i, j, n_elements, ipair, s, correct
    type(json_core) :: json
    type(json_value),pointer :: p, p_left, p_right
    logical :: sorted
    integer,dimension(:),allocatable :: indices

    call parse(add_dividers = .false.)
    ! now, process each pair:
    call json%info(p, n_children = n_elements)
    ipair = 0
    s = 0
    do i = 1, n_elements,2
        ipair = ipair + 1
        call json%get_child(p, i,   p_left)
        call json%get_child(p, i+1, p_right)
        call compare(p_left, p_right, correct, init = .true.)
        if (correct == 0) s = s + ipair
    end do
    write(*,*) '13a: ', s
    call json%destroy(p)

    call parse(add_dividers = .true.)
    call json%info(p, n_children = n_elements)
    ! bubble sort that baby:
    allocate(indices(n_elements)); indices = [(i, i = 1, n_elements)]
    do
        sorted = .true.
        do i = 1, n_elements-1
            call json%get_child(p, i,   p_left)
            call json%get_child(p, i+1, p_right)
            call compare(p_left, p_right, correct, init = .true.)
            if (correct==1) then
                call json%swap(p_left,p_right)
                call swap(indices(i), indices(i+1))
                sorted = .false.
            end if
        end do
        if (sorted) exit
    end do
    ! indices of the two divider packets:
    write(*,*) '13b: ', findloc(indices,1) * findloc(indices,2)
    call json%destroy(p)

    contains

    subroutine parse(add_dividers)

        logical,intent(in) :: add_dividers

        integer :: i, iunit, n_rows, n
        character(len=:),allocatable :: line, str

        ! convert to json for parsing:
        open(newunit=iunit,file='inputs/day13.txt', status='OLD')
        n_rows = number_of_lines_in_file(iunit)
        str = '['
        if (add_dividers) str = str // '[[2]], [[6]],' ! add the extra packets
        do i = 1, n_rows
            line = read_line(iunit)
            if (line=='') cycle
            str = str // line
            if (i<n_rows) str = str // ','
        end do
        str = str // ']'
        close(iunit)
        call json%deserialize(p, str)
        call json%initialize(no_whitespace=.true.)

    end subroutine parse

    recursive subroutine compare(p_left, p_right, correct, init)

        ! we have to stop comparing as soon as correct /= -1.
        ! this was not clear to me in the problem description.

        type(json_value),pointer :: p_left, p_right
        integer,intent(inout) :: correct !! -1: unknown, 0: right order, 1: wrong order
        logical,intent(in),optional :: init !! if true, being called for first time

        integer :: i, type_left, type_right, i_left, i_right, n_children_right, n_children_left
        type(json_value),pointer :: p_left_tmp, p_right_tmp, p_int

        ! first call:
        if (present(init)) then
            if (init) correct = -1
        end if

        if (correct==1 .or. correct==0) return ! done

        call json%info(p_left,  var_type = type_left,  n_children = n_children_left)
        call json%info(p_right, var_type = type_right, n_children = n_children_right)

        if (type_left==json_integer .and. type_right==json_integer) then

            call json%get(p_left, i_left)
            call json%get(p_right,i_right)
            if (i_left==i_right) then
                ! continue
            else if (i_left < i_right) then
                correct = 0 ! correct order
            else
                correct = 1 ! wrong order
            end if

        else if (type_right==json_array .and. type_left==json_array) then

            if (n_children_left>0 .or. n_children_right>0) then
                do i = 1, max(n_children_left,n_children_right)
                    if (i>n_children_left) then
                        if (correct==-1) then ! left side runs out
                            correct = 0 ! correct order
                            return
                        end if
                        exit
                    else if (i>n_children_right) then
                        if (correct==-1) then ! right side runs out
                            correct = 1 ! wrong order
                            return
                        end if
                        exit
                    else
                        call json%get_child(p_left,  i, p_left_tmp)
                        call json%get_child(p_right, i, p_right_tmp)
                        call compare(p_left_tmp, p_right_tmp, correct)
                        if (correct==1 .or. correct==0) return ! done
                    end if
                end do
            end if

        else if (type_left==json_array .and. type_right==json_integer) then

            call json%get(p_right, i)
            call json%create_array(p_int, '')
            call json%add(p_int, '', i)
            call compare(p_left, p_int, correct)
            call json%destroy(p_int)

        else if (type_left==json_integer .and. type_right==json_array) then

            call json%get(p_left, i)
            call json%create_array(p_int, '')
            call json%add(p_int, '', i)
            call compare(p_int, p_right, correct)
            call json%destroy(p_int)

        end if

    end subroutine compare

end program problem_13