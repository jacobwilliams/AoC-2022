program problem_13

    use aoc_utilities
    use iso_fortran_env
    use json_module

    implicit none

    integer :: i, j, iunit, n_rows, n_elements, ipair, s
    character(len=:),allocatable :: line, str
    type(json_core) :: json
    type(json_value),pointer :: p, p_left, p_right
    integer :: correct

    character(len=*),dimension(-1:1),parameter :: result = ['unknown    ',&
                                                            'right order',&
                                                            'wrong order']
    ! convert to json for parsing:
    open(newunit=iunit,file='inputs/day13.txt', status='OLD')
    n_rows = number_of_lines_in_file(iunit)
    str = '['
    do i = 1, n_rows
        line = read_line(iunit)
        if (line=='') cycle
        str = str // line
        if (i<n_rows) str = str // ','
    end do
    str = str // ']'
    close(iunit)
    call json%deserialize(p, str)
    call json%initialize(compress_vectors=.true.)
    !call json%print(p)

    ! now, process each pair:
    call json%info(p, n_children = n_elements)
    ipair = 0
    s = 0
    do i = 1, n_elements,2
        ipair = ipair + 1
        ! write(*,*) ''
        ! write(*,*) '============================================================'
        ! write(*,*) 'pair: ', ipair
        call json%get_child(p, i,   p_left)
        call json%get_child(p, i+1, p_right)
        correct = -1 ! not yet known
        call compare(p_left, p_right, correct)
        ! write(*,*) 'Result for pair', ipair, ' : ', result(correct)
        ! write(*,*) '============================================================'
        if (correct == 0) s = s + ipair
    end do

    write(*,*) '13a: ', s

    ! 2559 - too low

    contains

    recursive subroutine compare(p_left, p_right, correct)

        type(json_value),pointer :: p_left, p_right
        integer,intent(inout) :: correct !! -1: unknown, 0: right order, 1: wrong order

        integer :: i, type_left, type_right, i_left, i_right, n_children_right, n_children_left
        type(json_value),pointer :: p_left_tmp, p_right_tmp, p_int

        ! write(*,*) '.....................................'
        ! write(*,*) 'compare:'
        ! call json%print(p_left)
        ! write(*,*) 'with:'
        ! call json%print(p_right)
        ! write(*,*) ''

        call json%info(p_left,  var_type = type_left,  n_children = n_children_left)
        call json%info(p_right, var_type = type_right, n_children = n_children_right)

        if (type_right==json_integer .and. type_left==json_integer) then

            ! If both values are integers, the lower integer should come first. If the left integer is lower than the right integer, the inputs are in the right order. If the left integer is higher than the right integer, the inputs are not in the right order. Otherwise, the inputs are the same integer; continue checking the next part of the input.

            call json%get(p_left,i_left)
            call json%get(p_right,i_right)

            if (i_left==i_right) then
                ! continue
            else if (i_left < i_right) then
                correct = 0 ! correct order
            else
                correct = 1 ! wrong order
            end if

        else if (type_right==json_array .and. type_left==json_array) then

            ! If both values are lists, compare the first value of each list, then the second value, and so on. If the left list runs out of items first, the inputs are in the right order. If the right list runs out of items first, the inputs are not in the right order. If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.

            if (n_children_left>0 .or. n_children_right>0) then
                do i = 1, max(n_children_left,n_children_right)
                    if (i>n_children_left) then
                        if (correct==-1) then ! left side runs out
                            correct = 0 ! correct order
                            return
                        end if
                    else if (i>n_children_right) then
                        if (correct==-1) then ! right side runs out
                            correct = 1 ! wrong order
                            return
                        end if
                    else
                        call json%get_child(p_left,  i, p_left_tmp)
                        call json%get_child(p_right, i, p_right_tmp)
                        call compare(p_left_tmp, p_right_tmp, correct)
                        if (correct==1) return ! wrong
                    end if
                end do
            end if

        else if (type_left==json_array .and. type_right==json_integer) then

            ! If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison. For example, if comparing [0,0,0] and 2, convert the right value to [2] (a list containing 2); the result is then found by instead comparing [0,0,0] and [2].

            call json%get(p_right, i)
            call json%create_array(p_int, '')
            call json%add(p_int, '', i)
            call json%replace(p_right, p_int)
            call compare(p_left, p_int, correct)

        else if (type_left==json_integer .and. type_right==json_array) then

            call json%get(p_left, i)
            call json%create_array(p_int, '')
            call json%add(p_int, '', i)
            call json%replace(p_left, p_int)
            call compare(p_int, p_right, correct)

        end if

    end subroutine compare

end program problem_13