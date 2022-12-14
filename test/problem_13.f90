program problem_13

    use aoc_utilities
    use iso_fortran_env
    use json_module

    implicit none

    integer :: i, j, iunit, n_rows, n_elements
    character(len=:),allocatable :: line, str
    type(json_core) :: json
    type(json_value),pointer :: p, p_left, p_right

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
    !call json%initialize(compress_vectors=.true.)
    !call json%print(p)

    ! now, process each pair:
    call json%info(p, n_children = n_elements)
    do i = 1, n_elements,2
        call json%get_child(p, i, p_left)
        call json%get_child(p, i+1, p_right)
        call compare(p_left, p_right)
    end do


    contains

    recursive subroutine compare(p_left, p_right)

        type(json_value),pointer :: p_left, p_right
        integer :: type_left, type_right
        integer :: i_left, i_right

        call json%info(p_right, var_type = type_right)
        call json%info(p_left,  var_type = type_left)

        if (type_right==json_integer .and. type_right==type_left) then

            ! If both values are integers, the lower integer should come first. If the left integer is lower than the right integer, the inputs are in the right order. If the left integer is higher than the right integer, the inputs are not in the right order. Otherwise, the inputs are the same integer; continue checking the next part of the input.

            call json%get(p_left,i_left)
            call json%get(p_right,i_right)
            write(*,*) 'int/int', i_left, i_right

        else if (type_right==json_array .and. type_right==json_array) then

            ! If both values are lists, compare the first value of each list, then the second value, and so on. If the left list runs out of items first, the inputs are in the right order. If the right list runs out of items first, the inputs are not in the right order. If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.


            write(*,*) 'array/array'


        else if (type_right==json_integer .and. type_right==json_array) then

            ! If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison. For example, if comparing [0,0,0] and 2, convert the right value to [2] (a list containing 2); the result is then found by instead comparing [0,0,0] and [2].

            write(*,*) 'int/array'

        else if (type_right==json_array .and. type_right==json_integer) then
            write(*,*) 'array/int'

        end if

    end subroutine compare

end program problem_13