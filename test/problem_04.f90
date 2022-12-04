program problem_4

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer :: i, iunit, n_lines, icount, istart1, iend1, istart2, iend2
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals, vals1,vals2

    icount = 0
    open(newunit=iunit, file='inputs/day4.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    do i = 1, n_lines
        line = read_line(iunit)
        call split(line,',',vals)
        call split(vals(1)%str,'-',vals1)
        call split(vals(2)%str,'-',vals2)

        istart1 = vals1(1)%to_int()
        iend1   = vals1(2)%to_int()
        istart2 = vals2(1)%to_int()
        iend2   = vals2(2)%to_int()

        if ((istart1>=istart2 .and. iend1<=iend2) .or. &
            (istart2>=istart1 .and. iend2<=iend1)) icount = icount + 1

    end do
    close(iunit)
    write(*,*) '4a: number fully contained', icount


    end program problem_4