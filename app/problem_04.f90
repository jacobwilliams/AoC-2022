program problem_4

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer :: i, iunit, n_lines, icount, istart1, iend1, istart2, iend2, icount2
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals, s1, s2

    icount = 0; icount2 = 0
    open(newunit=iunit, file='inputs/day4.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    do i = 1, n_lines

        ! get star/end of each pair as integers:
        line = read_line(iunit)
        vals = split(line,',')
        s1 = split(vals(1),'-'); istart1 = int(s1(1)); iend1 = int(s1(2))
        s2 = split(vals(2),'-'); istart2 = int(s2(1)); iend2 = int(s2(2))

        ! completely overlap:
        if ((istart1>=istart2 .and. iend1<=iend2) .or. &
            (istart2>=istart1 .and. iend2<=iend1)) icount = icount + 1

        ! overlap at all:
        if ((istart1>=istart2 .and. istart1<=iend2) .or. &
            (istart2>=istart1 .and. istart2<=iend1)) icount2 = icount2 + 1

    end do
    close(iunit)

    write(*,*) '4a: number fully contained', icount
    write(*,*) '4b: number that overlap at all', icount2

    end program problem_4