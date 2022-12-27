program problem_2

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer :: iunit, istat, score
    character(len=3) :: str
    character(len=1) :: x,y

    open(newunit=iunit, file='inputs/day2.txt', status='OLD')

    score = 0
    do
        read(iunit,'(A3)', iostat=istat) str
        if (istat==iostat_end) exit

        x = str(1:1)
        y = str(3:3)

        select case (y)
        case('X') !lose
            score = score + 0
        case('Y') ! draw
            score = score + 3
        case('Z') ! win
            score = score + 6
        end select

        select case (x)
        case('A') !rock
            if (y=='X') then!lose
                score = score + 3
            elseif (y=='Y') then! draw
                score = score + 1
            elseif (y=='Z') then! win
                score = score + 2
            end if
        case('B') ! paper
            if (y=='X') then!lose
                score = score + 1
            elseif (y=='Y') then! draw
                score = score + 2
            elseif (y=='Z') then! win
                score = score + 3
            end if
        case('C') ! sissors
            if (y=='X') then!lose
                score = score + 2
            elseif (y=='Y') then! draw
                score = score + 3
            elseif (y=='Z') then! win
                score = score + 1
            end if
        end select

    end do
    write(*,*) '2b: score = ', score

    end program problem_2