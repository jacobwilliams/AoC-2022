program problem_1

use iso_fortran_env
!use aoc_utilities

implicit none

integer,dimension(:),allocatable :: iarray
integer :: i, n, elf, iunit, istat, ical
character(len=10) :: str
integer :: current_count, max_count, max_elf

open(newunit=iunit, file='inputs/day1.txt', status='OLD')
max_elf = 0
elf = 1
max_count = 0
current_count = 0
do
    read(iunit,'(A)', iostat=istat) str
    if (istat==iostat_end .or. str=='') then
        !update max count
        if (current_count > max_count) then
            max_count = current_count
            max_elf = elf
        end if
        elf = elf + 1
        current_count = 0
    end if
    if (istat==iostat_end) exit

    read(str, '(I10)') ical
    current_count = current_count + ical

end do

write(*,*) '1a: elf ', max_elf, ' has the max cals: ', max_count

! iarray = read_file_to_integer_array('inputs/day1.txt')

! n = 0
! do i = 2, size(iarray)
!     if (iarray(i)>iarray(i-1)) n = n + 1
! end do

! write(*,*) '1A: number measurements larger than the previous: ', n

! n = 0
! do i = 2, size(iarray)-2
!     if (sum(iarray(i:i+2)) > sum(iarray(i-1:i+1))) n = n + 1
! end do
! write(*,*) '1B: number sums larger than the previous: ', n

end program problem_1