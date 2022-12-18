program problem_18

    use aoc_utilities
    use iso_fortran_env, ip => int32

    implicit none

    integer :: iunit, n_rows, i, j, x, y, z, istat
    character(len=:),allocatable :: line
    integer,dimension(:,:),allocatable :: coords !! x,y,z coords of the cubes
    integer,dimension(:),allocatable :: n_common_faces !! number of faces common to other cubes
    integer :: surface_area, exterior_surface_area
    integer,dimension(3) :: delta

    ! read in coordinates:
    open(newunit=iunit,file='inputs/day18.txt', status='OLD')
    n_rows = number_of_lines_in_file(iunit)
    allocate(coords(n_rows,3))
    allocate(n_common_faces(n_rows))
    do i = 1, n_rows
        read(iunit,*) coords(i,:)
    end do
    close(iunit)
    ! find the common faces:
    n_common_faces = 0
    do i = 1, n_rows
        do j = 1, n_rows
            delta = coords(j,:) - coords(i,:)
            if (all(abs(delta)<=1) .and. count(abs(delta)==1)==1) &
                n_common_faces(i) = n_common_faces(i) + 1
        end do
    end do
    ! compute the surface area:
    surface_area = 0
    do i = 1, n_rows
        surface_area = surface_area + 6 - n_common_faces(i)
    end do
    write(*,*) '18a: ', surface_area

    ! compute the ones that are completely surrounded by other cubes:



end program problem_18