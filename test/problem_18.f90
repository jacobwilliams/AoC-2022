program problem_18

    use aoc_utilities
    use iso_fortran_env, ip => int32

    implicit none

    integer :: iunit, n_rows, i, j
    integer,dimension(:,:),allocatable :: coords !! x,y,z coords of the cubes
    integer :: total_surf_area, fill_surf_area, external_surf_area
    integer :: maxx
    integer,dimension(:,:),allocatable :: coords2 !! x,y,z coords of the external part
    logical,dimension(:,:,:),allocatable :: visited

    ! read in coordinates:
    open(newunit=iunit,file='inputs/day18.txt', status='OLD')
    n_rows = number_of_lines_in_file(iunit)
    allocate(coords(n_rows,3))
    do i = 1, n_rows
        read(iunit,*) coords(i,:)
    end do
    close(iunit)

    total_surf_area = surface_area(coords)
    write(*,*) '18a: ', total_surf_area

    ! fill the area around it, and compute the surface area of that,
    ! minus the outer box
    maxx = maxval(coords) + 1  ! bounding cube size (-1:maxx)
    allocate(visited(-1:maxx, -1:maxx, -1:maxx))
    visited = .false.
    do i = 1, n_rows
        visited(coords(i,1), coords(i,2), coords(i,3)) = .true. ! don't visit the other ones
    end do
    call fill(coords2, [maxx,maxx,maxx]) ! fill the bounding cube
    fill_surf_area = surface_area(coords2)
    external_surf_area = fill_surf_area - 6*(maxx+2)*(maxx+2)
    write(*,*) '18b: ', external_surf_area

    contains

    recursive subroutine fill(c, xyz)
    integer,dimension(:,:),allocatable,intent(inout) :: c !! x,y,z coords of the cubes
    integer,dimension(3),intent(in) :: xyz ! point to check

    integer,dimension(3) :: tmp

    if (any(xyz>maxx) .or. any(xyz<-1)) return ! outside of bounding box
    if (visited(xyz(1),xyz(2),xyz(3))) return  ! already visited
    visited(xyz(1),xyz(2),xyz(3)) = .true.
    if (allocated(c)) then
        if (any(xyz(1)==c(:,1) .and. &
                xyz(2)==c(:,2) .and. &
                xyz(3)==c(:,3) )) return ! already accounted for
    end if

    call add_cube(c, xyz)

    ! check adjacent ones:
    tmp = [xyz(1)+1, xyz(2),   xyz(3)  ]; call fill(c, tmp)
    tmp = [xyz(1)  , xyz(2)+1, xyz(3)  ]; call fill(c, tmp)
    tmp = [xyz(1)  , xyz(2),   xyz(3)+1]; call fill(c, tmp)
    tmp = [xyz(1)-1, xyz(2),   xyz(3)  ]; call fill(c, tmp)
    tmp = [xyz(1)  , xyz(2)-1, xyz(3)  ]; call fill(c, tmp)
    tmp = [xyz(1)  , xyz(2),   xyz(3)-1]; call fill(c, tmp)

    end subroutine fill

    subroutine add_cube(c,xyz)
        integer,dimension(:,:),allocatable,intent(inout) :: c !! x,y,z coords of the cubes
        integer,dimension(3),intent(in) :: xyz !! add this cube

        integer,dimension(:,:),allocatable :: tmp
        integer :: n ! size of c

        if (allocated(c)) then
            n = size(c,1)
            allocate(tmp(n+1,3))
            tmp(1:n,:) = c
            tmp(n+1,:) = xyz
            call move_alloc(tmp, c)
        else
            allocate(c(1,3)); c(1,:) = xyz
        end if

    end subroutine add_cube

    integer function surface_area(c)
        integer,dimension(:,:),intent(in) :: c !! x,y,z coords of the cubes

        integer,dimension(:),allocatable :: n_common_faces !! number of faces common to other cubes
        integer,dimension(3) :: delta
        integer :: i
        integer :: n

        n = size(c,1)
        allocate(n_common_faces(n))
        ! find the common faces:
        n_common_faces = 0
        do i = 1, n
            do j = 1, n
                delta = c(j,:) - c(i,:)
                if (all(abs(delta)<=1) .and. count(abs(delta)==1)==1) &
                    n_common_faces(i) = n_common_faces(i) + 1
            end do
        end do
        surface_area = sum(6-n_common_faces)

    end function surface_area

end program problem_18