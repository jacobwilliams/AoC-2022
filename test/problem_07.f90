program problem_7

    use aoc_utilities

    implicit none

    integer :: i, j, iunit, n_lines, isize
    character(len=:),allocatable :: line, dir, cwd
    type(string),dimension(:),allocatable :: vals
    type(string),dimension(:),allocatable :: directories
    integer,dimension(:),allocatable :: sizes

    open(newunit=iunit, file='inputs/day7.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    line = read_line(iunit); i = 1
    main: do
        if (i==n_lines) exit
        if (line=='') then
            ! do nothing
        else if (line(1:4)=='$ cd') then
            vals = split(line,' ')
            dir = vals(3)%str
            if (dir=='/') then
                cwd = dir
                call add_directory(cwd) ! *****
                dir = './RESULTS'  ! for the bash script
            else if (dir=='..') then
                vals = split(cwd,'/')
                if (size(vals)==2) then  ! /a -> '', 'a' ==> '/'
                    cwd = '/'
                else !/a/b -> '', 'a', 'b' ==> '/a'
                    cwd = ''
                    do j = 2, size(vals)-1
                        cwd = cwd//'/'//vals(j)%str
                    end do
                end if
                call add_directory(cwd) ! *****
            else
                if (cwd=='/') then
                    cwd = cwd//dir
                else
                    cwd = cwd//'/'//dir
                end if
                call add_directory(cwd) ! *****
            end if
        else if (line(1:4)=='$ ls') then
            ! process each file/directory in the ls:
            do
                if (i==n_lines) exit
                line = read_line(iunit); i = i + 1
                if (line=='') cycle
                if (line(1:1)=='$') then
                    cycle main ! done
                else if (line(1:1)>='0' .and. line(1:1)<='9') then
                    ! a file to create
                    vals = split(line,' ')
                    call add_file(filesize=int(vals(1)))
                else if (line(1:3)=='dir') then
                    ! note: don't process until we cd into it
                end if
            end do
        end if
        if (i==n_lines) exit
        line = read_line(iunit); i = i + 1
    end do main
    close(iunit)

    isize = 0
    do i = 1, size(directories)
        if (sizes(i)<=100000) isize = isize + sizes(i)
    end do
    write(*,*) '7a: ', isize

    ! smallest directory to delete so that total is <= 30000000
    isize = huge(1)
    do i = 1, size(sizes)
        if (70000000 - (sizes(1) - sizes(i)) >= 30000000 .and. &
            sizes(i) < isize) isize = sizes(i)
    end do
    write(*,*) '7b: ', isize

    contains

        subroutine add_directory(dir)
        character(len=*),intent(in) :: dir
        type(string) :: v

        integer :: i, n_dirs

        v%str = dir

        if (allocated(directories)) then
            n_dirs = size(directories)
            do i = 1, n_dirs
                if (directories(i)%str==dir) return ! already added. nothing to do
            end do
            directories = [directories,v]
            sizes = [sizes,0]
        else
            directories = [v]
            sizes = [0]
        end if

        end subroutine add_directory

        subroutine add_file(filesize)
        ! add a file to the cwd
        integer,intent(in) :: filesize

        integer :: i

        ! update file size for this directory and all parent directories
        do i = 1, size(directories)
            if (index(cwd,directories(i)%str)==1) sizes(i) = sizes(i) + filesize
        end do

        end subroutine add_file

end program problem_7