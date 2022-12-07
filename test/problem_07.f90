program problem_7

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer :: i, j, k, n, iunit, n_lines, iout
    character(len=:),allocatable :: line, dir, cwd
    type(string),dimension(:),allocatable :: vals

    ! a structure to hold the information:
    type :: file
        character(len=:),allocatable :: name
        integer :: size = 0
    end type file
    type,extends(file) :: directory
        type(file),dimension(:),allocatable :: files
        type(directory),dimension(:),allocatable :: dirs
    end type directory

    open(newunit=iout, file='commands.sh', action='WRITE', status='REPLACE')

    call command('#!/bin/bash')
    call command('rm -rf ./RESULTS')
    call command('mkdir ./RESULTS')

    open(newunit=iunit, file='inputs/day7-test.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)


    !.. note:
    !  keep track of full path
    !  any new file size added to current and all parent paths
    !  ... but also have to keep track of file name so we don't duplicate? ?

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
                write(*,*) 'cwd = '//cwd
                dir = './RESULTS'  ! for the bash script
            else if (dir=='..') then
                vals = split(cwd,'/')
                cwd = '/'
                do j = 2, size(vals)-1
                    cwd = cwd//vals(j)%str
                end do
                write(*,*) 'cwd = '//cwd
            else
                if (cwd=='/') then
                    cwd = cwd//dir
                else
                    cwd = cwd//'/'//dir
                end if
                write(*,*) 'cwd = '//cwd
            end if
            call command('cd '//dir)
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

                    ! put the file size in the file:
                    call command('echo '//vals(1)%str//' > '//vals(2)%str)
                else if (line(1:3)=='dir') then
                    vals = split(line,' ')
                    ! make a directory:
                    call command('mkdir -p '//vals(2)%str)
                end if
            end do
        end if

        if (i==n_lines) exit
        line = read_line(iunit); i = i + 1
    end do main

    close(iunit)
    close(iout)

    ! generate the files:
    ! call execute_command_line('chmod +x ./commands.sh')
    ! call execute_command_line('./commands.sh')
    ! call execute_command_line('tree ./RESULTS')

    contains

        subroutine command(cmd)
        character(len=*),intent(in) :: cmd
        !write(*,'(A)') cmd
        write(iout,'(A)') cmd
        !call execute_command_line(cmd)
        end subroutine command

end program problem_7