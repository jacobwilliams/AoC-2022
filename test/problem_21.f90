program problem_21

    use aoc_utilities
    use iso_fortran_env, ip => int64
    use dag_module, only: dag

    implicit none

    integer :: iunit, n_rows, istart
    integer :: i, j, k, n_tokens, iroot
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals
    type(dag) :: d
    integer,dimension(:),allocatable :: order
    integer :: istat

    type :: monkey
        character(len=4) :: name = ''
        integer(ip) :: value = 0
        character(len=1) :: operator = ''
        integer,dimension(2) :: depends_on
    end type monkey
    type(monkey),dimension(:),allocatable :: monkeys

    open(newunit=iunit,file='inputs/day21.txt', status='OLD')
    n_rows = number_of_lines_in_file(iunit)
    allocate(monkeys(n_rows))
    call d%set_vertices(n_rows)

    ! first just get all the names:
    rewind(iunit)
    do i = 1, n_rows
        read(iunit,'(A4)') monkeys(i)%name
    end do
    !write(*,'(A,1x,*(a,","))') 'monkeys = ', monkeys%name

    rewind(iunit)
    do i = 1, n_rows
        line = read_line(iunit)
        vals = split(line, ' ')
        n_tokens = size(vals)
        monkeys(i)%name = vals(1)%str(1:4)
        if (monkeys(i)%name=='root') iroot = i
        select case (n_tokens)
        case(2) ! name val
            monkeys(i)%value = int(vals(2))
        case(4) ! name name operator name
            monkeys(i)%operator = vals(3)%str
            monkeys(i)%depends_on(1) = find(vals(2)%str)
            monkeys(i)%depends_on(2) = find(vals(4)%str)
            call d%set_edges(i,monkeys(i)%depends_on)
        case default; error stop 'invalid row'
        end select
        call d%set_vertex_info(i,monkeys(i)%name)
    end do
    close(iunit)
    ! create a dag for visualization and toposort:
    call d%save_digraph('monkeys.dot','RL',300)
    call execute_command_line('dot -Tpdf -o monkeys.pdf monkeys.dot')
    call d%toposort(order,istat)
    call d%destroy()

    ! now evaluate them all:
    do i = 1, n_rows
        call evaluate(monkeys(order(i)))
    end do
    write(*,*) '21a: ', monkeys(iroot)%value

    contains

        subroutine evaluate(me)
        class(monkey),intent(inout) :: me
        select case (me%operator)
        case('+'); me%value = monkeys(me%depends_on(1))%value + monkeys(me%depends_on(2))%value
        case('-'); me%value = monkeys(me%depends_on(1))%value - monkeys(me%depends_on(2))%value
        case('*'); me%value = monkeys(me%depends_on(1))%value * monkeys(me%depends_on(2))%value
        case('/'); me%value = monkeys(me%depends_on(1))%value / monkeys(me%depends_on(2))%value
        end select
        end subroutine evaluate

        ! because I can't get findloc to work
        integer function find(val)
        character(len=4),intent(in) :: val
        integer :: i
        do i = 1, n_rows
            if (monkeys(i)%name==val) then
                find = i
                return
            end if
        end do
        error stop 'val not found'
        end function find

end program problem_21
