program problem_21

    use aoc_utilities
    use iso_fortran_env, ip => int64, wp => real64
    use dag_module, only: dag

    implicit none

    integer :: iunit, n_rows, istart
    integer :: i, j, k, n_tokens, iroot, ihumn
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals
    type(dag) :: d
    integer,dimension(:),allocatable :: order
    integer :: istat
    real(wp) :: x, correction

    integer,parameter :: maxiter = 100
    real(wp),parameter :: tol = 1.0e-5_wp ! tolerance for newton

    type :: monkey
        character(len=4) :: name = ''
        real(wp) :: value = 0 ! for part b, let's make it a real number
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
        if (monkeys(i)%name=='humn') ihumn = i ! for part b
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
    call d%save_digraph('21.dot','RL',300)
    call execute_command_line('dot -Tpdf -o 21.pdf 21.dot')
    call d%toposort(order,istat)
    call d%destroy()

    ! now evaluate them all:
    call evaluate_all()
    write(*,*) '21a: ', int(monkeys(iroot)%value, ip)

    ! for part b, will modify the dag to be a 1x1 equation solver,
    ! and use a simple newton's method to solve it.
    monkeys(iroot)%operator = '-' ! we want to constrain the root value to zero
    x = monkeys(ihumn)%value ! initial guess
    do i = 1, maxiter
        correction = f(x) / df(x)
        x = x - correction
        if (abs(correction)<tol) exit
    end do
    write(*,*) '21b: ', int(anint(x, wp),ip)

    contains

        real(wp) function f(x) ! function for newton
        real(wp),intent(in) :: x
        monkeys(ihumn)%value = x
        call evaluate_all()
        f = monkeys(iroot)%value
        end function f

        real(wp) function df(x) ! derivative function for newton
        real(wp),intent(in) :: x
        real(wp),parameter :: delx = 0.001_wp
        df = ( f(x+delx) - f(x-delx) ) / (2.0_wp * delx)
        end function df

        subroutine evaluate_all()
            integer :: i
            do i = 1, n_rows
                call evaluate(monkeys(order(i)))
            end do
        end subroutine evaluate_all

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
