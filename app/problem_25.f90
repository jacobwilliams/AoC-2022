program problem_25

    use iso_fortran_env, ip => int64, wp => real64
    use aoc_utilities
    use pikaia_module

    implicit none

    integer :: i, iunit
    integer(ip) :: isum, ierror
    character(len=:),allocatable :: s

    open(newunit=iunit, file='inputs/day25.txt', status='OLD')
    isum = 0_ip
    do i = 1, number_of_lines_in_file(iunit)
        isum = isum + snafu2int(read_line(iunit))
    end do
    close(iunit)
    call int2snafu(isum,s)
    ierror = snafu2int(s) - isum

    if (ierror==0) then
        write(*,*) '25a: '//s
    else
        write(*,*) 'could not find solution. best one found:'
        write(*,*) ' snafu     = '//s
        write(*,*) ' sum       = ', isum
        write(*,*) ' snafu2int = ', snafu2int(s)
        write(*,*) ' error     = ', ierror
    end if

    contains

    subroutine int2snafu(ival,s)
        ! use a genetic algorithm to guess the snafu string
        ! corresponding to the isum value.
        integer(ip),intent(in) :: ival
        character(len=:),allocatable,intent(out) :: s

        integer :: n ! max snafu digits
        type(pikaia_class) :: p
        real(wp),dimension(:),allocatable :: x,xl,xu
        integer :: status,iseed
        real(wp) :: f
        integer(ip) :: i

        ! first get the number of snafu digits:
        i = 0_ip
        do
            i = i + 1_ip
            if ((5_ip**i)>=isum) exit
        end do
        n = i
        !write(*,*) 'max digits: ', n

        iseed = 371    ! set random number seed
        allocate(x(n))
        allocate(xl(n))
        allocate(xu(n))
        xl = -2.0_wp   ! bounds
        xu = 2.0_wp
        x = 0.0_wp     ! initial guess

        call p%init(n,xl,xu,obj_func,status,&
                    ngen = 5000, &
                    np = 1000, &
                    nd = 1, & ! only need one significant digit here
                    ivrb = 0, &
                    convergence_tol = 1.0e-6_wp, &
                    convergence_window = 1000, &
                    initial_guess_frac = 0.5_wp, &
                    iseed = iseed )

        call p%solve(x,f,status)

        if (status==0) then
            s = real_snafu2string(x)
        else
            s = 'UNKNOWN'
        end if

    end subroutine int2snafu

    subroutine obj_func(me,x,f)  ! Pikaia fitness function
    class(pikaia_class),intent(inout) :: me !pikaia class
    real(wp),dimension(:),intent(in) :: x !optimization variable vector
    real(wp),intent(out) :: f !fitness value (to maximize)
    f = - real( abs(real_snafu2int(x) - isum), wp )
    end subroutine obj_func

    function snafu2int(s) result(ival)
        character(len=*),intent(in) :: s
        integer(ip) :: ival
        integer(ip) :: i, j, f, n
        ival = 0_ip
        n = len(s)
        do i = n, 1, -1
            f = 5**(n-i)
            select case (s(i:i))
            case('2'); ival = ival + 2_ip*f
            case('1'); ival = ival + f
            case('0') ! ival = ival + 0
            case('-'); ival = ival - f
            case('='); ival = ival - 2_ip*f
            end select
        end do
    end function snafu2int

    function real_snafu2int(x) result(ival)
        real(wp),dimension(:),intent(in) :: x
        integer(ip) :: ival
        integer(ip) :: i, j, n
        ival = 0_ip
        n = size(x)
        do i = n, 1, -1
            ival = ival + int(anint(x(i)),ip)*5**(n-i)
        end do
    end function real_snafu2int

    function real_snafu2string(x) result(s)
        real(wp),dimension(:),intent(in) :: x
        character(len=:),allocatable :: s
        character(len=1),dimension(-2:2),parameter :: tokens = ['=','-','0','1','2']
        integer(ip) :: i, j
        s = ''
        do i = size(x), 1, -1
            s = tokens(int(anint(x(i)),ip))//s
        end do
    end function real_snafu2string

end program problem_25
