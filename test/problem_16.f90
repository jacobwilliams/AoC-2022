program problem_16

    use aoc_utilities
    use iso_fortran_env, ip => int32
    use dag_module, only: dag

    implicit none

    integer :: iunit, n_valves, istart, itime
    integer(ip) :: i, j, k
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals, vals2
    type(dag) :: d
    integer :: n_tunnels, n_tokens
    character(len=8) :: tmp
    integer,dimension(:),allocatable :: tunnels
    character(len=2),dimension(:),allocatable :: v
    integer :: imax(1)

    integer,parameter :: max_minutes = 30 ! max minutes for part a

    type :: valve
        character(len=2) :: name = ''
        integer :: flow_rate = 0
        integer,dimension(:),allocatable :: tunnels ! leads to these valves
    end type valve
    type(valve),dimension(:),allocatable :: valves

    type :: valve_states
        integer,dimension(:),allocatable :: open ! minute valve was opened (>0)
    end type valve_states

    integer,dimension(:),allocatable :: iset ! indices in valves
    integer,dimension(:),allocatable :: iflow ! current flow rates
    type(valve_states),dimension(:),allocatable :: istates ! state of each valve (open/close)
    integer,dimension(:),allocatable :: ipressure

    integer,dimension(:),allocatable :: iset_tmp
    integer,dimension(:),allocatable :: iflow_tmp
    type(valve_states),dimension(:),allocatable :: istates_tmp
    integer :: itrim

    open(newunit=iunit,file='inputs/day16.txt', status='OLD')
    n_valves = number_of_lines_in_file(iunit)
    allocate(v(n_valves))
    allocate(valves(n_valves))
    call d%set_vertices(n_valves)

    ! Valve AZ has flow rate=12; tunnels lead to valves QF, PI, AS, PE

    ! first just get all the valves:
    rewind(iunit)
    do i = 1, n_valves
        read(iunit,'(A8)') tmp
        v(i) = tmp(7:8)
        valves(i)%name = v(i)
        if (v(i)=='AA') istart = i ! find starting point
    end do
    !write(*,'(A,1x,*(a,","))') 'valves = ', v

    ! parse the line:
    rewind(iunit)
    do i = 1, n_valves
        line = read_line(iunit)
        vals = split(line, ' ')
        n_tokens = size(vals)
        valves(i)%flow_rate = int(vals(5)%str(6:len(vals(5)%str)-1))
        n_tunnels = n_tokens - 9
        if (allocated(tunnels)) deallocate(tunnels)
        allocate(tunnels(n_tunnels))
        do j = 1,n_tunnels
            associate (s => vals(9+j)%str)
                if (j<n_tunnels) then
                    tunnels(j) = find(v,s(1:len(s)-1))
                else
                    tunnels(j) = find(v,s)
                end if
            end associate
        end do
        call d%set_edges(i,tunnels)
        call d%set_vertex_info(i,v(i))
        valves(i)%tunnels = tunnels
    end do
    close(iunit)

    ! create a dag for visualization:
    call d%save_digraph('16.dot','RL',300)
    call d%destroy()
    call execute_command_line('dot -Tpdf -o 16.pdf 16.dot')

    ! now traverse valves with BFS, starting at istart (AA):
    iset = [istart]
    iflow = [0] ! initialize
    allocate(istates(1)); allocate(istates(1)%open(n_valves)); istates(1)%open = -1 ! not open
    ! write(*,*) 'start=',valves(istart)%name

    !do itime = 0, max_minutes-1
    do itime = 1, max_minutes
        call advance(iset, iflow, istates)
        ! write(*,*) 'itime = ', itime, &
        !            ' # candidates: ', size(iset), &
        !            ' max flow: ', maxval(iflow), &
        !            ' # zero flow: ', count(iflow<maxval(iflow)/10)

        ! eliminate the ones with very low flows:
        if (size(iset)>2000) then
            if (allocated(iset_tmp))    deallocate(iset_tmp);    allocate(iset_tmp(0))
            if (allocated(iflow_tmp))   deallocate(iflow_tmp);   allocate(iflow_tmp(0))
            if (allocated(istates_tmp)) deallocate(istates_tmp); allocate(istates_tmp(0))

            if (size(iset)>5000) then
                itrim = 5*maxval(iflow)/6
            else
                itrim = 2*maxval(iflow)/3
            end if
            do i = 1, size(iset)
                if (iflow(i)>itrim) then
                    iset_tmp    = [iset_tmp,    iset(i)   ]
                    iflow_tmp   = [iflow_tmp,   iflow(i)  ]
                    istates_tmp = [istates_tmp, istates(i)]
                end if
            end do
            call move_alloc(iset_tmp   , iset)
            call move_alloc(iflow_tmp  , iflow)
            call move_alloc(istates_tmp, istates)
        end if

    end do
   !write(*,*) 'maxval(iflow): ', maxval(iflow)

    allocate(ipressure(size(iset))); ipressure = 0
    do j = 1, size(iset)
        do i = 1, n_valves
            if (istates(j)%open(i)>0) ipressure(j) = ipressure(j) + &
                                        valves(i)%flow_rate*(max_minutes-istates(j)%open(i))
        end do
    end do

    ! imax = maxloc(ipressure)
    ! associate (im => imax(1))
    !     write(*,'(a,1x,*(a3,","))') 'valves%name         = ', valves%name
    !     write(*,'(a,1x,*(i3,","))') 'istates(im)%open    = ', istates(im)%open
    !     write(*,'(a,1x,*(i3,","))') 'valves(i)%flow_rate = ', valves%flow_rate
    ! end associate

    write(*,*) '16a: ', maxval(ipressure)

    contains

        recursive subroutine advance(iset, iflow, istates)
            ! advance 1 minute.
            integer,dimension(:),allocatable,intent(inout) :: iset ! indices in valves
            integer,dimension(:),allocatable,intent(inout) :: iflow ! current flow rates
            type(valve_states),dimension(:),allocatable,intent(inout) :: istates ! state of each valve (open/close)

            integer,dimension(:),allocatable :: iset_out
            integer,dimension(:),allocatable :: iflow_out
            type(valve_states),dimension(:),allocatable :: istates_out
            integer :: i, j, ivalve, iflow_new, dest_ivalve
            type(valve_states) :: tmp_state
            logical :: open_valve

            allocate(iset_out(0))
            allocate(iflow_out(0))
            allocate(istates_out(0))

            do i = 1, size(iset)

                ivalve = iset(i) ! where we are

                ! where do we go from here...

                ! do we open the valve or move?
                if (istates(i)%open(ivalve)>0 .or. valves(ivalve)%flow_rate==0) then
                    ! in this case, only ioption is to move
                else
                    ! option to open the current valve

                    ! if we open the valve, this would be the flow rate:
                    iflow_new = valves(ivalve)%flow_rate + iflow(i)

                    tmp_state              = istates(i) ! copy current state
                    tmp_state%open(ivalve) = itime      ! open it

                    ! ignore any duplicates:
                    open_valve = .true.
                    do k = 1, size(iset_out)
                        if (iset_out(k)==ivalve .and. &
                            iflow_out(k)==iflow_new .and. &
                            all(istates_out(k)%open == tmp_state%open)) then
                                open_valve = .false.
                                exit
                        end if
                    end do
                    if (open_valve) then
                        iset_out    = [iset_out,ivalve]
                        iflow_out   = [iflow_out,iflow_new]  ! increase flow rate
                        istates_out = [istates_out,tmp_state]
                    end if

                end if

                ! option to move:
                move: do j = 1, size(valves(ivalve)%tunnels)
                    dest_ivalve = valves(ivalve)%tunnels(j) ! destination valve

                    ! ignore any duplicates:
                    do k = 1, size(iset_out)
                        if (iset_out(k)==dest_ivalve .and. &
                            iflow_out(k)==iflow(i) .and. &
                            all(istates_out(k)%open == istates(i)%open)) cycle move
                    end do

                    ! option to move to this valve
                    iset_out    = [iset_out,dest_ivalve]
                    iflow_out   = [iflow_out,iflow(i)]      ! same flow rate
                    istates_out = [istates_out,istates(i)] ! same state
                end do move

            end do

            ! update for next:
            call move_alloc(iset_out,    iset)
            call move_alloc(iflow_out,   iflow)
            call move_alloc(istates_out, istates)

        end subroutine advance

        ! because I can't get findloc to work
        integer function find(array,val)
        character(len=2),dimension(:),intent(in) :: array
        character(len=2),intent(in) :: val
        integer :: i
        do i = 1, size(array)
            if (array(i)==val) then
                find = i
                return
            end if
        end do
        error stop 'val not found'
        end function find

end program problem_16
