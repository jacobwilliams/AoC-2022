program problem_11

    use iso_fortran_env
    use aoc_utilities

    implicit none

    integer,parameter :: n_monkeys = 8
    ! integer,parameter :: n_monkeys = 4 ! test

    integer :: iturn, iround, n_items, i
    type :: monkey
        integer,dimension(:),allocatable :: items
        integer :: n_inspects = 0
    end type monkey
    type(monkey),dimension(0:n_monkeys-1) :: monkeys
    integer,dimension(n_monkeys) :: inspects

    ! initialize:
    monkeys(0)%items = [83, 62, 93]
    monkeys(1)%items = [90, 55]
    monkeys(2)%items = [91, 78, 80, 97, 79, 88]
    monkeys(3)%items = [64, 80, 83, 89, 59]
    monkeys(4)%items = [98, 92, 99, 51]
    monkeys(5)%items = [68, 57, 95, 85, 98, 75, 98, 75]
    monkeys(6)%items = [74]
    monkeys(7)%items = [68, 64, 60, 68, 87, 80, 82]

    ! monkeys(0)%items = [79, 98]
    ! monkeys(1)%items = [54, 65, 75, 74]
    ! monkeys(2)%items = [79, 60, 97]
    ! monkeys(3)%items = [74]

    do iround = 1, 20
        do iturn = 0, n_monkeys-1
            n_items = size(monkeys(iturn)%items)
            do i = 1, n_items
                associate (w => monkeys(iturn)%items(i))
                    select case(iturn)
                    case(0); w = w * 17; call throw(w,2, i,iturn,[1,6])
                    case(1); w = w + 1;  call throw(w,17,i,iturn,[6,3])
                    case(2); w = w + 3;  call throw(w,19,i,iturn,[7,5])
                    case(3); w = w + 5;  call throw(w,3, i,iturn,[7,2])
                    case(4); w = w * w;  call throw(w,5, i,iturn,[0,1])
                    case(5); w = w + 2;  call throw(w,13,i,iturn,[4,0])
                    case(6); w = w + 4;  call throw(w,7, i,iturn,[3,2])
                    case(7); w = w * 19; call throw(w,11,i,iturn,[4,5])

                    ! case(0); w = w * 19; call throw(w,23, i,iturn,[2,3])
                    ! case(1); w = w + 6;  call throw(w,19, i,iturn,[2,0])
                    ! case(2); w = w * w;  call throw(w,13, i,iturn,[1,3])
                    ! case(3); w = w + 3;  call throw(w,17, i,iturn,[0,1])

                    end select
                end associate
            end do
            monkeys(iturn)%items = pack(monkeys(iturn)%items, mask=monkeys(iturn)%items>0)
        end do
    end do

    ! do i = 0, n_monkeys-1
    !     write(*,*) 'monkey', i, 'inspected',monkeys(i)%n_inspects
    ! end do

    inspects = monkeys(:)%n_inspects
    call sort_ascending(inspects)
    write(*,*) '11a: ', product(inspects(n_monkeys-1:n_monkeys))

    contains

        subroutine throw(w,test,i,imonkey_from,imonkey_to)
            integer,intent(inout) :: w
            integer,intent(in) :: test
            integer,intent(in) :: i ! index in from array
            integer,intent(in) :: imonkey_from ! monkey throwing it
            integer,dimension(2),intent(in) :: imonkey_to ! true/false monkey to throw to

            monkeys(imonkey_from)%n_inspects = monkeys(imonkey_from)%n_inspects + 1
            w = w / 3
            if (mod(w,test)==0) then
                monkeys(imonkey_to(1))%items = [monkeys(imonkey_to(1))%items , w]
            else
                monkeys(imonkey_to(2))%items = [monkeys(imonkey_to(2))%items , w]
            end if
            monkeys(imonkey_from)%items(i) = 0 ! mark for removal

        end subroutine throw

end program problem_11