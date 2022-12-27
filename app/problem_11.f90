program problem_11

    use iso_fortran_env, ip => int64
    use aoc_utilities

    implicit none

    integer(ip),parameter :: n_monkeys = 8

    type :: monkey
        integer(ip),dimension(:),allocatable :: items
        integer(ip) :: n_inspects = 0
    end type monkey
    type(monkey),dimension(0:n_monkeys-1) :: monkeys
    integer(ip) :: result

    call go(20_ip,.true.);     write(*,*) '11a: ', result
    call go(10000_ip,.false.); write(*,*) '11b: ', result

    contains

    subroutine go(n_rounds, divide)
        integer(ip),intent(in) :: n_rounds
        logical,intent(in) :: divide

        integer(ip),dimension(n_monkeys) :: inspects
        integer(ip) :: iturn, iround, i

        ! initialize:
        monkeys(0)%items = [83, 62, 93]
        monkeys(1)%items = [90, 55]
        monkeys(2)%items = [91, 78, 80, 97, 79, 88]
        monkeys(3)%items = [64, 80, 83, 89, 59]
        monkeys(4)%items = [98, 92, 99, 51]
        monkeys(5)%items = [68, 57, 95, 85, 98, 75, 98, 75]
        monkeys(6)%items = [74]
        monkeys(7)%items = [68, 64, 60, 68, 87, 80, 82]
        monkeys(:)%n_inspects = 0

        do iround = 1, n_rounds
            do iturn = 0, n_monkeys-1
                do i = 1, size(monkeys(iturn)%items)
                    associate (w => monkeys(iturn)%items(i))
                        select case(iturn)
                        case(0); w = w * 17; call throw(w,2_ip, i,iturn,[1_ip,6_ip],divide)
                        case(1); w = w + 1;  call throw(w,17_ip,i,iturn,[6_ip,3_ip],divide)
                        case(2); w = w + 3;  call throw(w,19_ip,i,iturn,[7_ip,5_ip],divide)
                        case(3); w = w + 5;  call throw(w,3_ip, i,iturn,[7_ip,2_ip],divide)
                        case(4); w = w * w;  call throw(w,5_ip, i,iturn,[0_ip,1_ip],divide)
                        case(5); w = w + 2;  call throw(w,13_ip,i,iturn,[4_ip,0_ip],divide)
                        case(6); w = w + 4;  call throw(w,7_ip, i,iturn,[3_ip,2_ip],divide)
                        case(7); w = w * 19; call throw(w,11_ip,i,iturn,[4_ip,5_ip],divide)
                        end select
                    end associate
                end do
                monkeys(iturn)%items = pack(monkeys(iturn)%items, mask=monkeys(iturn)%items>0)
            end do

        end do

        inspects = monkeys(:)%n_inspects
        call sort_ascending_64(inspects)
        result = product(inspects(n_monkeys-1:n_monkeys))

    end subroutine go

    subroutine throw(w,test,i,imonkey_from,imonkey_to,divide)
        integer(ip),intent(inout) :: w
        integer(ip),intent(in) :: test
        integer(ip),intent(in) :: i ! index in from array
        integer(ip),intent(in) :: imonkey_from ! monkey throwing it
        integer(ip),dimension(2),intent(in) :: imonkey_to ! true/false monkey to throw to
        logical,intent(in) :: divide

        monkeys(imonkey_from)%n_inspects = monkeys(imonkey_from)%n_inspects + 1
        if (divide) then
            w = w / 3_ip
        else
            ! hint from r/adventofcode solution megathread
            ! this is to avoid integer overflow
            w = mod(w,2_ip*17_ip*19_ip*3_ip*5_ip*13_ip*7_ip*11_ip)
        end if

        if (mod(w,test)==0) then
            monkeys(imonkey_to(1))%items = [monkeys(imonkey_to(1))%items , w]
        else
            monkeys(imonkey_to(2))%items = [monkeys(imonkey_to(2))%items , w]
        end if

        monkeys(imonkey_from)%items(i) = 0 ! mark for removal

    end subroutine throw

end program problem_11