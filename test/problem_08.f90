program problem_8

    use aoc_utilities

    implicit none

    integer :: i, j, k, iunit, n_lines, n_visible, n_cols, score(4), best_score
    character(len=:),allocatable :: line
    integer,dimension(:,:),allocatable :: iarray

    open(newunit=iunit, file='inputs/day8.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)

    do i = 1, n_lines
        line = read_line(iunit)
        if (i==1) then
            n_cols = len(line)
            allocate(iarray(n_lines, n_cols))
        end if
        do j = 1, n_cols
            read(line(j:j),'(I1)') iarray(i,j)
        end do
    end do
    n_visible = 2*n_cols + 2*(n_lines-2) ! outer edge

    do i = 2, n_lines-1
        do j = 2, n_cols-1
            if (all(iarray(1:i-1,j)<iarray(i,j)) .or. &
                all(iarray(i,1:j-1)<iarray(i,j)) .or. &
                all(iarray(i+1:,j)<iarray(i,j)) .or. &
                all(iarray(i,j+1:)<iarray(i,j))  ) n_visible = n_visible + 1
        end do
    end do
    write(*,*) '8a: n_visible', n_visible

    best_score = 0
    do i = 2, n_lines-1 ! row
        do j = 2, n_cols-1 ! col
            score = 0
            do k = i-1,1,-1  !up
                score(1) = score(1) + 1
                if (iarray(k,j)>=iarray(i,j)) exit
            end do
            do k = i+1,n_lines !down
                score(2) = score(2) + 1
                if (iarray(k,j)>=iarray(i,j)) exit
            end do
            do k = j-1,1,-1 !left
                score(3) = score(3) + 1
                if (iarray(i,k)>=iarray(i,j)) exit
            end do
            do k = j+1,n_cols !right
                score(4) = score(4) + 1
                if (iarray(i,k)>=iarray(i,j)) exit
            end do
            if (product(score)>best_score) best_score = product(score)
        end do
    end do
    write(*,*) '8b: best score', best_score

end program problem_8