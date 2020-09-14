!----------------------------------------------------------------------------------------
program dots_ncurses ! @(#) random dots on screen till ctrl-C
!(LICENSE:PD)
use M_ncurses, nc_move=>move
    implicit none
    integer          :: x, y
    integer          :: z
    integer(c_long)  :: p
    integer          :: fg, bg
    integer          :: pair
    real             :: r
    real             :: c
    integer          :: ierr
    stdscr=initscr()
    if (has_colors()) then
        ierr=start_color()
        call getcolor(COLORS,COLOR_PAIRS)
	COLORS=min(8,COLORS)  !! this code does not define color pairs so keep to 8
        do fg=0,COLORS-1
            do bg=0,COLORS-1
                pair = mypair(fg, bg)
                if (pair > 0)then
                    ierr=init_pair(int(pair,c_short),int(fg,c_short),int(bg,c_short))
                endif
            enddo
        enddo
    endif
    call getmaxyx(stdscr,LINES,COLS)
    r = LINES - 4
    c = COLS - 4
    fg = COLOR_WHITE
    bg = COLOR_BLACK
    INFINITE: do
        x=int((c*ranf())+2)
        y=int((r*ranf())+2)
        if (ranf() > 0.9) then
           p=ichar("*")
        else
           p=ichar(" ")
        endif
        ierr=nc_move(y, x)
        if (has_colors()) then
            z=int(ranf()*COLORS)
            if (ranf() > 0.01) then
                call set_colors(z, bg)
                ierr=attron(COLOR_PAIR(mypair(fg, bg)))
             else
                call set_colors(fg, z)
                ierr=napms(1)
            endif
        else
            if (ranf() <= 0.01) then
                if (ranf() > 0.6) then
                    ierr=attron(A_REVERSE)
                else
                    ierr=attroff(A_REVERSE)
                endif
                ierr=napms(1)
            endif
        endif
        ierr=addch(p)
        ierr=refresh()
    enddo INFINITE
    ierr= endwin()
CONTAINS
!----------------------------------------------------------------------------------------
INTEGER FUNCTION mypair(fg,bg)
   IMPLICIT NONE
   INTEGER          :: fg, bg
   pair = (fg * COLORS) + bg
   IF (pair >= COLOR_PAIRS)THEN
      mypair=-1
   ELSE
      mypair=pair
   ENDIF
END FUNCTION mypair
!----------------------------------------------------------------------------------------
SUBROUTINE set_colors(fg, bg)
   IMPLICIT NONE
   INTEGER        :: fg, bg
   IF (mypair(fg,bg) > 0) THEN
      ierr=attron(COLOR_PAIR(mypair(fg, bg)))
   ENDIF
END SUBROUTINE set_colors
!----------------------------------------------------------------------------------------
FUNCTION ranf() result (r)
   IMPLICIT NONE
   REAL :: r
   LOGICAL,SAVE :: already_run=.TRUE.
   IF(.NOT.already_run)THEN
      CALL init_random_seed_by_clock()
      already_run=.TRUE.
   ENDIF
   CALL random_number(r)
END FUNCTION ranf
!----------------------------------------------------------------------------------------
SUBROUTINE init_random_seed_by_clock()
   implicit none
   INTEGER :: i, n, clock
   INTEGER, DIMENSION(:), ALLOCATABLE :: seed
   CALL RANDOM_SEED(size = n)
   ALLOCATE(seed(n))
   CALL SYSTEM_CLOCK(COUNT=clock)
   seed = clock + 37 * (/ (i - 1, i = 1, n) /)
   CALL RANDOM_SEED(PUT = seed)
   DEALLOCATE(seed)
END SUBROUTINE init_random_seed_by_clock
!----------------------------------------------------------------------------------------
END PROGRAM dots_ncurses
