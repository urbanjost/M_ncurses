! Test program, after testcurses.c
!(LICENSE:PD)
!
! iwin = C_NULL_PTR
!-------------------------------------------------------------------------------
module winsize
  use M_ncurses
  integer (C_INT) :: height, width
end module winsize
!-------------------------------------------------------------------------------
module commands
  use M_ncurses
  integer, parameter :: MAX_OPTIONS = 8
  character (len=14), dimension(MAX_OPTIONS) :: command =  &
  (/ 'Intro test    ',  &
     'Resize test   ',  &
     'Scroll test   ',  &
     'Input test    ',  &
     'Output test   ',  &
     'ACS test      ',  &
     'Colour test   ',  &
     'Clipboard test' /)
end module commands
!-------------------------------------------------------------------------------
subroutine initTest(iwin, istat)
  use M_ncurses
  use winsize
  type (C_PTR), intent(inout) :: iwin
  integer, intent(out) :: istat
  integer (C_INT) :: ierr

  istat=0
  iwin = C_NULL_PTR
  iwin = initscr()
  ! KLUDGE: LINES and COLS are not being set properly yet
  call getmaxyx(iwin,LINES,COLS)                            ! get the number of rows and columns 
  ierr = start_color()
  width=60
  height=13
  iwin=newwin(height, width, (LINES-height)/2, (COLS-width)/2)
  if (.not.c_associated(iwin)) then
    istat=1
  end if
end subroutine initTest
!-------------------------------------------------------------------------------
subroutine introtest(iwin, istat)
  use M_ncurses
  use winsize
  type (C_PTR), intent(inout) :: iwin
  integer, intent(out) :: istat
  integer (C_INT) :: ierr

  istat=0
  ierr=werase(iwin)
  ierr=wmove(iwin, height/2-5, width/2-10)
  ierr=wvline(iwin, acs_vline, 10)
  ierr=wmove(iwin, height/2, width/2-10)
  ierr=whline(iwin, acs_hline, 20)
  call cont(iwin)
  ierr=beep()
  ierr=werase(iwin)
  ierr=box(iwin, acs_vline, acs_hline)
  ierr=cbreak()
  ierr=mvwaddstr(iwin, 1, 1,  &
        'You should have a rectangle in the middle of the screen' // C_NULL_CHAR)
  ierr=mvwaddstr(iwin, 2, 1, 'You should have heard a beep' // C_NULL_CHAR)
  call cont(iwin)
  ierr=flash()
  ierr=mvwaddstr(iwin, 3, 1, 'You should have seen a flash' // C_NULL_CHAR)
  call cont(iwin)
end subroutine introtest
!-------------------------------------------------------------------------------
subroutine resizetest(istat)
  use M_ncurses
  integer, intent(out) :: istat

  type (C_PTR) :: win1
  integer (C_INT) :: nwidth = 135, nheight = 52
  integer (C_INT) :: ierr, owidth, oheight
  character (len=80) :: str

  istat=0
  owidth = COLS
  oheight = LINES

  ierr=savetty()
  ierr=resize_term(nheight, nwidth)
  if (ierr /= 0) then
    write(*,*) 'ERROR: failure in resize_term'
  end if
  ierr=clear()
  ierr=refresh()

  win1 = newwin(10, 50, 14, 25)
  if (.not.c_associated(win1)) then
    ierr=endwin()
    istat=-1
    return
  end if
  ierr=mvwaddstr(win1, 0, 0, 'The screen may now be resized' // C_NULL_CHAR)
  write(str, '(a,1x,i0,1x,a,1x,i0)') 'Given size:', nwidth, 'by', nheight
  ierr=mvwaddstr(win1, 1, 4, trim(str) // C_NULL_CHAR)
  write(str, '(a,1x,i0,1x,a,1x,i0)') 'Actual size:', COLS, 'by', LINES
  ierr=mvwaddstr(win1, 1, 4, trim(str) // C_NULL_CHAR)
  call cont(win1)

  ierr=wclear(win1)
  ierr=resetty()

  ierr=mvwaddstr(win1, 0, 0, 'The screen should now be reset' // C_NULL_CHAR)
  write(str, '(a,1x,i0,1x,a,1x,i0)') 'Old size:', owidth, 'by', oheight
  ierr=mvwaddstr(win1, 1, 4, trim(str) // C_NULL_CHAR)
  write(str, '(a,1x,i0,1x,a,1x,i0)') 'Size now:', COLS, 'by', LINES
  ierr=mvwaddstr(win1, 1, 4, trim(str) // C_NULL_CHAR)
  call cont(win1)

  ierr=delwin(win1)

  ierr=clear()
  ierr=refresh()
end subroutine resizetest
!-------------------------------------------------------------------------------
subroutine scrolltest(win, istat)
  use M_ncurses
  use winsize
  type (C_PTR), intent(inout) :: win
  integer, intent(out) :: istat
  integer (C_INT) :: ierr
  integer :: i, oldy

  istat=0
  ierr=werase(win)
  ierr=mvwaddstr(win, height - 2, 1,  &
                 'The window will now scroll slowly' // C_NULL_CHAR)
  ierr=box(win, acs_vline, acs_hline)
  ierr=wrefresh(win)
  ierr=scrollok(win, true)
  ierr=napms(500)

  do i = 1, height
    ierr=napms(150)
    ierr=scroll(win)
    ierr=wrefresh(win)
  end do

  oldy = getmaxy(win)
  ierr=mvwaddstr(win, 6, 1, 'The top of the window will scroll' // C_NULL_CHAR)
  ierr=wmove(win, 1, 1)
  ierr=wsetscrreg(win, 0, 4)
  ierr=box(win, acs_vline, acs_hline)
  ierr=wrefresh(win)

  do i = 1, 5
    ierr=napms(500)
    ierr=scroll(win)
    ierr=wrefresh(win)
  end do

  ierr=mvwaddstr(win, 3, 1,  &
                 'The bottom of the window will scroll' // C_NULL_CHAR)
  ierr=wmove(win, 8, 1)
  oldy=oldy-1
  ierr=wsetscrreg(win, 5, oldy)
  ierr=box(win, acs_vline, acs_hline)
  ierr=wrefresh(win)

  do i = 5, oldy
    ierr=napms(300)
    ierr=wscrl(win, -1)
    ierr=wrefresh(win)
  end do
  ierr=wsetscrreg(win, 0, oldy)
end subroutine scrolltest
!-------------------------------------------------------------------------------
subroutine outputtest(win)
  use M_ncurses
  type (C_PTR), intent(inout) :: win
  type (C_PTR) :: win1
  character (len=80) :: buffer
  integer (C_LONG) :: ch
  integer (C_INT) :: by, bx, ierr

  ierr=nl()
  ierr=wclear(win)
  ierr=mvwaddstr(win, 1, 1,  &
                 'You should now have a screen in the upper '  //  &
                 'left corner, and this text should have wrapped' // C_NULL_CHAR)
  ierr=waddstr(win,'\nThis text should be down\n' // C_NULL_CHAR)
  ierr=waddstr(win, 'and broken into two here ^' // C_NULL_CHAR)
  call cont(win)

  ierr=wclear(win)
  ierr=wattron(win, a_bold)
  ierr=mvwaddstr(win, 1, 1, 'A new window will appear with this text in it' // C_NULL_CHAR)
  ierr=mvwaddstr(win, 8, 1, 'Press any key to continue' // C_NULL_CHAR)
  ierr=wrefresh(win)
  ierr=wgetch(win)

  if (LINES < 24 .or. COLS < 75) then
    ierr=mvwaddstr(win, 5, 1, 'Some tests have been skipped as they require a' // C_NULL_CHAR)
    ierr=mvwaddstr(win, 6, 1, 'display of at least 24 LINES by 75 COLUMNS' // C_NULL_CHAR)
    call cont(win)
  else
    win1 = newwin(10, 50, 14, 25)
    if (.not.c_associated(win1)) then
      istat=1
      ierr=endwin()
      return
    end if
    ierr=wbkgd(win1, a_normal)

    ierr=wclear(win1)
    ierr=mvwaddstr(win1, 5, 1,  &
           'This text should appear; using overlay option' // C_NULL_CHAR)
    ierr=copywin(win, win1, 0, 0, 0, 0, 9, 49, 1)
    ierr=box(win1, acs_vline, acs_hline)
    ierr=wmove(win1, 8, 26)
    ierr=wrefresh(win1)
    ierr=wgetch(win1)

    ierr=wclear(win1)

    ierr=wattron(win1, a_blink)
    ierr=mvwaddstr(win1, 4, 1,  &
           'This blinking text should appear in only the second window' // C_NULL_CHAR)
    ierr=wattroff(win1, a_blink)

    !------------------------
    !ierr=mvwin(win1, by, bx)
    !------------------------
    ierr=overlay(win, win1)
    ierr=mvwin(win1, 14, 25)
    ierr=wmove(win1, 8, 26)
    ierr=wrefresh(win1)
    ierr=wgetch(win1)

    ierr=delwin(win1)
  end if

  ierr=clear()
  ierr=wclear(win)
  ierr=wrefresh(win)
  ierr=mvwaddstr(win, 6, 2, 'This line shouldn''t appear' // C_NULL_CHAR)
  ierr=mvwaddstr(win, 4, 2, 'Only half of the next line is visible' // C_NULL_CHAR)
  ierr=mvwaddstr(win, 5, 2, 'Only half of the next line is visible' // C_NULL_CHAR)
  ierr=wmove(win, 6, 1)
  ierr=wclrtobot(win)
  ierr=wmove(win, 5, 20)
  ierr=wclrtoeol(win)
  ierr=mvwaddstr(win, 8, 2, 'This line also shouldn''t appear' // C_NULL_CHAR)
  ierr=wmove(win, 8, 1)
  ierr=winsdelln(win, -1)
  call cont(win)

  ierr=wmove(win, 5, 9)
  ch = winch(win)

  ierr=wclear(win)
  ierr=wmove(win, 6, 2)
  ierr=waddstr(win, 'The next char should be l:  ' // C_NULL_CHAR)
  ierr=winsch(win, ch)
  call cont(win)

  ierr=mvwinsstr(win, 6, 2, 'A1B2C3D4E5' // C_NULL_CHAR)
  call cont(win)

  ierr=wmove(win, 5, 1)
  ierr=winsdelln(win, 1)
  ierr=mvwaddstr(win, 5, 2, 'The lines below should have moved down' // C_NULL_CHAR)
  call cont(win)

  ierr=wclear(win)
  ierr=wmove(win, 2, 2)
  write(buffer, '(a,1x,i0,1x,a)')  &
    'This is a formatted string in a window:', 42, 'is it'
  ierr=mvwaddstr(win, 1, 1, trim(buffer) // C_NULL_CHAR)
  ierr=mvwaddstr(win, 10, 1, 'Enter a string: ' // C_NULL_CHAR)
  ierr=wrefresh(win)
  ierr=echo()
! ierr=wscanw(win, "%s", buffer)
! write(*,*) 'Entered: ', trim(buffer)
  ierr=mvaddstr(10, 1, 'Enter a string: ' // C_NULL_CHAR)
! ierr=scanw("%s", buffer)
! write(*,*) 'Entered: ', trim(buffer)

  ierr=wclear(win)
  ierr=curs_set(2)
  ierr=mvwaddstr(win, 1, 1, 'The cursor should be in high-visibility mode' // C_NULL_CHAR)
  call cont(win)

  ierr=wclear(win)
  ierr=curs_set(0)
  ierr=mvwaddstr(win, 1, 1, 'The cursor should have disappeared' // C_NULL_CHAR)
  call cont(win)

  ierr=wclear(win)
  ierr=curs_set(1);
  ierr=mvwaddstr(win, 1, 1, 'The cursor should be normal' // C_NULL_CHAR)
  call cont(win)
end subroutine outputtest
!-------------------------------------------------------------------------------
subroutine acstest(win)
  use M_ncurses
  type (C_PTR), intent(inout) :: win
  integer, parameter :: ACSNUM = 25
  character (len=13), dimension(ACSNUM) :: acs_names = (/  &
    'ACS_ULCORNER ', 'ACS_URCORNER ', 'ACS_LLCORNER ', 'ACS_LRCORNER ',  &
    'ACS_LTEE     ', 'ACS_RTEE     ', 'ACS_TTEE     ', 'ACS_BTEE     ',  &
    'ACS_HLINE    ', 'ACS_VLINE    ', 'ACS_PLUS     ', 'ACS_S1       ',  &
    'ACS_S9       ', 'ACS_DIAMOND  ', 'ACS_CKBOARD  ', 'ACS_DEGREE   ',  &
    'ACS_PLMINUS  ', 'ACS_BULLET   ', 'ACS_LARROW   ', 'ACS_RARROW   ',  &
    'ACS_UARROW   ', 'ACS_DARROW   ', 'ACS_BOARD    ', 'ACS_LANTERN  ',  &
    'ACS_BLOCK    '  /)
  integer (C_LONG), dimension(ACSNUM) :: acs_values = (/  &
    acs_ulcorner , acs_urcorner , acs_llcorner, acs_lrcorner ,  &
    acs_ltee     , acs_rtee     , acs_ttee    , acs_btee     ,  &
    acs_hline    , acs_vline    , acs_plus    , acs_s1       ,  &
    acs_s9       , acs_diamond  , acs_ckboard , acs_degree   ,  &
    acs_plminus  , acs_bullet   , acs_larrow  , acs_rarrow   ,  &
    acs_uarrow   , acs_darrow   , acs_board   , acs_lantern  ,  &
    acs_block   /)
  integer :: i
  integer (C_INT) :: ierr, ix, iy, tmarg

  tmarg = (LINES - 22) / 2;

  ierr=attrset(a_bold)
  ierr= mvaddstr(tmarg, (COLS - 23) / 2,  &
                 'Alternate Character Set' // C_NULL_CHAR)
  ierr=attrset(a_normal)

  tmarg=tmarg+3

  do i=1, ACSNUM
    iy=mod(i-1,8) * 2 + tmarg
    ix=((i-1) / 8) * (COLS / 4) + (COLS / 8 - 7)
    ierr=move(iy, ix)
    ierr=addch(acs_values(i))
    ierr=mvaddstr(iy, ix+2, trim(acs_names(i)) // C_NULL_CHAR)
  end do
  ierr=mvaddstr(tmarg + 18, 3, 'Press any key to continue' // C_NULL_CHAR)
  ierr=getch()
end subroutine acstest
!-------------------------------------------------------------------------------
subroutine notavail(iwin)
  use M_ncurses
  use winsize
  type (C_PTR), intent(inout) :: iwin
  integer (C_INT) :: ierr

  ierr=beep()
  ierr=werase(iwin)
  ierr=box(iwin, acs_vline, acs_hline)
  ierr=cbreak()
  ierr=mvwaddstr(iwin, 2, 5,  &
        'Sorry, not yet implemented' // C_NULL_CHAR)
  call cont(iwin)
end subroutine notavail
!-------------------------------------------------------------------------------
subroutine cont(iwin)
  use M_ncurses
  type (C_PTR), intent(inout) :: iwin
  integer (C_INT) :: ierr
  ierr=mvwaddstr(iwin, 10, 1, ' Press any key to continue' // C_NULL_CHAR)
  ierr=wrefresh(iwin)
  ierr=raw()
  ierr=wgetch(iwin)
end subroutine cont
!-------------------------------------------------------------------------------
subroutine cont2()
  use M_ncurses
  integer (C_INT) :: ierr
  ierr=move(LINES-1,1)
  ierr=clrtoeol()
  ierr=mvaddstr(LINES-2, 1, ' Press any key to continue' // C_NULL_CHAR)
  ierr=refresh()
  ierr=raw()
  ierr=getch()
end subroutine cont2
!-------------------------------------------------------------------------------
subroutine display_menu(old_option, new_option)
  use M_ncurses
  use commands
  integer, intent(in) :: old_option, new_option
  integer (C_INT) :: lmarg, tmarg
  integer (C_INT) :: ierr
  integer :: i

  lmarg = (COLS - 14)/2
  tmarg = (LINES - (MAX_OPTIONS + 2))/2
  if (old_option == 0) then
    ierr=attrset(a_bold)
    ierr=mvaddstr(tmarg-3, lmarg-5, 'NCurses Test Program' // C_NULL_CHAR)
    ierr=attrset(a_normal)
    do i=1, MAX_OPTIONS
      ierr=mvaddstr(tmarg+i, lmarg, trim(command(i)) // C_NULL_CHAR)
    end do
  else
    ierr=mvaddstr(tmarg + old_option, lmarg,  &
                  trim(command(old_option)) // C_NULL_CHAR)
  end if
  ierr=attrset(a_reverse)
  ierr=mvaddstr(tmarg + new_option, lmarg,  &
                trim(command(new_option)) // C_NULL_CHAR)
  ierr=attrset(a_normal)
  ierr=mvaddstr(tmarg + MAX_OPTIONS + 2, lmarg - 23,  &
         "Use Up and Down Arrows to select - Enter to run - Q to quit"  &
             // C_NULL_CHAR)
    ierr=refresh()
end subroutine display_menu
!-------------------------------------------------------------------------------
program testcurses
  use M_ncurses
  use commands
  type (C_PTR) :: iwin = C_NULL_PTR
  integer (C_INT) :: key
  integer :: istat, new_option=1, old_option=0
  call initTest(iwin, istat)
  if (istat /= 0) then
    write(*,*) 'ERROR 101: initscr failed! STATUS=',istat
    ierr=endwin()
    stop
  end if
! ierr=init_pair(1, color_white, color_blue)
! ierr=wbkgd(iwin,
  ierr=wbkgd(iwin, a_reverse)
  ierr=erase()
  call display_menu(old_option, new_option)
  do
    ierr=noecho()
    ierr=raw()
    ierr=keypad(stdscr, true)
    key=getch()
    if (key == 10 .or. key == 13 .or. key==key_enter) then
      old_option = 0
      ierr=erase()
      ierr=refresh()
      if (new_option == 1) then
        call introtest(iwin, istat)
      else if (new_option == 2) then
        call resizetest(istat)
      else if (new_option == 3) then
        call scrolltest(iwin, istat)
      else if (new_option == 5) then
        call outputtest(iwin)
      else if (new_option == 6) then
        call acstest(iwin)
      else
        call notavail(iwin)
      end if
      ierr=erase()
      call display_menu(old_option, new_option)
    else if (key==key_ppage .or. key==key_home) then
      old_option = new_option
      new_option = 1
      call display_menu(old_option, new_option)
    else if (key==key_npage .or. key==key_end) then
      old_option = new_option
      new_option = MAX_OPTIONS
      call display_menu(old_option, new_option)
    else if (key==key_up) then
      old_option = new_option
      if (new_option > 1) then
        new_option = new_option-1
      end if
      call display_menu(old_option, new_option)
    else if (key==key_down) then
      old_option = new_option
      if (new_option < MAX_OPTIONS) then
        new_option = new_option+1
      end if
      call display_menu(old_option, new_option)
    else if (key==ichar('Q') .or. key==ichar('q')) then
      exit
    end if
  end do
  ierr=delwin(iwin)
  ierr=endwin()
end program testcurses
!-------------------------------------------------------------------------------
