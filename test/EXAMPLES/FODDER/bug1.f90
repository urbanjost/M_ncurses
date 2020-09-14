program seeattr
   use M_ncurses
   integer(C_LONG) :: attributes !chtype attributes
   integer(C_INT)  :: y, x
   integer :: ierr
   stdscr=initscr()
!-----------------------------------------------------------------------------------------------------------------------------------
   call getmaxyx(stdscr,y,x);
   ierr=printw("Window size is %d rows, %d columns."//C_NEW_LINE//C_NULL_CHAR,y,x)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=addstr("This terminal "//C_NULL_CHAR);
   if(has_ic())then
      ierr=addstr("has "//C_NULL_CHAR)
   else
      ierr=addstr("does not have "//C_NULL_CHAR)
   endif
   ierr=addstr("insert/delete character abilities"//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=addstr("This terminal "//C_NULL_CHAR)
   if(has_il())then
      ierr=addstr("has "//C_NULL_CHAR)
   else
      ierr=addstr("does not have "//C_NULL_CHAR);
   endif
   ierr=addstr("insert/delete line abilities"//C_NEW_LINE//C_NULL_CHAR)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   if(has_colors())then
      ierr=addstr("This terminal can do colors."//C_NEW_LINE//C_NULL_CHAR)
   else
      ierr=addstr("This terminal cannot do colors."//C_NEW_LINE//C_NULL_CHAR)
   endif
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   if(can_change_color())then
      ierr=addstr("This terminal can change the standard colors."//C_NEW_LINE//C_NULL_CHAR)
   else
      ierr=addstr("This terminal cannot change the colors."//C_NEW_LINE//C_NULL_CHAR)
   endif
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   terminal_baud_rate=baudrate()
   write(20,*)terminal_baud_rate,baudrate()
   ierr=printw("This terminal's baud rate is %d."//C_NEW_LINE//C_NULL_CHAR,terminal_baud_rate)
   ierr=refresh()
   ierr=getch()
!-----------------------------------------------------------------------------------------------------------------------------------
   !!call nc_printhtml(stdscr,"termattrs.html")
!-----------------------------------------------------------------------------------------------------------------------------------
   do
      ierr=getch()
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=endwin()
!-----------------------------------------------------------------------------------------------------------------------------------
end program seeattr
!-----------------------------------------------------------------------------------------------------------------------------------
