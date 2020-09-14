subroutine attryn(a, c)
! @(#) test termattrs(3f) and query which attributes this terminal type takes
   use M_ncurses
   implicit none
   integer(C_LONG) :: a     !chtype a
   integer(C_LONG) :: c     !chtype c
   integer         :: ierr
   integer(C_INT)  :: terminal_baud_rate
   !if( ior(a,c)  .eq. a) then
   !if( iand(a,c)  .ne. 0) then  !! this would get A_NORMAL wrong, because it is zero
   if( iand(a,c) .eq. c) then
      ierr=addstr("Yes "//C_NULL_CHAR)
      ierr=attron(c)
      ierr=addstr("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"//C_NULL_CHAR)
      ierr=attroff(c)
   else
      ierr=addstr("No  "//C_NULL_CHAR)
      ierr=attron(c)
      ierr=addstr("abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"//C_NULL_CHAR)
      ierr=attroff(c)
   endif
   ierr=addch(int(ichar(C_NEW_LINE),C_LONG))
end subroutine attryn

program seeattr
   use M_ncurses
   integer(C_LONG) :: attributes !chtype attributes
   integer(C_INT)  :: y, x
   integer :: ierr
   stdscr=initscr()
   attributes = termattrs()
   ierr=addstr("This terminal is capable of the following attributes:"//C_NEW_LINE//C_NULL_CHAR)
   ierr=printw("%13s"//C_NULL_CHAR, "AltCharSet: "//C_NULL_CHAR); call attryn(attributes, A_ALTCHARSET)
   ierr=printw("%13s"//C_NULL_CHAR, "Blink: "//C_NULL_CHAR);      call attryn(attributes, A_BLINK)
   ierr=printw("%13s"//C_NULL_CHAR, "Bold: "//C_NULL_CHAR);       call attryn(attributes, A_BOLD)
   ierr=printw("%13s"//C_NULL_CHAR, "Dim: "//C_NULL_CHAR);        call attryn(attributes, A_DIM)
   ierr=printw("%13s"//C_NULL_CHAR, "Invis(ible):"//C_NULL_CHAR);  call attryn(attributes, A_INVIS)
   ierr=printw("%13s"//C_NULL_CHAR, "Normal: "//C_NULL_CHAR);     call attryn(attributes, A_NORMAL)
   ierr=printw("%13s"//C_NULL_CHAR, "Reverse: "//C_NULL_CHAR);    call attryn(attributes, A_REVERSE)
   ierr=printw("%13s"//C_NULL_CHAR, "Standout: "//C_NULL_CHAR);   call attryn(attributes, A_STANDOUT)
   ierr=printw("%13s"//C_NULL_CHAR, "Underline: "//C_NULL_CHAR);  call attryn(attributes, A_UNDERLINE)
   ierr=printw("%13s"//C_NULL_CHAR, "Protect: "//C_NULL_CHAR);    call attryn(attributes, A_PROTECT)
   ierr=printw("%13s"//C_NULL_CHAR, "Horizontal: "//C_NULL_CHAR); call attryn(attributes, A_HORIZONTAL)
   ierr=printw("%13s"//C_NULL_CHAR, "Left: "//C_NULL_CHAR);       call attryn(attributes, A_LEFT)
   ierr=printw("%13s"//C_NULL_CHAR, "Low: "//C_NULL_CHAR);        call attryn(attributes, A_LOW)
   ierr=printw("%13s"//C_NULL_CHAR, "Right: "//C_NULL_CHAR);      call attryn(attributes, A_RIGHT)
   ierr=printw("%13s"//C_NULL_CHAR, "Top: "//C_NULL_CHAR);        call attryn(attributes, A_TOP)
   ierr=printw("%13s"//C_NULL_CHAR, "Vertical: "//C_NULL_CHAR);   call attryn(attributes, A_VERTICAL)
   ierr=printw("%13s"//C_NULL_CHAR, "Italic:   "//C_NULL_CHAR);   call attryn(attributes, A_ITALIC)
   !ierr=printw("%13s"//C_NULL_CHAR, "Color:    "//C_NULL_CHAR);   call attryn(attributes, A_COLOR)
   !ierr=printw("%13s"//C_NULL_CHAR, "Attributes:"//C_NULL_CHAR);  call attryn(attributes, A_ATTRIBUTES)
   !ierr=printw("%13s"//C_NULL_CHAR, "CharText:  "//C_NULL_CHAR);  call attryn(attributes, A_CHARTEXT)
   ierr=refresh()
   ierr=getch()
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
   ierr=printw("This terminal's baud rate is %d."//C_NEW_LINE//C_NULL_CHAR,baudrate())
   ierr=printw("This terminal's baud rate is %d."//C_NEW_LINE//C_NULL_CHAR,terminal_baud_rate)
   ierr=printw("This terminal's baud rate is %f."//C_NEW_LINE//C_NULL_CHAR,terminal_baud_rate)
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
!   write(20,*)'IOR(A,C)=',ior(a,c),a,c
! IOR(A,C)=             15663104             15663104              4194304
! IOR(A,C)=             15663104             15663104               524288
! IOR(A,C)=             15663104             15663104              2097152
! IOR(A,C)=             16711680             15663104              1048576
! IOR(A,C)=             15663104             15663104              8388608
! IOR(A,C)=             15663104             15663104                    0
! IOR(A,C)=             15663104             15663104               262144
! IOR(A,C)=             15663104             15663104                65536
! IOR(A,C)=             15663104             15663104               131072
! IOR(A,C)=             32440320             15663104             16777216
! IOR(A,C)=             49217536             15663104             33554432
! IOR(A,C)=             82771968             15663104             67108864
! IOR(A,C)=            149880832             15663104            134217728
! IOR(A,C)=            284098560             15663104            268435456
! IOR(A,C)=            552534016             15663104            536870912
! IOR(A,C)=           1089404928             15663104           1073741824
!   write(20,*)'IAND(A,C)=',iand(a,c),a,c
! IAND(A,C)=              4194304             15663104              4194304
! IAND(A,C)=               524288             15663104               524288
! IAND(A,C)=              2097152             15663104              2097152
! IAND(A,C)=                    0             15663104              1048576
! IAND(A,C)=              8388608             15663104              8388608
! IAND(A,C)=                    0             15663104                    0
! IAND(A,C)=               262144             15663104               262144
! IAND(A,C)=                65536             15663104                65536
! IAND(A,C)=               131072             15663104               131072
! IAND(A,C)=                    0             15663104             16777216
! IAND(A,C)=                    0             15663104             33554432
! IAND(A,C)=                    0             15663104             67108864
! IAND(A,C)=                    0             15663104            134217728
! IAND(A,C)=                    0             15663104            268435456
! IAND(A,C)=                    0             15663104            536870912
! IAND(A,C)=                    0             15663104           1073741824
!-----------------------------------------------------------------------------------------------------------------------------------
