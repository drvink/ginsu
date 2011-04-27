AC_INIT(ginsu,0.8.0)
AC_CONFIG_SRCDIR(Main.hs)

AC_ARG_WITH(compiler, [AC_HELP_STRING([--with-compiler=HC], [use specified haskell compiler (ignored)])])
AC_ARG_WITH(gcc, [AC_HELP_STRING([--with-gcc=CC], [use specified C compiler (ignored)])])

AC_CHECK_LIB(curses,   addnstr,   [CURSES_LIB=curses])
AC_CHECK_LIB(ncurses,  addnstr,   [CURSES_LIB=ncurses])
AC_CHECK_LIB(ncursesw, waddnwstr, [CURSES_LIB=ncursesw])

AC_CHECK_HEADERS(curses.h ncurses.h ncurses/ncurses.h ncursesw/ncurses.h)

AC_CONFIG_HEADERS([config.h])
AC_SUBST(CURSES_LIB)
AC_OUTPUT(ginsu.buildinfo)
