AC_INIT(ginsu,0.8.0)
AC_CONFIG_SRCDIR(Main.hs)

AC_ARG_WITH(compiler, [AC_HELP_STRING([--with-compiler=HC], [use specified haskell compiler (ignored)])])
AC_ARG_WITH(gcc, [AC_HELP_STRING([--with-gcc=CC], [use specified C compiler])], [CC=$withval])

AC_CHECK_LIB(curses,   addnstr,   [CURSES_LIB=curses])
AC_CHECK_LIB(ncurses,  addnstr,   [CURSES_LIB=ncurses])
AC_CHECK_LIB(ncursesw, waddnwstr, [CURSES_LIB=ncursesw])
AS_IF([test -z $CURSES_LIB], AC_MSG_FAILURE([curses library not found]))
LDFLAGS="-l$CURSES_LIB $LDFLAGS"

AC_CHECK_HEADERS(curses.h ncurses.h ncurses/ncurses.h ncursesw/ncurses.h, [CURSES_HDR=$ac_header])
AS_IF([test -z $CURSES_HDR], AC_MSG_FAILURE([curses headers not found]))

AC_CHECK_FUNCS(waddnwstr use_default_colors resizeterm)

AC_CONFIG_HEADERS([config.h])
AC_SUBST(CURSES_LIB)
AC_OUTPUT(ginsu.buildinfo)
