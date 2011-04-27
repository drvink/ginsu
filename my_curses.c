#include "my_curses.h"
#include <HsFFI.h>
WINDOW ** get_stdscr (void) {return &stdscr;}
int * get_LINES (void) {return &LINES;}
int * get_COLS (void) {return &COLS;}
int * get_COLOR_PAIRS (void) {return &COLOR_PAIRS;}
int * get_COLORS (void) {return &COLORS;}
chtype hs_curses_color_pair (HsInt pair) {return COLOR_PAIR (pair);}
chtype hs_curses_acs_ulcorner (void) {return ACS_ULCORNER;}
chtype hs_curses_acs_llcorner (void) {return ACS_LLCORNER;}
chtype hs_curses_acs_urcorner (void) {return ACS_URCORNER;}
chtype hs_curses_acs_lrcorner (void) {return ACS_LRCORNER;}
chtype hs_curses_acs_rtee     (void) {return ACS_RTEE;}
chtype hs_curses_acs_ltee     (void) {return ACS_LTEE;}
chtype hs_curses_acs_btee     (void) {return ACS_BTEE;}
chtype hs_curses_acs_ttee     (void) {return ACS_TTEE;}
chtype hs_curses_acs_hline    (void) {return ACS_HLINE;}
chtype hs_curses_acs_vline    (void) {return ACS_VLINE;}
chtype hs_curses_acs_plus     (void) {return ACS_PLUS;}
chtype hs_curses_acs_s1       (void) {return ACS_S1;}
chtype hs_curses_acs_s9       (void) {return ACS_S9;}
chtype hs_curses_acs_diamond  (void) {return ACS_DIAMOND;}
chtype hs_curses_acs_ckboard  (void) {return ACS_CKBOARD;}
chtype hs_curses_acs_degree   (void) {return ACS_DEGREE;}
chtype hs_curses_acs_plminus  (void) {return ACS_PLMINUS;}
chtype hs_curses_acs_bullet   (void) {return ACS_BULLET;}
chtype hs_curses_acs_larrow   (void) {return ACS_LARROW;}
chtype hs_curses_acs_rarrow   (void) {return ACS_RARROW;}
chtype hs_curses_acs_darrow   (void) {return ACS_DARROW;}
chtype hs_curses_acs_uarrow   (void) {return ACS_UARROW;}
chtype hs_curses_acs_board    (void) {return ACS_BOARD;}
chtype hs_curses_acs_lantern  (void) {return ACS_LANTERN;}
chtype hs_curses_acs_block    (void) {return ACS_BLOCK;}
#  ifdef ACS_S3
chtype hs_curses_acs_s3       (void) {return ACS_S3;}
chtype hs_curses_acs_s7       (void) {return ACS_S7;}
chtype hs_curses_acs_lequal   (void) {return ACS_LEQUAL;}
chtype hs_curses_acs_gequal   (void) {return ACS_GEQUAL;}
chtype hs_curses_acs_pi       (void) {return ACS_PI;}
chtype hs_curses_acs_nequal   (void) {return ACS_NEQUAL;}
chtype hs_curses_acs_sterling (void) {return ACS_STERLING;}
#  endif
