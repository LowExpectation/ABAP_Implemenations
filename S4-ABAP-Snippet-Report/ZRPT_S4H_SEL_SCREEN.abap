*&---------------------------------------------------------------------*
*& Include zrpt_s4h_sel_screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    rb1 RADIOBUTTON GROUP g01 DEFAULT 'X' USER-COMMAND rad,
    rb2 RADIOBUTTON GROUP g01,
    rb3 RADIOBUTTON GROUP g01.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    p1 TYPE char1 DEFAULT '1' MODIF ID rb1,
    p2 TYPE char1 DEFAULT '2' MODIF ID rb1.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-003.
  PARAMETERS:
    p3 TYPE char1 DEFAULT '3' MODIF ID rb2,
    p4 TYPE char1 DEFAULT '4' MODIF ID rb2.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-004.

SELECTION-SCREEN END OF BLOCK b04.