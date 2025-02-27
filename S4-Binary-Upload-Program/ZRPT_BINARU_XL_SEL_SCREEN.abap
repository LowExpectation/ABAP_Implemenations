*----------------------------------------------------------*
* Upload Excel Spreadsheet to abap internal table object oriented approach:
* This allows for wide rows up to 4096 characters in width, where 250 to 750 would be the cutoff
* length before being truncated during upload.
*----------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Include zrpt_binary_xl_sel_screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
  PARAMETERS:
    p_filenm TYPE ESEFTFRONT MODIF ID up OBLIGATORY,
    p_column TYPE i DEFAULT 5 MODIF ID up OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.
