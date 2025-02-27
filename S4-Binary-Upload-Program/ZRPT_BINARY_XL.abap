*----------------------------------------------------------*
* Upload Excel Spreadsheet to abap internal table object oriented approach:
* This allows for wide rows up to 4096 characters in width, where 250 to 750 would be the cutoff
* length before being truncated during upload.
*----------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Report zrpt_binary_xl
*&---------------------------------------------------------------------*
*& This program is great for wide row uploads up to 4096 characters
*& The common cutoff would be 250 to 750 for the row width before truncating
*&---------------------------------------------------------------------*
REPORT zrpt_binary_xl NO STANDARD PAGE HEADING.

INCLUDE:
* Selection Screen
zrpt_binary_xl_sel_screen,
* Global Declarations
zrpt_binary_xl_top,
* Main form
zrpt_binary_xl_f01,
* main program class/ super class
zrpt_binary_xl_cls.

***** LOAD-OF-PROGRAM

***** INITIALIZATION
INITIALIZATION.

***** Before selection screen is shown
AT SELECTION-SCREEN OUTPUT.

***** SELECTION-SCREEN
AT SELECTION-SCREEN.

***** Value help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filenm.

  lcl_gui_upload=>value_help_file_determine( ).

***** START-OF-SELECTION
START-OF-SELECTION.

* Instantiate the local class into instance object
  DATA(lo_gui_upload) = NEW lcl_gui_upload(
    iv_filename = p_filenm
    iv_column   = p_column
  ).

* Perform the file upload and extraction
  lo_gui_upload->upload_file(
    RECEIVING
      rv_success = DATA(lv_success)
  ).

***** END-OF-SELECTION
END-OF-SELECTION.

* Display either success or error ALV
  lo_gui_upload->alv_output( iv_success = lv_success  ).
