*----------------------------------------------------------*
* S/4_Snippets
* https://github.com/LowExpectation
*----------------------------------------------------------*
* A collection of S/4 and other interesting techniques for
* use in 1909 and above system versions. The instructions
* are abstract in some cases so make sure to apply as per
* your requirement. Goodluck fearless abap'er.
*----------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Report zrpt_s4h_syntax
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrpt_s4h_syntax NO STANDARD PAGE HEADING.

INCLUDE:
zrpt_s4h_sel_screen,
zrpt_s4h_cls.

***** LOAD-OF-PROGRAM
LOAD-OF-PROGRAM.

***** INITIALIZATION
INITIALIZATION.

***** Before selection screen is shown
AT SELECTION-SCREEN OUTPUT.
  lcl_s4h=>selection_screen_modification(
    radio_button1 = rb1
    radio_button2 = rb2
    radio_button3 = rb3
  ).

***** SELECTION-SCREEN
AT SELECTION-SCREEN.


***** Value help
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filenm.

***** START-OF-SELECTION
START-OF-SELECTION.
  DATA(lo_s4h) = NEW lcl_s4h( ).
  lo_s4h->s4h_syntax( ).

  IF rb3 EQ abap_true.
    lo_s4h->example_for_output( ).
  ENDIF.

***** END-OF-SELECTION
END-OF-SELECTION.
  IF rb3 = abap_true.
    lo_s4h->alv_output( ).
  ENDIF.