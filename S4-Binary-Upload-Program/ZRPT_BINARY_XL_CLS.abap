*----------------------------------------------------------*
* Upload Excel Spreadsheet to abap internal table object oriented approach:
* This allows for wide rows up to 4096 characters in width, where 250 to 750 would be the cutoff
* length before being truncated during upload.
*----------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Include zrpt_binary_xl_cls
*&---------------------------------------------------------------------*

***** Create local class for the program to use
***** This could also be a superclass based on the complexity requirements

* Class Definition
CLASS lcl_gui_upload DEFINITION.

  PUBLIC SECTION.

* Types Declarations
    TYPES:

      BEGIN OF ty_sheet_structure,
        column_a_rldnr  TYPE fins_ledger,
        column_b_rbukrs TYPE bukrs,
        column_c_gjahr  TYPE gjahr,
        column_d_belnr  TYPE belnr_d,
        column_e_docln  TYPE docln6,
      END OF ty_sheet_structure,

      tty_sheet_structure TYPE STANDARD TABLE OF ty_sheet_structure.

* Static Methods
    CLASS-METHODS:
      value_help_file_determine.


* Instance Methods
    METHODS:
      constructor
        IMPORTING
          iv_filename TYPE eseftfront
          iv_column   TYPE i,
      upload_file
        RETURNING VALUE(rv_success) TYPE abap_bool,
      alv_output
        IMPORTING
          iv_success TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.

* Class Data declarations
    DATA:
      class_v_filename TYPE string,
      class_v_column   TYPE i,
      class_t_output   TYPE tty_sheet_structure.

ENDCLASS.

* Class implementation and code
CLASS lcl_gui_upload IMPLEMENTATION.

  METHOD constructor.
* Get the values imported into class instance
    class_v_filename = iv_filename.
    class_v_column = iv_column.

  ENDMETHOD.

  METHOD value_help_file_determine.

    DATA:
      lt_file_table TYPE filetable,
      lv_rc         TYPE i.

* Get the file location for the user
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = 'Please choose an XL file'
        file_filter             = '.xlsx'
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF lv_rc = -1 OR sy-subrc <> 0.
      MESSAGE 'Failed to determine file for upload' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
* Read through table and append the file name to the selection screen
      LOOP AT lt_file_table ASSIGNING FIELD-SYMBOL(<lfs_file>).
        p_filenm = <lfs_file>-filename.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD upload_file.

* Data Declarations
    DATA:
      lv_filelength    TYPE i,
      lv_headerxstring TYPE xstring,
      lt_data          TYPE STANDARD TABLE OF x, "you might need xstring depending on system...
      lo_excel_ref     TYPE REF TO cl_fdt_xl_spreadsheet,
      lt_upload        TYPE tty_sheet_structure.

* Field Symbols
    FIELD-SYMBOLS:
      <lfs_data>            TYPE any,
      <lfs_field>           TYPE any,
      <lfs_upload>          TYPE ty_sheet_structure,
      <lfs_excel_reference> TYPE STANDARD TABLE.

    CLEAR: lt_data.

* Validate that file exists in case of manual typing
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = class_v_filename
      RECEIVING
        result               = DATA(lv_result)
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc EQ 0 AND lv_result = abap_true.

* Call the GUI upload wrapper
      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename   = class_v_filename
          filetype   = 'BIN'
        IMPORTING
          filelength = lv_filelength
          header     = lv_headerxstring
        CHANGING
          data_tab   = lt_data
        EXCEPTIONS
          OTHERS     = 1.
*  We want to make sure that no errors can move forward
      IF sy-subrc <> 0.
        rv_success = abap_false.
        RETURN.
      ENDIF.

    ELSE.
      rv_success = abap_false.
      RETURN.

    ENDIF.

* Convert binary data to a text based data
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = lv_headerxstring
      TABLES
        binary_tab   = lt_data
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      rv_success = abap_false.
      RETURN.
    ENDIF.

* Instantiate the spreadsheet class using our extracted data
    TRY.
        lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
        document_name = class_v_filename
        xdocument = lv_headerxstring ).
      CATCH cx_fdt_excel_core.
        rv_success = abap_false.
        RETURN.
    ENDTRY.

* Get the worksheet names into a string table
    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
    worksheet_names = DATA(lt_worksheets) ).

* If we have a worksheet then we can pull the data from it
    IF lt_worksheets IS NOT INITIAL.
* We could also loop this to pull from multiple worksheets but will only do a single one
      READ TABLE lt_worksheets
      INTO DATA(lv_worksheetname) INDEX 1.
      IF sy-subrc EQ 0.
*      We will get the data from the worksheets. This is where we could loop if needed
        DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                              worksheet_name  = lv_worksheetname ).
        ASSIGN lo_data_ref->* TO <lfs_excel_reference>.

      ELSE.
        rv_success = abap_false.
        RETURN.
      ENDIF.

    ELSE.
      rv_success = abap_false.
      RETURN.
    ENDIF.

* Since we have the data, now we can start building the internal table
    LOOP AT <lfs_excel_reference> ASSIGNING <lfs_data> FROM 2. " This is to skip the header

      APPEND INITIAL LINE TO class_t_output ASSIGNING <lfs_upload>.

*    This allows for us to be dynamic based on amount of columns
      DO class_v_column TIMES.

*     Based on the field index we will update the initial line from above
        ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_data> TO <lfs_field>.
        IF sy-subrc EQ 0.

* This allows for the field to be correctly mapped to the output structure
* This might be able to be made dynamic as well by using DDIC structure then looping through the structure
          CASE sy-index.

            WHEN 1.
              <lfs_upload>-column_a_rldnr = <lfs_field>.

            WHEN 2.
              <lfs_upload>-column_b_rbukrs = <lfs_field>.

            WHEN 3.
              <lfs_upload>-column_c_gjahr = <lfs_field>.

            WHEN 4.
              <lfs_upload>-column_d_belnr = <lfs_field>.

            WHEN 5.
              <lfs_upload>-column_e_docln = <lfs_field>.

            WHEN OTHERS.

          ENDCASE.

        ENDIF.

      ENDDO.

    ENDLOOP.

* If the return is successful it will have a X in the return value
    IF class_t_output IS NOT INITIAL.
      rv_success = abap_true.
    ELSE.
      rv_success = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD alv_output.

    DATA:
      lv_message_row1 TYPE string,
      lv_message_row2 TYPE string,
*     ALV and Columns
      lo_alv          TYPE REF TO cl_salv_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lo_column       TYPE REF TO cl_salv_column_table,
*     Header
      lo_top_element  TYPE REF TO cl_salv_form_layout_grid,
      lo_header       TYPE REF TO cl_salv_form_header_info,
      lo_action       TYPE REF TO cl_salv_form_action_info,
*     Functionality
      lo_functions    TYPE REF TO cl_salv_functions.

* We will display a simple ALV output in the case of a failure
* This could be expanded to show specific reasons or stop lights, etc
    IF iv_success = abap_false.
      lv_message_row1 = 'Failure'.
      lv_message_row2 = 'Failure type: to be added later'.

      IF class_t_output IS INITIAL.
        APPEND INITIAL LINE TO class_t_output.
      ENDIF.

    ELSE.
      lv_message_row1 = 'Success'.

      SELECT COUNT( * )
      FROM @class_t_output AS output_table ##ITAB_KEY_IN_SELECT
      .
      DATA(lv_dbcnt) = sy-dbcnt.
      lv_message_row2 = lv_dbcnt && ' Rows extracted'.
    ENDIF.

* Create the ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = class_t_output.
      CATCH cx_salv_msg.
    ENDTRY.

    TRY.
* Create a header with rows and columns
        CREATE OBJECT lo_top_element.

        lo_header = lo_top_element->create_header_information(
        row = 1
        column = 1
        text     = lv_message_row1                          "#EC NOTEXT
        tooltip  = 'Flugdaten' ).                           "#EC NOTEXT

        lo_action = lo_top_element->create_action_information(
        row = 2
        column = 1
        text     = lv_message_row2                          "#EC NOTEXT
        tooltip  = 'Uebersicht ueber alle Flugdaten' ).     "#EC NOTEXT

* Set the header using object built previous
        lo_alv->set_top_of_list( lo_top_element ).

* Functions
        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( abap_true ).

* Columns
        lo_columns = lo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

* Format the column headers
        TRY.
            lo_column ?= lo_columns->get_column( 'COLUMN_A_RLDNR' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

        TRY.
            lo_column ?= lo_columns->get_column( 'COLUMN_B_RBUKRS' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

        TRY.
            lo_column ?= lo_columns->get_column( 'COLUMN_C_GJAHR' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

        TRY.
            lo_column ?= lo_columns->get_column( 'COLUMN_D_BELNR' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

        TRY.
            lo_column ?= lo_columns->get_column( 'COLUMN_E_DOCLN' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

*  Display the List Viewer
        lo_alv->display( ).

      CATCH cx_salv_data_error cx_sy_ref_is_initial.    "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
