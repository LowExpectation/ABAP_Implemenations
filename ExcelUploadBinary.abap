*----------------------------------------------------------*
* zexcel_upload_binary
*
*----------------------------------------------------------*
* Upload Excel Spreadsheet to abap internal table object oriented approach:
* This allows for wide rows up to 4096 characters in width, where 250 to 750 would be the cutoff
* length before being truncated during upload.
*----------------------------------------------------------*

report zexcel_upload_binary.

SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-s01.
PARAMETERS:
  p_filenm TYPE localfile MODIF ID up,
  p_column TYPE I DEFAULT 10 MODIF ID up.
SELECTION-SCREEN END OF BLOCK B01.

CLASS lcl_gui_upload DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING i_filename TYPE string,
      upload_file RETURNING VALUE(r_success) TYPE abap_bool.
  PRIVATE SECTION.
    DATA:
      m_filename TYPE string,
      m_column TYPE i.
ENDCLASS.

CLASS lcl_gui_upload IMPLEMENTATION.
  METHOD constructor.
    m_filename = i_filename.
    m_column = p_column.
  ENDMETHOD.

  METHOD upload_file.
    DATA:
      lv_filelength TYPE I,
      lv_headerxstring TYPE xstring,
      lt_data TYPE STANDARD TABLE OF xstring,
      lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet,
      lv_filesize TYPE i,
      lv_success TYPE abap_bool,
      ct_upload TYPE STANDARD TABLE OF acdoca. "Type of the custom table or end objective

    FIELD-SYMBOLS:
      <lfs_data> TYPE any,
      <lfs_field> TYPE any,
      <lfs_upload> TYPE acdoca, "type of the custom table or final objective structure
      <lfs_excel_ref> TYPE STANDARD TABLE.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = m_filename
        filetype = 'BIN'
      IMPORTING
        filelength = lv_filesize
        header = lv_headerxstring
      CHANGING
        data_tab = lt_data
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0. “ Many more exceptions to add from template
      r_success = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer = lv_headerxstring
      TABLES
        binary_tab = lt_data.
    IF sy-subrc <> 0.
      r_success = abap_false.
      RETURN.
    ENDIF.

    TRY.
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
        document_name = m_filename
        xdocument = lv_headerxstring ).
    CATCH cx_fdt_excel_core.
      r_success = abap_false.
      RETURN.
    ENDTRY.

    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
      Importing
        worksheet_names = DATA(lt_worksheets) ).

    IF NOT lt_worksheets IS INITIAL.

      READ TABLE lt_worksheets INTO DATA(lv_worksheetname) INDEX 1.
      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_worksheetname ).
      ASSIGN lo_data_ref->* TO <lfs_excel_ref>.
    ELSE.
      r_success = abap_false.
      RETURN.
    ENDIF.

    LOOP AT <lfs_excel_ref> ASSIGNING <lfs_data> FROM 2.

    APPEND INITIAL LINE TO ct_upload ASSIGNING <lfs_upload>.

    DO m_columns TIMES.
* Add when statements based on the amount of columns
* Maybe a dynamic approach is available based on m_columns
      ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_data> TO <lfs_field>.
      IF sy-subrc EQ 0.

        CASE sy-index.
          WHEN 1.
            <lfs_upload>-field1 = <lfs_field>.
          WHEN 2.
            <lfs_upload>-field2 = <lfs_field>.
          WHEN 3.
            <lfs_upload>-field3 = <lfs_field>.
          WHEN OTHERS.

        ENDCASE.

       ENDIF.

     ENDDO.

   ENDLOOP.

   IF ct_upload IS NOT INITIAL.
     lv_success = abap_true.
   ELSE.
     lv_success = abap_false.
   ENDIF.

   r_success = lv_success.

 ENDMETHOD.

ENDCLASS.

* Write the output after running the program
DATA(lo_gui_upload) = NEW lcl_gui_upload( p_filenm ).
IF lo_gui_upload->upload_file( ) = abap_true.
 WRITE: 'File uploaded successfully.' AT /1(30).
ELSE.
 WRITE: 'File upload failed.' AT /1(30).
ENDIF.
