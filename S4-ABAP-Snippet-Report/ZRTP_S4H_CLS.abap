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
*& Include zrpt_s4h_cls
*&---------------------------------------------------------------------*

CLASS lcl_s4h DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_acdoca,
             rldnr  TYPE fins_ledger,
             rbukrs TYPE bukrs,
             gjahr  TYPE gjahr,
             belnr  TYPE belnr_d,
             docln  TYPE docln6,
           END OF ty_acdoca,

           tty_acdoca TYPE STANDARD TABLE OF ty_acdoca WITH KEY rldnr rbukrs gjahr belnr docln.

    CLASS-METHODS:
      selection_screen_modification
        IMPORTING
          Radio_button1 TYPE abap_bool
          Radio_button2 TYPE abap_bool
          Radio_button3 TYPE abap_bool.

    METHODS:
      constructor,
      s4h_syntax,
      example_for_output,
      alv_output.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_while_loop,
             rldnr     TYPE fins_ledger,
             rbukrs    TYPE bukrs,
             gjahr     TYPE gjahr,
             belnr     TYPE belnr_d,
             docln     TYPE docln6,
             completed TYPE abap_bool,
           END OF ty_while_loop,
           tty_while_loop TYPE STANDARD TABLE OF ty_while_loop WITH KEY rldnr rbukrs gjahr belnr docln,

           BEGIN OF ty_plant,
             werks TYPE werks_d,
           END OF ty_plant,
           tty_plant TYPE SORTED TABLE OF ty_plant WITH UNIQUE KEY werks.


    CLASS-DATA:
      class_t_input          TYPE tty_acdoca,
      class_t_output         TYPE tty_acdoca,
      class_t_example_output TYPE zif_badi_reuse=>tty_lifnr.

    METHODS:

      switch_example
        IMPORTING
                  current_user           TYPE string
        RETURNING VALUE(rv_current_user) TYPE string,
      continue_processing
        IMPORTING
                  it_table           TYPE tty_while_loop
        RETURNING VALUE(rv_continue) TYPE abap_bool,
      for_loop_helper
        RETURNING VALUE(rv_for_output) TYPE string,
      return_lifnr
        RETURNING VALUE(rv_lifnr) TYPE lifnr.

ENDCLASS.

CLASS lcl_s4h IMPLEMENTATION.

  METHOD constructor.
    DATA:
    * Set this with your values needed to test in the environment
    * Example lv_rldnr = '0L'
    lv_rldnr TYPE acdoca-rldnr.

* Get data for the class
    SELECT
    acdoca~rldnr,
    acdoca~rbukrs,
    acdoca~gjahr,
    acdoca~belnr,
    acdoca~docln
    FROM acdoca
    WHERE acdoca~rbukrs = @lv_rbukrs
    INTO TABLE @class_t_input
    UP TO 500 ROWS.
    IF sy-subrc EQ 0.
      SORT class_t_input BY rldnr rbukrs gjahr belnr docln.
    ELSE.
      MESSAGE 'Failed to collect data' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.

  METHOD selection_screen_modification.

    CONSTANTS:
      lc_rb1 TYPE char3 VALUE 'RB1',
      lc_rb2 TYPE char3 VALUE 'RB2',
      lc_rb3 TYPE char3 VALUE 'RB3'.

* We want to alternate between displaying two blocks of screen based on radio button
    LOOP AT SCREEN.
*    We use the modification ID rb1 to make the block 2 visible or invisible
      IF radio_button1 = abap_true.
        IF screen-group1 = lc_rb1.
          screen-input = 1.
          screen-active = 1.
        ENDIF.
        IF screen-group1 = lc_rb2.
          screen-input = 0.
          screen-active = 0.
        ENDIF.
        IF screen-group1 = lc_rb3.
          screen-input = 0.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
*    We use the modification ID rb2 to make the block 3 visible or invisible
      ELSEIF radio_button2 = abap_true. " Else if allows this to be expanded for bigger screens
        IF screen-group1 = lc_rb1.
          screen-input = 0.
          screen-active = 0.
        ENDIF.
        IF screen-group1 = lc_rb2.
          screen-input = 1.
          screen-active = 1.
        ENDIF.
        IF screen-group1 = lc_rb3.
          screen-input = 0.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ELSEIF radio_button3 = abap_true. " Else if allows this to be expanded for bigger screens
        IF screen-group1 = lc_rb1.
          screen-input = 0.
          screen-active = 0.
        ENDIF.
        IF screen-group1 = lc_rb2.
          screen-input = 0.
          screen-active = 0.
        ENDIF.
        IF screen-group1 = lc_rb3.
          screen-input = 1.
          screen-active = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD s4h_syntax.

* Table expression with typed and sorted table
    FIELD-SYMBOLS:
    <lfs_exp_search> TYPE any.

    DATA:
    * Set these with your values needed to test in the environment
    * Example lv_rldnr = '0L'
    lv_rldnr TYPE acdoca-rldnr,
    lv_rbukrs TYPE acdoca-rbukrs,
    lv_gjahr TYPE acdoca-gjahr,
    lv_belnr TYPE acdoca-belnr,
    lv_docln TYPE acdoca-docln.

    * Replace with value or condition
    DATA(lv_belnr2) = 'XXXXXXXXXX'.
    DATA(lv_belnr3) = 'XXXXXXXXXX'.


    ASSIGN class_t_input[ rldnr = lv_rldnr
                          rbukrs = lv_rbukrs
                          gjahr = lv_gjahr
                          belnr = lv_belnr
                          docln = lv_docln ] TO <lfs_exp_search>.
* Handle the Field symbol as needed to evaluate and clear the memory area
    IF <lfs_exp_search> IS ASSIGNED AND <lfs_exp_search> IS NOT INITIAL.
      UNASSIGN <lfs_exp_search>.
    ENDIF.

* Table expression with inline declaration
    ASSIGN class_t_input[ rldnr = lv_rldnr
                          rbukrs = lv_rbukrs
                          gjahr = lv_gjahr
                          belnr = lv_belnr
                          docln = lv_docln ] TO FIELD-SYMBOL(<lfs_inline_expression>).
    IF sy-subrc EQ 0 AND <lfs_inline_expression> IS ASSIGNED.
      UNASSIGN <lfs_inline_expression>.
    ENDIF.

* This will work with a work area also
    TRY.
        DATA(ls_expression) = class_t_input[
                                  rldnr = lv_rldnr
                                  rbukrs = lv_rbukrs
                                  gjahr = lv_gjahr
                                  belnr = lv_belnr
                                  docln = lv_docln ].
        CLEAR: ls_expression.
      CATCH cx_sy_itab_line_not_found.
*     We didnt find a value if we end up in this block
    ENDTRY.

* Dynamic table using FOR loop copying all fields
    DATA(lt_for_loop_copy) = VALUE tty_acdoca( FOR ls_class_s_input IN class_t_input
    WHERE ( rldnr = lv_rldnr
        AND rbukrs = lv_rbukrs
        AND gjahr = lv_gjahr
        AND belnr = lv_belnr
         OR belnr = lv_belnr2 ) ( ls_class_s_input ) ).
    IF lt_for_loop_copy IS NOT INITIAL.
      CLEAR: lt_for_loop_copy.
    ENDIF.

* Dynamic table filtering using FOR loop to copy on specified columns
    DATA(lt_for_loop_filtered) = VALUE tty_acdoca( FOR ls_class_s_input IN class_t_input
    WHERE ( rldnr = lv_rldnr
        AND rbukrs = lv_rbukrs
        AND gjahr = lv_gjahr
        AND belnr = lv_belnr
         OR belnr = lv_belnr2 ) (
         rbukrs = ls_class_s_input-rbukrs
         gjahr = ls_class_s_input-gjahr
         belnr = ls_class_s_input-belnr ) ).
    IF lt_for_loop_filtered IS NOT INITIAL.
      CLEAR lt_for_loop_filtered.
    ENDIF.

    DATA(lt_for_loop_method) = VALUE tty_acdoca( FOR ls_class_s_input IN class_t_input
    WHERE ( rldnr = lv_rldnr
        AND rbukrs = lv_rbukrs
        AND gjahr = lv_gjahr )
        ( rldnr = ls_class_s_input-rldnr
           rbukrs = ls_class_s_input-rbukrs
           gjahr = ls_class_s_input-gjahr
           belnr = me->for_loop_helper( )
           docln = me->for_loop_helper( ) ) ).
    CLEAR: lt_for_loop_method.

* The trusty and ubiquitous line exists soft check
    IF line_exists( class_t_input[ rldnr = lv_rldnr
                                   rbukrs = lv_rbukrs
                                   gjahr = lv_gjahr
                                   belnr = lv_belnr
                                   docln = lv_docln ] ).
* Do something because it exists
    ELSE.
* Do something because it was not existing
    ENDIF.

* Checking index with line_index and using that index
    DATA(lv_tabix) = line_index( class_t_input[ rldnr = lv_rldnr
                                                rbukrs = lv_rbukrs
                                                gjahr = lv_gjahr
                                                belnr = lv_belnr
                                                docln = lv_docln ] ).
    IF lv_tabix <> 0.
      ASSIGN class_t_input[ lv_tabix ] TO FIELD-SYMBOL(<lfs_table_row>).
      IF <lfs_table_row> IS ASSIGNED.
        UNASSIGN <lfs_table_row>.
      ENDIF.
      CLEAR: lv_tabix.
    ENDIF.

* Switch statement - purposed case selection
* Add your leader or coworkers ID here in USER WHEN statement
DATA:
lv_user_cohort TYPE sy-uname.
lv_user_cohort = 'USER'
    DATA(lv_output) = SWITCH #( sy-uname
    WHEN lv_user_cohort
    THEN 'Manager user: ' && sy-uname
    ELSE 'User: ' && sy-uname ).

* Switch used within a method call
    DATA(lv_current_user) = me->switch_example( current_user = SWITCH #( sy-uname
        WHEN lv_user_cohort
        THEN 'Manager user: ' && sy-uname
        ELSE 'User: ' && sy-uname ) ).
    CLEAR: lv_current_user, lv_user_cohort.

* Internal and external conversions
    DATA:
        lv_docln TYPE docln6 VALUE '1'.
    lv_docln = |{ lv_docln ALPHA = IN }|.

    lv_docln = |{ lv_docln ALPHA = OUT }|.

* While control loop    
    DATA(lt_while_input) = VALUE tty_while_loop( FOR ls_while_input IN class_t_input
    WHERE ( rldnr = lv_rldnr
        AND rbukrs = lv_rbukrs
        AND gjahr = lv_gjahr
        AND belnr = lv_belnr
         OR belnr = lv_belnr2
         OR belnr = lv_belnr3 )
         ( rldnr = ls_while_input-rldnr
           rbukrs = ls_while_input-rbukrs
           gjahr = ls_while_input-gjahr
           belnr = ls_while_input-belnr
           docln = ls_while_input-docln
           completed = abap_false ) ).

* The while loop will continue processing as long as the method tells it to
    WHILE me->continue_processing( EXPORTING it_table = lt_while_input ) = abap_true.

* Update the table rows line by line so that while method can control the loop
      READ TABLE lt_while_input ASSIGNING FIELD-SYMBOL(<lfs_while_input>)
      WITH KEY completed = abap_false.
      IF sy-subrc EQ 0.
*      Marking as completed
        <lfs_while_input>-completed = abap_true.
      ENDIF.

    ENDWHILE.
    CLEAR: lt_while_input, lv_belnr2, lv_belnr3.

* Update single column in a table
    DATA:
      lt_acdoca TYPE STANDARD TABLE OF acdoca,
      ls_acdoca TYPE acdoca.

    ls_acdoca-gjahr = '2024'.

    DO 5 TIMES.
      APPEND INITIAL LINE TO lt_acdoca ASSIGNING FIELD-SYMBOL(<lfs_acdoca>).
      <lfs_acdoca>-gjahr = '2023'.
    ENDDO.

    MODIFY lt_acdoca FROM ls_acdoca TRANSPORTING gjahr
    WHERE gjahr NE ls_acdoca-gjahr.
    CLEAR: lt_acdoca, ls_acdoca.

* Corresponding Operator usage
    DATA:
        lt_plant TYPE tty_plant.

    SELECT
    mara~matnr,
    marc~werks
    FROM mara
    INNER JOIN marc ON mara~matnr = marc~matnr
    INTO TABLE @DATA(lt_werks).
    IF sy-subrc EQ 0.
      SORT lt_werks BY matnr werks.
    ENDIF.

* This should give a unique listing of the plants from the Material Master tables
    lt_plant = CORRESPONDING #( lt_werks DISCARDING DUPLICATES
    MAPPING werks = werks ).

    CLEAR: lt_werks, lt_plant.

* Below is a way to get a rollup summing of costs
    SELECT
    local_table~*,
    acdoca~werks,
    acdoca~hsl
    FROM @class_t_input AS local_table
    INNER JOIN acdoca ON
     local_table~rldnr =  acdoca~rldnr AND
     local_table~rbukrs = acdoca~rbukrs AND
     local_table~gjahr = acdoca~gjahr AND
     local_table~belnr = acdoca~belnr AND
     local_table~docln = acdoca~docln
     INTO TABLE @DATA(lt_acdoca_costing).
    IF sy-subrc EQ 0.
      SORT lt_acdoca_costing BY local_table-rldnr local_table-rbukrs local_table-gjahr local_table-belnr local_table-docln.

      SELECT ##ITAB_DB_SELECT
      db~werks,
      abs( SUM( db~hsl ) ) AS hsl,
      grouping( db~werks ) AS grouping_werks
      FROM @lt_acdoca_costing AS db
      GROUP BY GROUPING SETS ( ( db~werks ) )
      INTO TABLE @DATA(lt_werks_cost_per_unit).
      IF sy-subrc EQ 0.
        SORT lt_werks_cost_per_unit BY werks.
        CLEAR: lt_acdoca_costing, lt_werks_cost_per_unit.
      ENDIF.

    ENDIF.

* Decimal notation currency changes
    DATA:
        lv_kstel TYPE keph-kst001 VALUE '1900.00'.

    SELECT *
    FROM tcurx
    INTO TABLE @DATA(lt_curx).
    IF sy-subrc EQ 0.
      SORT lt_curx BY currkey.
    ENDIF.

    READ TABLE lt_curx
    ASSIGNING FIELD-SYMBOL(<lfs_curx>)
    WITH KEY currkey = 'BHD'
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      DATA(lv_shift) = 2 - <lfs_curx>-currdec.
      lv_kstel *= ( 10 ** lv_shift ) ##OPERATOR[**].
    ENDIF.

* Here we will call a custom reusable BADI
* Start with creating a reference of the BADI from SE18 Enhancement Spot
    DATA:
      lo_zbadi_reuse TYPE REF TO zbadi_reuse,
      lt_lifnr       TYPE zif_badi_reuse=>tty_lifnr.

    TRY.
* Get the object into the reference, the class constructor is also called **once**
        GET BADI lo_zbadi_reuse.

        DATA(lv_lifnr) = me->return_lifnr( ).

* Call whatever methods or attributes you need
        CALL BADI lo_zbadi_reuse->set_lifnr
          EXPORTING
            iv_lifnr = lv_lifnr.

        CALL BADI lo_zbadi_reuse->get_lifnr
          IMPORTING
            ev_lifnr = DATA(lv_get_lifnr).

        CALL BADI lo_zbadi_reuse->helper_method
*           EXPORTING
*             iv_any =
*             it_any =
          IMPORTING
*           ev_any =
            et_any = lt_lifnr.
* We dont want these to dump as they are usually used in high risk standard programs
      CATCH
      cx_badi_activation_error
      cx_badi_consistency_error
      cx_badi_filter_error
      cx_badi_ilm_destr_with_arkey
      cx_badi_initial_context
      cx_badi_initial_reference
      cx_badi_itmf_nf_fields
      cx_badi_releng_in_ecatt
      cx_badi_unknown_error
      cx_badi_multiply_implemented
      cx_badi_not_single_use
      cx_badi_context_error
      cx_badi_deprec_handle_class
      cx_badi_error
      cx_badi_not_found INTO DATA(lo_gxc).
        DATA(lv_msg) = lo_gxc->get_text( ).
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    CLEAR: lt_lifnr, lv_get_lifnr, lv_lifnr, lo_zbadi_reuse.

  ENDMETHOD.

  METHOD switch_example.

    rv_current_user = current_user.

  ENDMETHOD.

  METHOD continue_processing.

* We check the completed column to monitor the while loop logic process for exit
    SELECT COUNT( * )
    FROM @it_table AS local_table
    WHERE local_table~completed = @abap_false.
    IF sy-dbcnt > 0.
      rv_continue = abap_true.
    ELSE.
      rv_continue = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD for_loop_helper.

    rv_for_output = sy-abcde.

  ENDMETHOD.

  METHOD return_lifnr.
    lv_lifnr TYPE lfa1-lifnr.
    * Replace with your information
    lv_lifnr = 'XXXXXXXXXX'
    SELECT
    lfa1~lifnr
    FROM lfa1
    INTO @rv_lifnr
    UP TO 1 ROWS.
      IF sy-subrc NE 0.
        rv_lifnr = lv_lifnr.
      ENDIF.
      EXIT.
    ENDSELECT.

  ENDMETHOD.

  METHOD example_for_output.

    MOVE-CORRESPONDING class_t_input TO class_t_output.

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




    SELECT COUNT( * )
    FROM @class_t_output AS output_table ##ITAB_KEY_IN_SELECT.
    DATA(lv_dbcnt) = sy-dbcnt.
    IF lv_dbcnt > 0.
      lv_message_row1 = 'Success'.
    ELSE.
      lv_message_row1 = 'Failure'.
    ENDIF.
    lv_message_row2 = lv_dbcnt && ' Rows extracted'.

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
*            lo_column ?= lo_columns->get_column( 'COLUMN_A_RLDNR' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

        TRY.
*            lo_column ?= lo_columns->get_column( 'COLUMN_B_RBUKRS' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

        TRY.
*            lo_column ?= lo_columns->get_column( 'COLUMN_C_GJAHR' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

        TRY.
*            lo_column ?= lo_columns->get_column( 'COLUMN_D_BELNR' ).
*        lo_column->set_short_text( 'MY EXCEPT' ).
*        lo_column->set_medium_text( 'MY EXCEPTION' ).
*        lo_column->set_long_text( 'MY EXCEPTION COLUMN' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

        TRY.
*            lo_column ?= lo_columns->get_column( 'COLUMN_E_DOCLN' ).
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