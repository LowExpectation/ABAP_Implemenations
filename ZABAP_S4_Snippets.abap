*----------------------------------------------------------*
* ZABAP_S/4_Snippets
* https://github.com/LowExpectation
*----------------------------------------------------------*
* A collection of S/4 and other interesting techniques for
* use in 1909 and above system versions. The instructions
* are abstract in some cases so make sure to apply as per
* your requirement. Goodluck fearless abap'er.
*----------------------------------------------------------*

report ZABAP_S4_Snippets.

* Selection screen modifications for hiding and layering output options
* Here a selection screen will only show certain aspects depending on radio button clicked
* Selection screen and at output of screen are used for controlling it
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: 
rb1 RADIOBUTTON GROUP g1 DEFAULT 'X'1USER-COMMAND rad,
rb2 RADIOBUTTON GROUP g1 USER-COMMAND rad.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: 
p3 TYPE char1 DEFAULT '1' MODIF ID rb1,
p4 TYPE char1 DEFAULT '2' MODIF ID rb1,
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP 1

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: 
p5 TYPE char1 DEFAULT '3' MODIF ID rb2,
p6 TYPE char1 DEFAULT '4' MODIF ID rb2,
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

IF rb1 = abap_true.
    IF screen-group1 = 'RB1'.
        screen-input = 1.
        screen-active = 1.
   ENDIF.
    IF screen-group1 = 'RB2'.
        screen-input = 0.
        screen-active = 0.
   ENDIF.
MODIFY SCREEN.
ELSEIF rb2 = abap_true.
    IF screen-group1 = 'RB1'.
        screen-input = 0.
        screen-active = 0.
   ENDIF.
    IF screen-group1 = 'RB2'.
        screen-input = 1.
        screen-active = 1.
   ENDIF.
MODIFY SCREEN.
ENDIF.
  ENDLOOP.

* Example of Table Expression for reading a table
* This is equal to READ TABLE with no BINARY SEARCH
* This logic is intended to be used with TYPE SORTED or HASHED tables

* AS always make sure to manually handle un-assigning field symbols within iterations
ASSIGN lt_table[ field = field to compare ] TO <lfs_declared>.
IF <lfs_declared> IS ASSIGNED.

ELSE.

ENDIF.

* This shows an inline declaration using the same logic above
ASSIGN lt_table[ field = field to compare ] TO FIELD-SYMBOL <lfs_inline>.
IF <lfs_inline> IS ASSIGNED.

ELSE.

ENDIF.

* This shows an inline work area using the same logic above
TRY.
DATA(ls_output) =  lt_table[ field = field to compare ].
* Perform work as subrc 0
CATCH cx_sy_itab_line_not_found.
* Read did not succeed, so need to handle non 0 subrc
ENDTRY.

* Create a dynamic table using FOR loop and established typing
* This will loop through lt_input where input fields match the comparison fields
* The bottom statement ( ls_input ) will write the entire row to the created table row
* You can also call methods and return values to the input_field portions extending the capability
* of the FOR loop iteration logic
DATA(lt_created_table) = VALUE ddic_tt_or_ty( FOR ls_input IN lt_input
WHERE ( input_field = field
           OR input_field2 = field2 )
               ( ls_input ) ).

* This derivation allows for us to manually map the input fields to the created table row
* This is good for when you only need a portion of the input fields or even from other memory areas
DATA(lt_created_table) = VALUE ddic_tt_or_ty( FOR ls_input IN lt_input
WHERE ( input_field = field
           OR input_field2 IS NOT INITIAL)
               ( ddic-field1 = ls_input-field1
	      ddic-field2 = ls_input-field2 ) ).

* This short form allows for a READ TABLE TRANSPORTING NO FIELDS
* No memory area is established and should not be used for gathering an index row number
* Or for assigning the contents of the table structure. 
IF line_exists( lt_table[ table_field = field ] ).

ELSE.

ENDIF.

* If you need to check the index using a table expression
* You can use this to find the row of a table as the sy-tabix is not set for table expressions
DATA(lv_tabix) = line_index( lt_table[ table_field = field
                                       table_field2 = field2 ] ).
IF lv_tabix <> 0.

ASSIGN lt_table[ lv_tabix ] TO FIELD-SYMBOL(<lfs_table_row>).
IF <lfs_table_row> IS ASSIGNED.

ENDIF.

ELSE.

ENDIF.

* Here we see the usages of a switch statement
* Switches allow for a clean and object oriented case statement implementation
DATA(lv_output) = SWITCH #( lv_value_to_evaluate
		         WHEN abap_true
		         THEN sy-datum
		         WHEN abap_false
		         THEN sy-datum – 1
		         ELSE sy-datum + 1 ).

* Here we will see how switches can tie into a method call
* We want to customize the report name based on if a manager is running the report
* Name is given the output of SWITCH based on the user running the report
cl_demo_output=>display(
exporting
data = lt_data
name = SWITCH #( sy-uname
WHEN 'Sridhar'
THEN 'Manager output for: ' && sy-uname
ELSE 'User output for: ' && sy-uname ) ).

* Conversions have been done using the search help FM's classically
* Here we will see the modern usages in 7.40 and above
* Below you can see ALPH = IN(input) converting to internal format
* ALPHA = OUT(output) converts to external format
* The conversion is based on the data type, you can also run into issues when running this
* conversion multiple times on the same data field in some cases so be intentional with the usage
DATA: lv_matnr TYPE matnr_d.

lv_matnr = '123456789'

lv_matnr = |{ lv_matnr ALPHA = IN }|.
WRITE: / lv_matnr && 'Converted to internal'.
lv_matnr = |{ lv_matnr ALPHA = OUT }|.
WRITE: / lv_matnr && ' Converted to external'.

* We can use functions to expand our loop logic past that of the FOR or LOOP keywords
* WHILE loops can be combined with returning values to allow for a precise iteration control
* Below is a function that will drive the WHILE loop while it remains true
* This example has the method, implementation, and the while loop for example

METHODS:
continue_processing
IMPORTING
it_table TYPE ddic_data
RETURNING
VALUE(rv_continue) TYPE ABAP_BOOL.

METHOD continue_processing.

SELECT COUNT(*)
FROM @it_table as local_table
WHERE local_table~completed = @abap_false.
IF sy-dbcnt > 0.
rv_continue = abap_true.
ELSE.
rv_continue = abap_false.
ENDIF.

ENDMETHOD.

WHILE me->continue_processing( EXPORTING it_table = lt_table ) = abap_true.

ENDWHILE.

* Here we will see a fun way to update single columns of a table without looping
* It has limited functionality but can be useful when applicable
* We are adding the static header material to all blank rows in the matnr column for existing table
* This allows for a column to be updated with values based on the Where clause
* You can use this with auditing fields or any type of header or repetitive value when applicable 

DATA:
lt_existing_table TYPE STANDARD TABLE OF mara,
 ls_matnr_column TYPE mara.

ls_matnr_column-matnr = '123456789'.

MODIFY lt_existing_table FROM ls_matnr_column TRANSPORTING matnr
WHERE matnr IS INITIAL.

* Corresponding operator will allow for a structure or table to be populated based on input
* One usage would be grabbing unique values like plants for use later
* Below we grab distinct plants from accounting data input for later usage
* Be careful with sorted tables as they will not work with duplicates

TYPES: BEGIN OF ty_plant,
werks TYPE werks_d
END OF ty_plant,

tty_plant TYPE SORTED TABLE OF ty_plant.
DATA lt_plant TYPE tty_plant,
     lt_accounting_data TYPE STANDARD TABLE OF ty_plant.

lt_plant = CORRESPONDING #( lt_accounting_data DISCARDING DUPLICATES
                 MAPPING werks = werks ).

* Have you ever needed to sum a value in groups such as per plant or company code?
* Below may help you out as it allow for grouping sets to be created in our OpenSql syntax
* In the example we use local amount in ACDOCA table to be summed against the plant it is in
* This allows for all amounts to be summed per each unique plant creating a plant level rollup

SELECT
db~werks,
abs( SUM( db~hsl ) ) as hsl,
grouping( db~werks ) as grouping_werks
FROM @lt_acdoca as db
GROUP BY GROUPING SETS ( ( db~werks ) )
INTO TABLE @DATA(lt_werks_cost_per_unit).
IF sy-subrc EQ 0.
SORT  lt_werks_cost_per_unit BY werks.
ENDIF.

* Using Try and catch with the raising clause
* A method using the RAISING keyword will allow for exceptions to be handled
* using the exception class that we inherit or custom build to handle errors
TRY.
* Perform a method call or logical operation
CATCH global_exception_class_name INTO DATA(lo_gxc).
* We caught the exception into an inline reference
DATA(lv_msg) = ls_gxc→get_text( ).
*Using the standard get text method we can capture the message text and display for user
MESSAGE e000(message_class) WITH lv_msg.
ENDTRY.

* Decimal Notation can be done classically with FM
* This would be done with passing local and foreign currency for conversion
* Here is an S/4 based approach that may be useful in OO format

CLASS lcl_currency

PUBLIC SECTION.

TYPES: tt_data TYPE STANDARD TABLE OF acdoca

METHODS:

CONSTRUCTOR
IMPORTING
it_curx TYPE tt_tcurx OPTIONAL

validate_decimal
IMPORTING
iv_currency TYPE kstel
iv_currency_key TYPE waers
RETURNING
VALUE(rv_kstel).

PROTECTED SECTION.

PRIVATE SECTION.

CLASS-DATA:
dt_curx TYPE tt_tcurx.

ENDCLASS.

CLASS lcl_currency IMPLEMENTATION.

METHOD CONSTRUCTOR.
* You can either bring it in
dt_curx = it_curx

* Or the constructor can grab it generically if no populated
IF lines( dt_curx ) < 1.

SELECT *
FROM tcurx
INTO TABLE @dt_curx.
IF sy-subrc EQ 0.

SORT dt_curx BY currkey.

ENDIF.

ENDIF.
ENDMETHOD.

METHOD validate_decimal.

* Read data table to get decimal notation information
READ TABLE dt_curx
ASSIGNING FIELD-SYMBOL(<lfs_curx>)
WITH KEY currkey = iv_currency_key
BINARY SEARCH.
IF sy-subrc EQ 0.

* Get decimal format and shift the output
DATA(lv_shift) = 2 - <lfs_curx>-currdec.
rv_kstel = iv_kstel * ( 10 ** lv_shift ).

ELSE.
* Fail to current notation as master data issue occurred
rv_kstel = iv_kstel.
* Raise message or handle exception as needed
ENDIF.

ENDMETHOD.

ENDCLASS.