*----------------------------------------------------------*
* Custom Badi implementation
*
*----------------------------------------------------------*
* Quick guide to implementing and calling custom BADI
*----------------------------------------------------------*

*Implementing custom BADI instance. This is not S4 specific but is a good choice when you need to *keep standard process and auditing with a large enhancement operation.

* Step 1 - 4.
* Create Enhancement Spot(ES) SE18 (Reusing Instantiation)
* Create Enhancement Implementation for Spot
* Maintain/Create Interface for Enhancement Spot
* Maintain/Create Class for Enhancement Spot Interface

* Step 5, calling BADI examples below:

* Create a local object
DATA:
lo_badi_custom_imp TYPE badi_custom_implementation. "(definition you defined in SE18 for ES) 

*We use a try and catch to protect illegal actions
TRY.


* Allows for creation of a BADI object similar to CREATE OBJECT but handles variants also
GET BADI lo_badi_custom_imp. "Maybe can add constructor here or multiple badi criteria

* Here we only have a single use call but more care needs to be taken if multi use
* Notice that we query the BADI object instead of a class or interface using CALL BADI
CALL BADI lo_badi_custom_imp->method1(
EXPORTING
iv_err_text = lv_error_text
IMPORTING
et_return = lt_return
CHANGING
ct_data = lt_data ).

CALL BADI lo_badi_custom_imp->method2(
EXPORTING
it_input = lt_input
IMPORTING
et_output = lt_output
CHANGING
cv_flag = lv_flag ).

CATCH
cx_badi_context_error
cx_badi_filter_error
cx_badi_initial_context
cx_badi_multiply_implemented
cx_badi_unknown_errror
cx_badi_initial_reference
cx_badi_not_implemented
cx_sy_dyn_call_illegal_method.

* Handle if possible but this will help with dumping else wise

ENDTRY.