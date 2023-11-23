*----------------------------------------------------------*
* ZABAP_SPTA_FRAMEWORK
*
*----------------------------------------------------------*
* Parallel Processing with AIF call to backend:
* Parallel processing allows for multiple threads to be used when processing a request
* This framework allows for use of the SPTA_PARA_PROCESS_START_2
* This is an object oriented approach from standard SAP that allows a flexible work processing
* Still needs some work but will get to it when possible
*----------------------------------------------------------*



* Main Forms:
REPORT zspta_framework. 


INCLUDE:
top,
parallel_processing,
class,
exception_class.

FORM exception_class.
CLASS gcx_pp IMPLEMENTATION.

METHOD constructor.

Super->constructor().
ms_msg-msgid = iv_msgid.
ms_msg-msgno = iv_msgno.
ms_msg-msgty = iv_msgty.
ms_msg-msgv1 = iv_msgv1.
ms_msg-msgv2 = iv_msgv2.
ms_msg-msgv3 = iv_msgv3.
ms_msg-msgv4 = iv_msgv4.
mv_err_text = iv_err_text.
mv_excep_id = iv_excep_id.

ENDMETHOD.

ENDCLASS.
ENDFORM.

FORM top:

CLASS gcx_pp DEFINITION INHERITING FROM cx_static_check FINAL.

PUBLIC SECTION.

DATA:
mv_err_text TYPE string,
mv_excep_id TYPE sotr_conc,
ms_msg TYPE symsg.

METHODS:

CONSTRUCTOR
IMPORTING
iv_msgid TYPE symsgid OPTIONAL.
iv_msgno TYPE symsgno OPTIONAL.
iv_msgty TYPE symsgty OPTIONAL.
iv_msgv1 TYPE  syst_msgv OPTIONAL.
iv_msgv2 TYPE syst_msgv OPTIONAL.
iv_msgv3 TYPE syst_msgv OPTIONAL.
iv_msgv4 TYPE syst_msgv OPTIONAL.
iv_err_text TYPE string OPTIONAL.
iv_excep_id TYPE sotr_conc OPTIONAL.

ENDCLASS.

CONSTANTS:
gc_rfcgr_pg TYPE spta_rfcgr VALUE 'parallel_generators'.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.

PARAMETERS:
p_pacmin TYPE int4 DEFAULT 1, " Minimum packet size
p_pacmax TYPE int4 DEFAULT 50, "Max packet size
p_tasks TYPE int4 DEFAULT 5, "Number of tasks
p_timout TYPE wfcs_time_timeout DEFAULT 1800, "Timeout in seconds
p_servr TYPE spta_rfcgr OBLIGATORY DEFAULT gc_rfc_gr_pg. " Server group RZ12 t-code

SELECTION-SCREEN END OF BLOCK b01.

ENDFORM.

* Include parallel_processing.

* This include will consist of three logical processing units
* 1st RFC before submission to multi threading
* 2nd RFC during multi threading
* 3rd RFC after multi threading
* Each one is called per packet and needs to be debugged using external breakpoints

* Every time the task manager intends to launch a new parallel task, it calls the RFC_BEFORE
* callback form. In this form the application determines whether to start a new task and, if so, which
* parameters it needs to supply the new child task.
FORM f_before_rfc
USING
ft_before_rfc_imp TYPE spta_t_before_rfc_imp
CHANGING
ft_before_rfc_exp TYPE spta_t_before_rfc_exp
ft_rfcdata TYPE spta_t_indxtab
ft_failed_objects TYPE spta_t_failed_objects
ft_objects_in_process TYPE spta_t_objects_in_process
fs_user_param TYPE me->ty_rfc_user_param.

* Call a method from our parameters structure
fs_user_param-proc_instance->prepare_parallel_task(
IMPORTING
es_task_data = DATA(ls_rfc_data_in)
CHANGING
cs_before_rfc_exp = ft_before_rfc_exp
ct_objects_in_process = ft_objects_in_process ).

* Encode the RFC data for the IN_RFC form later
CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
EXPORTING
data = ls_rfc_data_in
IMPORTING
indxtab = ft_rfcdata.

ENDFORM.

FORM f_in_rfc
USING
ft_in_rfc_imp TYPE spta_t_in_rfc_imp
CHANGING
ft_in_rfc_exp TYPE spta_t_in_rfc_exp
ft_rfcdata TYPE spta_t_indxtab.

DATA:
ls_task_data TYPE me->ty_task_data.

CLEAR:
ls_task_data.

* Decode rfc_input data from previous form
CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
EXPORTING
indxtab = ft_rfcdata
IMPORTING
data = ls_task_data.

* Initiate processing of RFC
* We can configure the class method to allow for export, import, changing
me=>process_parallel_task(
EXPORTING
is_task_data = ls_task_data-importing
IMPORTING
es_task_data = ls_task_data-exporting
CHANGING
cs_task_data = ls_task_data-changing ).

* Repack the data for RFC_AFTER form later
CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
EXPORTING
data = ls_task_data
IMPORTING
indxtab = ft_rfcdata.

ENDFORM.

FORM f_after_rfc
USING
ft_rfcdata TYPE spta_t_indxtab
fv_rfcsubrc TYPE sy-subrc
ft_rfcmsg TYPE spta_t_rfcmsg
ft_objects_in_process TYPE spta_t_objects_in_process
ft_after_rfc_imp TYPE spta_t_after_rfc_imp
CHANGING
fs_after_rfc_exp TYPE spta_after_rfc_exp
fs_user_param TYPE me->ty_rfc_user_param.

DATA:
ls_task_data TYPE me->ty_task_data.

IF fv_rfcsubrc = 0.

* Decode in_rfc data from previous form
CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
EXPORTING
indxtab = ft_rfcdata
IMPORTING
data = ls_task_data.

ENDIF.


* Call instance that will finish the processing of the current packet
fs_user_param-proc_instance->finish_parallel_task(
EXPORTING
is_task_data = ls_task_data
iv_rfcsubrc = fv_rfcsubrc
iv_rfcmsg = fvrfcmsg
it_objects_in_process = ft_objects_in_process
CHANGING
cs_after_rfc_exp = fs_after_rfc_exp ).

ENDFORM.

* INCLUDE class:

PUBLIC SECTION.

BEGIN OF ty_rfc_user_param,
proc_instance TYPE REF TO me,
END OF ty_rfc_user_param,
BEGIN OF ty_importing,
object TYPE spta_t_objects_in_process,
package_seq TYPE I,
package_rec_cnt TYPE I,
* anything below is custom to requirements
test_run TYPE char1,
END OF ty_importing,
BEGIN OF ty_changing,
alv_t TYPE tty_alv,
data TYPE tty_data,
END of ty_changing,
BEGIN OF ty_exporting,
result_t TYPE tty_result,
END OF ty_exporting,

* Line type for parallel processing
BEGIN OF ty_task_data,
exporting TYPE ty_exporting,
importing TYPE ty_importing,
changing TYPE ty_changing,
END OF ty_task_data,
tty_task_data TYPE STANDARD TABLE OF ty_task_data.

CLASS-METHODS:
* In_RFC form call as class method due to split between application servers
process_parallel_task
EXPORTING
es_task_data TYPE ty_task_data-exporting
IMPORTING
is_task_data TYPE ty_task_data-importing
CHANGING
cs_task_data TYPE ty_task_data-changing.

METHODS:
* before_rfc form handler
prepare_parallel_task
EXPORTING
es_task_data TYPE ty_task_data
CHANGING
cs_before_rfc_exp TYPE spta_t_before_rfc_exp
ct_objects_in_process TYPE spta_objects_in_process,

* after_rfc from handler
finish_parallel_task
IMPORTING
is_task_data TYPE ty_task_data
iv_rfcsubrc TYPE sy-subrc
iv_rfcmsg TYPE spta_t_rfcmsg
it_objects_in_process TYPE spta_t_objects_in_process
CHANGING
cs_after_rfc_exp TYPE spta_t_after_rfc_exp.

PRIVATE SECTION.

* Need to map these to the selection screen during processing
CLASS-DATA:
dt_task_data TYPE tty_task_data,
dv_package_seq TYPE I,
dv_minpac TYPE int4,
dv_maxpac TYPE int4,
dv_tasks TYPE int4,
dv_servr TYPE spta_rfcgr,
dv_timout TYPE tw_task_timeout,
* Below are the custom requirements needs declarations
dv_test TYPE abap_bool.

CONSTANTS:
c_before_rfc TYPE char20 VALUE 'F_BEFORE_RFC',
c_in_rfc TYPE char20 VALUE 'F_IN_RFC',
c_after_rfc_mm TYPE char20 VALUE 'F_AFTER_RFC',
c_callback_prog TYPE sy-repid VALUE 'PUT_MAIN_FORM_HERE'.

METHODS:

* Handles the preparatory work before calling the framework
initiate_parallel_processing
IMPORTING
iv_test TYPE abap_bool
CHANGING
lt_data TYPE tty_data
RAISING
gcx_pp,

* Sets the package and system usage parameters
initialize_packets
IMPORTING
iv_test TYPE abap_bool
EXPORTING
et_task_data TYPE tty_task_data
CHANGING
ct_data TYPE tty_data,

calc_pack_size_para_process
IMPORTING
iv_number_keys TYPE I,
iv_pack_size_max TYPE I,
iv_pack_size_min TYPE I,
iv_prl_prc_max TYPE I.

CLASS IMPLEMENTATION:

METHOD initiate_parallel_process.

DATA:
lv_msgv1 TYPE syst_msgv.

IF ct_data IS INITIAL.
MESSAGE
RETURN.
ENDIF.

* Populate the task data and other information into packets for processing
me->initialize_packets(
EXPORTING
iv_test = iv_test
IMPORTING
et_task_data = dt_task_data
CHANGING
ct_data = ct_data ).

* Call the Simple Parallel Task Administrator
   CALL FUNCTION 'SPTA_PARA_PROCESS_START_2' 
     EXPORTING 
       server_group = dv_servr
       max_no_of_tasks = dv_tasks
       before_rfc_callback_form = c_BEFORE_RFC 
       in_rfc_callback_form = c_IN_RFC 
       after_rfc_callback_form = c_AFTER_RFC
       callback_prog = c_callback_prog
       resource_timeout = dv_timout
     CHANGING 
       user_param = ls_rfc_user_param
     EXCEPTIONS 
       invalid_server_group = 1 
       no_resources_available = 2 
     OTHERS = 3. 
CASE sy-subrc.
WHEN 0.
* Do nothing
WHEN 1.
lv_msgv1 = dv_servr.

RAISE EXCEPTION TYPE gcx_pp
EXPORTING
iv_msgid = 'Put_your_message_class'
iv_msgno = 'Put your message number' "Invalid Server Group
iv_msgty = 'E'
iv_msgv1 = lv_msgv1

WHEN 2.
RAISE EXCEPTION TYPE gcx_pp
EXPORTING
iv_msgid = 'Put_your_message_class'
iv_msgno = 'Put your message number' "No Resources available for processing
iv_msgty = 'E'

WHEN OTHERS.
RAISE EXCEPTION TYPE gcx_pp
EXPORTING
iv_msgid = 'Put_your_message_class'
iv_msgno = 'Put your message number' "An exception occurred while starting parallel process
iv_msgty = 'E'

ENDMETHOD.

METHOD initialize_packets.

DATA:
lv_rec_cnt TYPE I,
ls_object TYPE LINE OF spta_t_objects_in_process,
lt_data TYPE tty_data,
lt_object TYPE spta_t_objects_in_process,
lv_packsize_cnt TYPE I,
ls_task_data TYPE ty_task_data.

CLEAR:
et_task_datam lt_data, ls_object,
lt_object, ls_task_data.

lv_rec_cnt = lines( ct_data ).

* Optimized package size
DATA(lv_packsize_opt) = me->calc_pack_size_para_process(
EXPORTING
iv_number_of_keys = lv_rec_cnt
iv_pack_size_max = dv_maxpac
iv_pack_size_min = dv_minpac
iv_prl_prc_max = dv_tasks ).

* Build the packets based on data available
LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<lfs_data>).

DATA(lv_tabix) = sy-tabix.

* Build Object key based on unique combinations
ls_object-obj_id = <lfs_data>-key1 && <lfs_data>-key2.
APPEND ls_object TO lt_object.

APPEND <lfs_data> TO lt_data.

* We want to make sure locking does not occur so we group data together as needed
AT END OF key.

* Increment packet count
lv_packsize_cnt += 1.

* Start building the individual include portions of task data
APPEND LINES OF lt_data TO ls_task_data-changing-data_t.
APPEND LINES OF lt_object TO ls_task_data-importing-object.

CLEAR:
lt_object, lt_data.

IF lv_packsize_cnt >= lv_packsize_opt
OR lv_tabix = lv_rec_cnt.

* Add any individual fields to the importing, exporting, or changing structures
ls_task_data-importing-test_run = iv_test.

* When finished working the task data we append to the exporting table
APPEND ls_task_data TO et_task_data.

CLEAR: 
lv_packsize_cnt, ls_task_data.

ENDIF.
ENDAT.

ENDLOOP.

* Make sure we covered all scenarios
IF NOT ls_task_data IS INITIAL.


APPEND ls_task_data TO et_task_data.
CLEAR:
lv_packsize_cnt, ls_task_data.

ENDIF.

* Put helpful message for sm37 output help and clarity
IF sy-batch EQ abap_true.
DATA(lv_packet_count) = lines ( et_task_data ).
MESSAGE s000(msg_class) WITH 'Number of packets:' lv_packet_count.
ENDIF.

ENDMETHOD.

METHOD calc_pack_size_para_process.

CONSTANTS:
lc_num_packs_rel_diff TYPE I VALUE 2.

DATA:
lv_last_pack_size TYPE I,
lv_prl_opt_cnt TYPE I,
lv_num_packs TYPE I,
lv_cnt TYPE I,
lv_opt TYPE boole_d,
lv_num_packs_prl TYPE I,
lv_num_packs_tmp_prl TYPE I,
lv_num_packs_tmp TYPE I,
lv_first_do_call TYPE boole_d,
lv_first_do_call0 TYPE boole_d,
lv_pack_size_tmp TYPE I,
lv_pack_size_min TYPE I,
lv_num_packs_rel TYPE I,
lv_num_packs_opt TYPE I,
lv_pack_size_max_opt TYPE tb_par_msize,
lv_num_packs_rel_opt TYPE I,
lv_num_packs_last TYPE I,
lv_num_packs_rel_last TYPE I,
lv_pack_size_max_last TYPE tb_par_msize,
lv_num_packs_rel_min TYPE I VALUE 100.

IF iv_pack_size_max < = 0 
OR iv_number_keys = 0
OR iv_pack_size_min > iv_pack_size_max
OR iv_prl_prc_max <= 1.
RAISE EXCEPTION TYPE gcx_pp
EXPORTING
iv_msgid = 'Put_your_message_class'
iv_msgno = 'Put your message number' "Missing thread processing information”
iv_msgty = 'E'

ENDIF.

IF iv_number_keys LE iv_pack_size_min.
rv_pack_size = iv_number_keys.
RETURN.
ENDIF.

* Number of iterations for optimization
IF iv_prl_prc_max <= 10.
lv_prl_opt_cnt = 5.
ELSEIF  iv_prl_prc_max <= 50.
lv_prl_opt_cnt = 4.
ELSE.
lv_prl_opt_cnt = 3.
ENDIF.

* Determine required number of packets
rv_pack_size = iv_pack_size_max.
lv_num_packs = iv_number_keys div rv_pack_size.
lv_last_pack_size = iv_number_keys MOD rv_pack_size.

lv_pack_size_min = iv_pack_size_min.
IF lv_pack_size_min = 0.
* Minimum packet size must be > 0
lv_pack_size_min = 1.
ENDIF.

lv_cnt = 0.
lv_first_do_call0 = abap_true.

IF lv_last_pack_size > 0 AND lv_last_pack_size >= lv_pack_size_min.
lv_num_packs += 1.
ENDIF.

lv_num_packs_opt = lv_num_packs_last = lv_num_packs.
lv_pack_size_max_opt = lv_pack_size_max_last = rv_pack_size.
lv_num_packs_prl = 

IF lv_num_packs < lv_num_packs_prl.

DO lv_prl_opt_cnt TIMES.

lv_cnt += 1.
lv_num_packs_prl = iv_prl_prc_max * lv_cnt.

* Maximum number of packages=> target for optimization.
IF lv_cnt > 1.

* Reduce the minimum percentage of parallel processes used per iteration
lv_num_packs_rel_min -=  lc_num_packs_rel_diff.
* Determine starting max packet size
lv_pack_size_tmp = iv_number_keys DIV lv_num_packs_prl.
IF rv_pack_size < lv_pack_size_tmp
AND lv_pack_size_tmp >= lv_pack_size_min
AND  lv_pack_size_tmp <= lv_pack_size_max
AND  lv_pack_size_tmp > 0.
rv_pack_size = lv_pack_size_tmp.
ENDIF.
ENDIF.

* End of optimization
IF rv_pack_size <= lv_pack_size_min
OR rv_pack_size < 2
OR ( lv_num_packs_rel_opt >= lv_num_packs_rel_min
AND lv_num_packs_rel_opt <= 100 ).
* Optimum solution found
EXIT.
ENDIF.

CLEAR:
lv_opt.

lv_first_do_call = abap_true.

lv_num_packs = iv_number_keys DIV rv_pack_size.
lv_last_pack_size = iv_number_keys MOD rv_pack_size.
IF lv_last_pack_size > 0 AND lv_last_pack_size >= lv_pack_size_min.
lv_num_packs +=1.
ENDIF.

IF lv_num_packs = lv_num_packs_prl.
* Optimum solution found
lv_num_packs_opt = lv_num_packs.
lv_pack_size_max_opt = rv_pack_size.
lv_num_packs_rel_opt = lv_num_packs_rel = 100.
CLEAR:
lv_first_do_call.
EXIT.

ELSE.

WHILE lv_opt = abap_false.

IF lv_cnt = 1.
lv_num_packs_rel_last = 100 * lv_num_packs / lv_num_packs_prl.
ELSEIF lv_first_do_call = abap_false
OR lv_first_do_call0 = abap_true.
lv_num_packs_tmp_prl = iv_prl_prc_max * ( lv_cnt - 1 ).
lv_num_packs_tmp = lv_num_packs - lv_num_packs_tmp_prl.
lv_num_packs_tmp_prl = lv_num_packs_prl - lv_num_packs_tmp_prl.
lv_num_packs_rel_last = 100 * lv_num_packs_tmp / lv_num_packs_tmp_prl.
ENDIF.

IF lv_num_packs_rel_opt IS INITIAL.
lv_num_packs_rel_opt = lv_num_packs_rel_last.
ENDIF.

rv_pack_size -= 1.

lv_num_packs = iv_number_keys DIV rv_pack_size.
lv_last_pack_size = iv_number_keys MOD rv_pack_size.

IF lv_last_pack_size > 0 AND lv_last_pack_size >= lv_pack_size_min.
lv_num_packs +=1.
ENDIF.

CLEAR:
lv_first_do_call, lv_first_do_call0.

IF lv_num_packs = lv_num_packs_prl.
* Optimum solution found
lv_opt = abap_true.
lv_num_packs_opt = lv_num_packs_last = lv_num_packs.
lv_num_packs -= 1.
lv_pack_size_max_opt = lv_pack_size_max_last = rv_pack_size.
lv_num_packs_rel_opt = lv_num_packs_rel = 100.
EXIT.

ELSE.

IF lv_cnt = 1.
lv_num_packs_rel = 100 * lv_num_packs / lv_num_packs_prl.
ELSE.
lv_num_packs_tmp_prl = iv_prl_prc_max * ( lv_cnt - 1 ).
lv_num_packs_tmp = lv_num_packs - lv_num_packs_tmp_prl.
lv_num_packs_tmp_prl = lv_num_packs_prl - lv_num_packs_tmp_prl.
lv_num_packs_rel_last = 100 * lv_num_packs_tmp / lv_num_packs_tmp_prl.
ENDIF.

lv_num_packs_last = lv_num_packs.
lv_pack_size_max_last = rv_pack_size.

* Check relative usage of parallel processes
IF lv_num_packs_rel >= 100.
lv_opt = abap_true. 

ELSEIF lv_num_packs_rel < 0.
lv_opt = abap_true.

ELSEIF lv_num_packs_rel_opt > 100
OR ( lv_num_packs_rel > lv_num_packs_rel_last 
AND lv_num_packs_rel > lv_num_packs_rel_opt ).

lv_num_packs_opt = lv_num_packs.
IF lv_last_pack_size > 0 AND lv_last_pack_size >= lv_pack_size_min.
lv_num_packs += 1. 
ENDIF.

lv_pack_size_max_opt = rv_pack_size.
lv_num_packs_rel_opt = lv_num_packs_rel.

ENDIF.
ENDIF.

IF  rv_pack_size <= lv_pack_size_min
OR  rv_pack_size < 2
OR ( lv_num_packs_rel >= lv_num_packs_rel_min 
AND lv_num_packs_rel <= 100 ).
lv_opt = abap_true.
ENDIF.

ENDWHILE.
ENDIF.

ENDDO.
ENDIF.

* Take the optimum values and return for output.
rv_pack_size = lv_pack_size_max_opt.

ENDMETHOD.

METHOD prepare_parallel_task.

READ TABLE dt_task_data
INDEX 1 ASSIGNING FIELD-SYMBOL(<lfs_task_data>).
IF sy-subrc EQ 0.

*Increment the packet index
<lfs_task_data>-importing-package_seq = dv_package_seq += 1.

* Append the exporting values to be carried into next PP workflow
es_task_data = VALUE ty_task_data(
importing = <lfs_task_data>-importing
exporting = <lfs_task_data>-exporting
changing = <lfs_task_data>-changing ).

* Add task identifiers for task manager
APPEND LINES OF <lfs_task_data>-importing-object
TO ct_objects_in_process.

* Raise message for user. Logic will work better with message class entry
* Modify how you need it to be
IF sy-batch EQ abap_true.
MESSAGE i000(Message_class) WITH
'Packet' &1<lfs_task_data>-importing-package_seq 
'WITH' &2<lfs_task_data>-importing-package_rec_cnt
' record(s) submitted for processing.'
ENDIF. 

* Remove our exporting task segment before next process.
DELETE dt_task_data INDEX 1.

* We tell task manager that validation was okay
cs_before_rfc_exp-start_rfc = abap_true.

ELSE.
* No packages found or validations failed
cs_before_rfc_exp-start_rfc = abap_false.
ENDIF.

ENDMETHOD.

METHOD process_parallel_task.

* Here is where most of our processing main logic will be appended
* If using instance methods we will need to instantiate the class method each task
* messages or raises are collected by the task manager

* Example class instantiation
IF go_class IS NOT BOUND.

go_class = NEW gcl_class( Importing ev_foo = bar exporting iv_foo = bar_e ).
ENDIF.

ENDMETHOD.

METHOD finish_parallel_task.

* Check parallel task return code
IF iv_rfcsubrc EQ 0.

* Message for user
IF sy-batch EQ abap_true.
* Packet &1 processing completed
MESSAGE i000(message_class) WITH is_task_data-importing-package_seq.
ENDIF.

ELSE.

* Show user the error encountered
MESSAGE i999(message_class) WITH iv_rfcmsg.

ENDIF.

* Populate any type of ALV or output
* Doing in this way allows for all the multi threads to be collected into one table
IF is_task_data-importing-test_run EQ abap_true.
APPEND LINES OF is_task_data-changing-alv_t TO dt_data_alv.
ENDIF.

ENDMETHOD.