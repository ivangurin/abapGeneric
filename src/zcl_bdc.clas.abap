class ZCL_BDC definition
  public
  final
  create public .

*"* public components of class ZCL_BDC
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  data MODE type CHAR1 value 'E'. "#EC NOTEXT             " .
  data UPDATE type CHAR1 value 'S'. "#EC NOTEXT           " .
  data DATA type HRTB_BDCDATA read-only .

  methods ADD_SCREEN
    importing
      !I_PROG type SIMPLE
      !I_NUMBER type SIMPLE .
  methods ADD_ACTION
    importing
      !I_ACTION type SIMPLE .
  methods ADD_FIELD
    importing
      !I_NAME type SIMPLE
      !I_VALUE type SIMPLE .
  methods RUN
    importing
      !I_TRANS type SIMPLE
    returning
      value(ET_MESSAGES) type ZIMESSAGES .
protected section.
*"* protected components of class ZCL_BDC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_BDC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_BDC IMPLEMENTATION.


method add_action.

  field-symbols <ls_data> like line of data.
  append initial line to data assigning <ls_data>.

  <ls_data>-fnam = 'BDC_OKCODE'.
  <ls_data>-fval = i_action.

endmethod.


method add_field.

  field-symbols <ls_data> like line of data.
  append initial line to data assigning <ls_data>.

  <ls_data>-fnam = i_name.
  <ls_data>-fval = i_value.

endmethod.


method add_screen.

  field-symbols <ls_data> like line of data.
  append initial line to data assigning <ls_data>.

  <ls_data>-dynbegin = abap_true.
  <ls_data>-program  = i_prog.
  <ls_data>-dynpro   = i_number.

endmethod.


method run.

  data lt_messages type ettcd_msg_tabtype.
  call transaction i_trans
    using data
    mode mode
    update update
    messages into lt_messages.

  et_messages = zcl_message_static=>bdc2msg( lt_messages ).

endmethod.
ENDCLASS.
