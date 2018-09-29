class zcl_bdc definition
  public
  final
  create public .

*"* public components of class ZCL_BDC
*"* do not include other source files here!!!
  public section.
    type-pools abap .

    data mode type char1 value 'E' ##NO_TEXT.              " .
    data update type char1 value 'S' ##NO_TEXT.            " .
    data data type hrtb_bdcdata read-only .

    methods add_screen
      importing
        !i_prog   type simple
        !i_number type simple .
    methods add_action
      importing
        !i_action type simple .
    methods add_field
      importing
        !i_name  type simple
        !i_value type simple .
    methods run
      importing
        !i_trans           type simple
      returning
        value(et_messages) type zimessages .
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
