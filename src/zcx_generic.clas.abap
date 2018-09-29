class ZCX_GENERIC definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

*"* public components of class ZCX_GENERIC
*"* do not include other source files here!!!
public section.

  data T_MESSAGES type ZIMESSAGES read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !T_MESSAGES type ZIMESSAGES optional .
  methods ADD_TEXT
    importing
      !I_TYPE type MSGTY default 'E'
      !I_TEXT type SIMPLE .
  methods ADD_MESSAGE
    importing
      !IS_MESSAGE type DATA optional .
  methods ADD_MESSAGES
    importing
      !IT_MESSAGES type DATA .
  methods ADD_BAPIRET
    importing
      !IT_RETURN type BAPIRET2_T .
  methods ADD_ROOT
    importing
      !IX_ROOT type ref to CX_ROOT .
  methods ADD_OI_ERROR
    importing
      !IR_OI_ERROR type ref to I_OI_ERROR .
  methods ADD_LIST
    importing
      !IT_LIST type ZIMESSAGE_LIST .
  methods ADD_FAIL
    importing
      !IR_FAIL type ref to ZCX_GENERIC .
  methods MESSAGE .
  methods WRITE .
  methods SHOW .
  methods ASSERT .
  methods GET_BAPIRET
    returning
      value(ET_MESSAGES) type BAPIRET2_T .
  methods GET_POWL
    returning
      value(ET_MESSAGES) type POWL_MSG_TTY .
  methods GET_LIST
    returning
      value(ET_LIST) type ZIMESSAGE_LIST .
  methods RAISE_PROXY
    importing
      !I_CLASS type CLASSNAME default 'ZSRM_CX_STANDARD_MESSAGE_FAULT'
    raising
      CX_AI_APPLICATION_FAULT .
  methods GET_FAULT
    exporting
      !ES_FAULT type EXCHANGE_FAULT_DATA15 .
  class-methods RAISE
    importing
      !I_TYPE type MSGTY default 'E'
      !I_TEXT type DATA optional
      !IX_ROOT type ref to CX_ROOT optional
      !IT_MESSAGES type DATA optional
      !IT_LIST type ZIMESSAGE_LIST optional
      !IS_RETURN type BAPIRET2 optional
      !IT_RETURN type BAPIRET2_T optional
      !IT_PD type TABLE optional
      !IV_OBJ_TYPE type STRING optional
      !IR_OI_ERROR type ref to I_OI_ERROR optional
    raising
      ZCX_GENERIC .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
*"* protected components of class ZCX_GENERIC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_GENERIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_GENERIC IMPLEMENTATION.


method add_bapiret.

  data lt_messages like t_messages.
  lt_messages = zcl_message_static=>bapiret2msg( it_return ).

  insert lines of lt_messages into table t_messages.

endmethod.


method add_fail.

  insert lines of ir_fail->t_messages into table t_messages.

endmethod.


method add_list.

  data lt_messages type zimessages.
  lt_messages = zcl_message_static=>list2messages( it_list ).

  insert lines of lt_messages into table t_messages.

endmethod.


method add_message.

  if is_message is supplied.
    data ls_message like line of t_messages.
    ls_message = is_message.
  else.
    move-corresponding sy to ls_message.
  endif.

  insert ls_message into table t_messages.

endmethod.


method add_messages.

  zcl_abap_static=>table2table(
    exporting it_data = it_messages
    importing et_data = t_messages ).

endmethod.


method add_oi_error.

  data lt_messages type zimessages.
  lt_messages = zcl_message_static=>oi2messages( ir_oi_error ).

  insert lines of lt_messages into table t_messages.

endmethod.


method add_root.

  data lt_messages type zimessages.
  lt_messages = zcl_message_static=>root2messages( ix_root ).

  insert lines of lt_messages into table t_messages.

endmethod.


method add_text.

  data ls_message type zsmessage.
  ls_message =
    zcl_message_static=>get_message(
      i_type = i_type
      i_text = i_text ).

  add_message( ls_message ).

endmethod.


method assert.

  data ls_message like line of t_messages.
  loop at t_messages into ls_message where msgty ca 'EAX'.
    message id ls_message-msgid type 'X' number ls_message-msgno
      with ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
  endloop.

  assert 1 = 2.

endmethod.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->T_MESSAGES = T_MESSAGES .
  endmethod.


method get_bapiret.

  et_messages = zcl_message_static=>msg2bapiret( t_messages ).

endmethod.


method get_fault.

  es_fault-fault_text = get_text( ).

  data ls_message like line of t_messages.
  loop at t_messages into ls_message.

    data l_counter type i.
    add 1 to l_counter.

    field-symbols <ls_detail> like line of es_fault-fault_detail.
    append initial line to es_fault-fault_detail assigning <ls_detail>.

    <ls_detail>-id       = l_counter.
    <ls_detail>-text     = zcl_message_static=>get_text( is_message = ls_message ).

    condense <ls_detail>-id.

    case ls_message-msgty.
      when 'E' or 'A' or 'X'.
        <ls_detail>-severity = 'error'.
      when 'W'.
        <ls_detail>-severity = 'warning'.
      when 'S' or 'I'.
        <ls_detail>-severity = 'success'.
    endcase.

  endloop.

endmethod.


method get_list.

  et_list = zcl_message_static=>messages2list( t_messages ).

endmethod.


method get_powl.

  et_messages = zcl_message_static=>msg2powl( t_messages ).

endmethod.


method IF_MESSAGE~GET_TEXT.
*CALL METHOD SUPER->IF_MESSAGE~GET_TEXT
*  RECEIVING
*    RESULT =
*    .
  endmethod.


method message.

  data ls_message like line of t_messages.
  loop at t_messages into ls_message.

    message id ls_message-msgid type ls_message-msgty number ls_message-msgno
      with ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.

  endloop.

endmethod.


method raise.

  data lx_fail type ref to zcx_generic.
  create object lx_fail.

  if i_text is supplied.

    lx_fail->add_text(
      i_type = i_type
      i_text = i_text ).

  elseif it_messages is supplied.

    lx_fail->add_messages( it_messages ).

  elseif it_list is supplied.

    lx_fail->add_list( it_list ).

  elseif is_return is supplied.

    data lt_return like it_return.
    insert is_return into table lt_return.

    lx_fail->add_bapiret( lt_return ).

  elseif it_return is supplied.

    lx_fail->add_bapiret( it_return ).

  elseif ix_root is supplied.

    lx_fail->add_root( ix_root ).

  elseif ir_oi_error is supplied.

    lx_fail->add_oi_error( ir_oi_error ).

  else.

    lx_fail->add_message( ).

  endif.

  raise exception lx_fail.

endmethod.


method raise_proxy.

  data lt_messages type bapiret2_t.
  lt_messages = get_bapiret( ).

  cl_proxy_fault=>raise(
    exporting
      exception_class_name = i_class
      bapireturn_tab       = lt_messages ).

endmethod.


  method show.

    data lr_log type ref to zcl_log.
    create object lr_log.

    lr_log->add_messages( t_messages ).

    lr_log->show( ).

  endmethod.


method write.

  zcl_message_static=>write( t_messages ).

endmethod.
ENDCLASS.
