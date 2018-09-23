class ZCL_MESSAGE_STATIC definition
  public
  create public .

*"* public components of class ZCL_MESSAGE_STATIC
*"* do not include other source files here!!!
public section.

  constants SUCCESS type MSGTY value 'S'. "#EC NOTEXT
  constants WARNING type MSGTY value 'W'. "#EC NOTEXT
  constants ERROR type MSGTY value 'E'. "#EC NOTEXT
  constants TYPE_SUCCESS type STRING value 'success'. "#EC NOTEXT
  constants TYPE_WARNING type STRING value 'warning'. "#EC NOTEXT
  constants TYPE_ERROR type STRING value 'error'. "#EC NOTEXT
  constants TYPE_INFO type STRING value 'info'. "#EC NOTEXT

  class-methods GET_TEXT
    importing
      !IS_MESSAGE type DATA optional
    returning
      value(E_TEXT) type STRING .
  class-methods GET_MESSAGE
    importing
      !I_TEXT type SIMPLE optional
      !I_TYPE type SYMSGTY default 'E'
    returning
      value(ES_MESSAGE) type ZSMESSAGE .
  class-methods MSG2BAPIRET
    importing
      !IT_MESSAGES type ZIMESSAGES
    returning
      value(ET_MESSAGES) type BAPIRET2_T .
  class-methods BAPIRET2MSG
    importing
      !IT_MESSAGES type BAPIRET2_T
    returning
      value(ET_MESSAGES) type ZIMESSAGES .
  class-methods BDC2MSG
    importing
      !IT_MESSAGES type ETTCD_MSG_TABTYPE
    returning
      value(ET_MESSAGES) type ZIMESSAGES .
  class-methods MSG2POWL
    importing
      !IT_MESSAGES type ZIMESSAGES
    returning
      value(ET_MESSAGES) type POWL_MSG_TTY .
  class-methods MSG2STR
    importing
      !IT_MESSAGES type ZIMESSAGES
    returning
      value(E_MESSAGES) type STRING .
  class-methods LIST2MESSAGES
    importing
      !IT_LIST type ZIMESSAGE_LIST
    returning
      value(ET_MESSAGES) type ZIMESSAGES .
  class-methods MESSAGES2LIST
    importing
      !IT_MESSAGES type ZIMESSAGES
    returning
      value(ET_LIST) type ZIMESSAGE_LIST .
  class-methods ADD_MESSAGE
    importing
      !IS_MESSAGE type ZSMESSAGE optional
    changing
      !CT_MESSAGES type ZIMESSAGES .
  class-methods ROOT2MESSAGES
    importing
      !IX_ROOT type ref to CX_ROOT
    returning
      value(ET_MESSAGES) type ZIMESSAGES .
  class-methods OI2MESSAGES
    importing
      !IR_OI type ref to I_OI_ERROR
    returning
      value(ET_MESSAGES) type ZIMESSAGES .
protected section.
*"* protected components of class ZCLSRM_MESSAGE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_MESSAGE_STATIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_MESSAGE_STATIC IMPLEMENTATION.


method add_message.

  if is_message is supplied.
    data ls_message like is_message.
    ls_message = is_message.
  else.
    move-corresponding sy to ls_message.
  endif.

  insert ls_message into table ct_messages.

endmethod.


method bapiret2msg.

  data ls_message like line of it_messages.
  loop at it_messages into ls_message.

    field-symbols <ls_message> like line of et_messages.
    append initial line to et_messages assigning <ls_message>.

    <ls_message>-msgty = ls_message-type.
    <ls_message>-msgid = ls_message-id.
    <ls_message>-msgno = ls_message-number.
    <ls_message>-msgv1 = ls_message-message_v1.
    <ls_message>-msgv2 = ls_message-message_v2.
    <ls_message>-msgv3 = ls_message-message_v3.
    <ls_message>-msgv4 = ls_message-message_v4.

  endloop.

endmethod.


method bdc2msg.

  data ls_message like line of it_messages.
  loop at it_messages into ls_message.

    field-symbols <ls_message> like line of et_messages.
    append initial line to et_messages assigning <ls_message>.

    move-corresponding ls_message to <ls_message>.

    <ls_message>-msgno = ls_message-msgnr.

  endloop.

endmethod.


method get_message.

  if i_text is not initial.

    data l_text(200).
    l_text = i_text.

    data l_dummy.
    message id 'EZ' type i_type number '001'
      with l_text(50) l_text+50(50) l_text+100(50) l_text+150(50)
      into l_dummy.

  endif.

  move-corresponding sy to es_message.

endmethod.


method get_text.

  if is_message is supplied.

    data ls_message type zsmessage.
    move-corresponding ls_message to ls_message.

  else.

    move-corresponding sy to ls_message.

  endif.

  message id ls_message-msgid type ls_message-msgty number ls_message-msgno
    with ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
    into e_text.

endmethod.


method list2messages.

  data ls_list like line of it_list.
  loop at it_list into ls_list.

    if ls_list-type eq 'E' or ls_list-type eq 'error'.
      data l_type.
      l_type = 'E'.
    elseif ls_list-type eq 'W' or ls_list-type eq 'warning'.
      l_type = 'W'.
    elseif ls_list-type eq 'S' or ls_list-type eq 'success'.
      l_type = 'S'.
    else.
      l_type = 'I'.
    endif.

    data ls_message like line of et_messages.
    ls_message =
      get_message(
        i_text = ls_list-text
        i_type = l_type ).

    insert ls_message into table et_messages.

  endloop.

endmethod.


method messages2list.

  data ls_message like line of it_messages.
  loop at it_messages into ls_message.

    field-symbols <ls_list> like line of et_list.
    append initial line to et_list assigning <ls_list>.

    <ls_list>-type = ls_message-msgty.
    <ls_list>-text = get_text( is_message = ls_message ).

  endloop.

endmethod.


method msg2bapiret.

  data ls_message like line of it_messages.
  loop at it_messages into ls_message.

    field-symbols <ls_message> like line of et_messages.
    append initial line to et_messages assigning <ls_message>.

    <ls_message>-type       = ls_message-msgty.
    <ls_message>-id         = ls_message-msgid.
    <ls_message>-number     = ls_message-msgno.
    <ls_message>-message_v1 = ls_message-msgv1.
    <ls_message>-message_v2 = ls_message-msgv2.
    <ls_message>-message_v3 = ls_message-msgv3.
    <ls_message>-message_v4 = ls_message-msgv4.

    <ls_message>-message    = get_text( ls_message ).

  endloop.

endmethod.


method msg2powl.

  data ls_message like line of it_messages.
  loop at it_messages into ls_message.

    field-symbols <ls_message> like line of et_messages.
    append initial line to et_messages assigning <ls_message>.

    <ls_message>-msgtype    = ls_message-msgty.
    <ls_message>-msgid      = ls_message-msgid.
    <ls_message>-msgnumber  = ls_message-msgno.
    <ls_message>-message_v1 = ls_message-msgv1.
    <ls_message>-message_v2 = ls_message-msgv2.
    <ls_message>-message_v3 = ls_message-msgv3.
    <ls_message>-message_v4 = ls_message-msgv4.

    <ls_message>-message    = get_text( ls_message ).

  endloop.

endmethod.


method msg2str.

  data ls_message like line of it_messages.
  loop at it_messages into ls_message.

    data l_message type string.
    l_message = get_text( is_message = ls_message ).

    if e_messages is initial.
      e_messages = l_message.
    else.
      concatenate e_messages l_message into e_messages separated by cl_abap_char_utilities=>newline.
    endif.

  endloop.

endmethod.


method oi2messages.

    data ls_message like line of et_messages.
    ir_oi->get_message(
      importing
        message_id     = ls_message-msgid
        message_number = ls_message-msgno
        param1         = ls_message-msgv1
        param2         = ls_message-msgv2
        param3         = ls_message-msgv3
        param4         = ls_message-msgv4 ).

    ls_message-msgty = 'E'.

    insert ls_message into table et_messages.

  endmethod.


method root2messages.

    data lt_return type bapiret2_t.
    call function 'RS_EXCEPTION_TO_BAPIRET2'
      exporting
        i_r_exception = ix_root
      changing
        c_t_bapiret2  = lt_return.

    et_messages = bapiret2msg( lt_return ).

  endmethod.
ENDCLASS.
