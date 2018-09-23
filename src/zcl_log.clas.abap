class ZCL_LOG definition
  public
  final
  create public .

*"* public components of class ZCL_LOG
*"* do not include other source files here!!!
public section.

  data HANDLE type BALLOGHNDL read-only .

  class-methods CREATE
    importing
      !I_OBJECT type SIMPLE optional
      !I_SUBOBJECT type SIMPLE optional
      !I_NUMBER type SIMPLE optional
      !IT_MESSAGES type ZIMESSAGES optional
    preferred parameter IT_MESSAGES
    returning
      value(ER_LOG) type ref to ZCL_LOG
    raising
      ZCX_GENERIC .
  class-methods GET
    importing
      !I_HANDLE type SIMPLE
    returning
      value(ER_LOG) type ref to ZCL_LOG
    raising
      ZCX_GENERIC .
  methods CONSTRUCTOR
    importing
      !I_OBJECT type SIMPLE optional
      !I_SUBOBJECT type SIMPLE optional
      !I_NUMBER type SIMPLE optional
      !I_HANDLE type SIMPLE optional
      !IT_MESSAGES type ZIMESSAGES optional
    raising
      ZCX_GENERIC .
  methods ADD_MESSAGE
    importing
      !IS_MESSAGE type ZSMESSAGE optional
    raising
      ZCX_GENERIC .
  methods ADD_MESSAGES
    importing
      !IT_MESSAGES type ZIMESSAGES
    raising
      ZCX_GENERIC .
  methods GET_MESSAGES
    returning
      value(ET_MESSAGES) type ZIMESSAGES
    raising
      ZCX_GENERIC .
  methods SAVE
    importing
      !I_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_GENERIC .
  methods SHOW
    raising
      ZCX_GENERIC .
  methods FREE
    raising
      ZCX_GENERIC .
protected section.
*"* protected components of class ZCL_LOG
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_LOG
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_LOG IMPLEMENTATION.


method add_message.

  if is_message is supplied.
    data ls_message type bal_s_msg.
    move-corresponding is_message to ls_message.
  else.
    move-corresponding sy to ls_message.
  endif.

  call function 'BAL_LOG_MSG_ADD'
    exporting
      i_log_handle = handle
      i_s_msg      = ls_message
    exceptions
      others       = 1.
  if sy-subrc ne 0.
    zcx_generic=>raise( ).
  endif.

endmethod.


method add_messages.

  data ls_message like line of it_messages.
  loop at it_messages into ls_message.
    add_message( ls_message ).
  endloop.

endmethod.


method constructor.

  if i_handle is not initial.

    handle = i_handle.

  else.


    data ls_log type bal_s_log.
    ls_log-object    = i_object.
    ls_log-subobject = i_subobject.
    ls_log-extnumber = i_number.

    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log      = ls_log
      importing
        e_log_handle = handle
      exceptions
        others       = 1.
    if sy-subrc ne 0.
      zcx_generic=>raise( ).
    endif.

  endif.

  add_messages( it_messages ).

endmethod.


method create.

  create object er_log
    exporting
      i_object    = i_object
      i_subobject = i_subobject
      i_number    = i_number
      it_messages = it_messages.

endmethod.


method free.

  data lt_handle type bal_t_logh.
  insert handle into table lt_handle.

  call function 'BAL_GLB_MEMORY_REFRESH'
    exporting
      i_t_logs_to_be_refreshed = lt_handle
    exceptions
      others                   = 1.
  if sy-subrc ne 0.
    zcx_generic=>raise( ).
  endif.

endmethod.


method get.

  create object er_log
    exporting
      i_handle = i_handle.

endmethod.


method get_messages.

  data lt_handle type bal_t_logh.
  insert handle into table lt_handle.

  data lt_msg_handles type bal_t_msgh.
  call function 'BAL_GLB_SEARCH_MSG'
    exporting
      i_t_log_handle = lt_handle
    importing
      e_t_msg_handle = lt_msg_handles
    exceptions
      msg_not_found  = 1
      others         = 2.
  if sy-subrc <> 0.
    zcx_generic=>raise( ).
  endif.

  data ls_msg_handle like line of lt_msg_handles.
  loop at lt_msg_handles into ls_msg_handle.

    data ls_message type bal_s_msg.
    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = ls_msg_handle
      importing
        e_s_msg        = ls_message
      exceptions
        log_not_found  = 1
        msg_not_found  = 2
        others         = 3.
    if sy-subrc <> 0.
      zcx_generic=>raise( ).
    endif.

    field-symbols <ls_message> like line of et_messages.
    append initial line to et_messages assigning <ls_message>.

    move-corresponding ls_message to <ls_message>.

  endloop.

endmethod.


method save.

  data lt_handle type bal_t_logh.
  insert handle into table lt_handle.

  call function 'BAL_DB_SAVE'
    exporting
      i_in_update_task = abap_true
      i_t_log_handle   = lt_handle
    exceptions
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      others           = 4.
  if sy-subrc ne 0.
    zcx_generic=>raise( ).
  endif.

  if i_commit eq abap_true.
    zcl_abap_static=>commit( ).
  endif.

endmethod.


method SHOW.

  data ls_profile type bal_s_prof.
  call function 'BAL_DSP_PROFILE_POPUP_GET'
    importing
      e_s_display_profile = ls_profile.

  ls_profile-title             = text-001.
  ls_profile-use_grid          = abap_true.
  ls_profile-disvariant-report = sy-repid.
  ls_profile-disvariant-handle = handle.

  data lt_handle type bal_t_logh.
  insert handle into table lt_handle.

  call function 'BAL_DSP_LOG_DISPLAY'
    exporting
      i_s_display_profile = ls_profile
      i_t_log_handle      = lt_handle
    exceptions
      others              = 1.
  if sy-subrc ne 0.
    zcx_generic=>raise( ).
  endif.

endmethod.
ENDCLASS.
