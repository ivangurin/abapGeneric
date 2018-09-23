class zcl_stack_static definition
  public
  final
  create public .

*"* public components of class ZCL_STACK_STATIC
*"* do not include other source files here!!!
  public section.

    type-pools abap .
    class-methods check
      importing
        !i_prog_name    type dbglprog optional
        !i_event_type   type dbglevtype optional
        !i_event_name   type dbglevent optional
      returning
        value(e_result) type abap_bool .
  protected section.
*"* protected components of class ZCL_STACK_STATIC
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_STACK_STATIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_STACK_STATIC IMPLEMENTATION.


  method check.

    data lt_stack type sys_callst.
    call function 'SYSTEM_CALLSTACK'
      importing
        et_callstack = lt_stack.

    data ls_range type zsrange.
    ls_range-sign   = 'I'.
    ls_range-option = 'EQ'.

    data lt_progname type zirange.
    if i_prog_name is supplied.
      ls_range-low = i_prog_name.
      insert ls_range into table lt_progname.
    endif.

    data lt_eventtype type zirange.
    if i_event_type is supplied.
      ls_range-low = i_event_type.
      insert ls_range into table lt_eventtype.
    endif.

    data lt_eventname type zirange.
    if i_event_name is supplied.
      ls_range-low = i_event_name.
      insert ls_range into table lt_eventname.
    endif.

    loop at lt_stack transporting no fields
      where
        progname  in lt_progname and
        eventtype in lt_eventtype and
        eventname in lt_eventname.
      e_result = abap_true.
      return.
    endloop.

  endmethod.
ENDCLASS.
