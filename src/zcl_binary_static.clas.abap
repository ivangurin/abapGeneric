class ZCL_BINARY_STATIC definition
  public
  final
  create public .

*"* public components of class ZCL_BINARY_STATIC
*"* do not include other source files here!!!
public section.

  class-methods TABLE2VALUE
    importing
      !I_LENGTH type SIMPLE
      !I_FIELD type SIMPLE optional
      !IT_DATA type TABLE
    returning
      value(E_DATA) type XSTRING
    raising
      ZCX_GENERIC .
  class-methods VALUE2TABLE
    importing
      !I_DATA type XSTRING
    exporting
      !ET_DATA type TABLE .
protected section.
*"* protected components of class ZCL_BINARY_STATIC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_BINARY_STATIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_BINARY_STATIC IMPLEMENTATION.


method TABLE2VALUE.

  data l_length type i.
  l_length = i_length.

  call function 'SCMS_BINARY_TO_XSTRING'
    exporting
      input_length = l_length
    importing
      buffer       = e_data
    tables
      binary_tab   = it_data
    exceptions
      failed       = 1
      others       = 2.
  if sy-subrc ne 0.
    zcx_generic=>raise( ).
  endif.

***  field-symbols <ls_data> type any.
***  loop at it_data assigning <ls_data>.
***
***    if e_data is initial.
***      e_data = <ls_data>.
***    else.
***      concatenate e_data <l_value> into e_data in byte mode.
***    endif.
***
***  endloop.
***
***  try.
***      e_data = e_data(l_length).
***      data lx_root type ref to cx_root.
***    catch cx_root into lx_root.
***      zcx_srm_generic_sy=>raise( ix_root = lx_root ).
***  endtry.

endmethod.


method VALUE2TABLE.

  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer     = i_data
    tables
      binary_tab = et_data.

endmethod.
ENDCLASS.
