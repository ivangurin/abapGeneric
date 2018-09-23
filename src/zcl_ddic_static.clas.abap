class ZCL_DDIC_STATIC definition
  public
  final
  create public .

*"* public components of class ZCL_DDIC_STATIC
*"* do not include other source files here!!!
public section.

  class-methods GET_DOMA_VALUES
    importing
      !I_NAME type SIMPLE
    returning
      value(ET_VALUES) type ZIVALUES .
  class-methods GET_DOMA_VALUE_TEXT
    importing
      !I_NAME type SIMPLE
      !I_VALUE type SIMPLE
    returning
      value(E_TEXT) type STRING .
  protected section.
*"* protected components of class ZCL_DDIC_STATIC
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_DDIC_STATIC IMPLEMENTATION.


  method get_doma_values.

    select domvalue_l as id ddtext as text
      from dd07t
      into table et_values
      where
        domname    eq i_name and
        ddlanguage eq sy-langu and
        as4local   eq 'A'.

  endmethod.


  method get_doma_value_text.

    data l_id type string.
    concatenate i_name '/' i_value into l_id.

    try.
        zcl_cache_static=>get_data(
          exporting
            i_name = 'ZCL_DDIC_STATIC=>GET_DOMA_VALUE_TEXT'
            i_id   = l_id
          importing
            e_data = e_text ).
        return.
      catch cx_root.
    endtry.

    select single ddtext
      from dd07t
      into e_text
      where
        domname    eq i_name and
        ddlanguage eq sy-langu and
        as4local   eq 'A' and
        domvalue_l eq i_value.
    if sy-subrc is initial.
      e_text = i_value.
    endif.

    zcl_cache_static=>set_data(
      i_name = 'ZCL_DDIC_STATIC=>GET_DOMA_VALUE_TEXT'
      i_id   = l_id
      i_data = e_text ).

  endmethod.
ENDCLASS.
