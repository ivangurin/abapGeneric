class ZCL_TEXT definition
  public
  final
  create public .

*"* public components of class ZCL_TEXT
*"* do not include other source files here!!!
public section.

  data TEXT type STRING read-only .

  methods CONSTRUCTOR
    importing
      !I_TEXT type SIMPLE optional .
  methods GET
    returning
      value(E_TEXT) type STRING .
  methods SET
    importing
      !I_TEXT type SIMPLE .
  methods ADD
    importing
      !I_TEXT type SIMPLE
      !I_SEPARATOR type SIMPLE optional .
  methods ADD_SPACE .
  methods ADD_COMMA .
  methods ADD_POINT .
  methods ADD_SLASH .
  methods ADD_VALUE
    importing
      !I_VALUE type SIMPLE .
  methods ADD_NEWLINE .
  protected section.
*"* protected components of class ZCL_TEXT
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_TEXT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_TEXT IMPLEMENTATION.


  method add.

    if i_separator is initial.

      text = text && i_text.

    else.

      text = text && i_separator && i_text.

    endif.

  endmethod.


  method add_comma.

    text = text && `,`.

  endmethod.


  method add_newline.

    text = text && cl_abap_char_utilities=>newline.

  endmethod.


  method add_point.

    text = text && `.`.

  endmethod.


  method add_slash.

    text = text && `/`.

  endmethod.


  method add_space.

    text = text && ` `.

  endmethod.


  method add_value.

    text = text && zcl_abap_static=>write( i_value ).

  endmethod.


  method constructor.

    text = i_text.

  endmethod.


  method get.

    e_text = text.

  endmethod.


  method set.

    text = i_text.

  endmethod.
ENDCLASS.
