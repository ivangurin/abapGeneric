class ZCL_MIME_STATIC definition
  public
  final
  create public .

*"* public components of class ZCL_MIME_STATIC
*"* do not include other source files here!!!
public section.

  class-methods READ
    importing
      !I_PATH type STRING
      !I_NAME type STRING optional
    returning
      value(E_DATA) type XSTRING
    raising
      ZCX_GENERIC .
  class-methods SAVE
    importing
      !I_PATH type STRING
      !I_NAME type STRING optional
      !I_DATA type XSTRING .
  protected section.
*"* protected components of class ZCL_MIME_STATIC
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_MIME_STATIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_MIME_STATIC IMPLEMENTATION.


  method read.

    if i_name is initial.
      data l_url type string.
      l_url = i_path.
    else.
      concatenate i_path i_name into l_url.
    endif.

    data lr_api type ref to if_mr_api.
    lr_api ?= cl_mime_repository_api=>get_api( ).

    lr_api->get(
      exporting
        i_url     = l_url
      importing
        e_content = e_data ).

  endmethod.


  method save.

    if i_name is initial.
      data l_url type string.
      l_url = i_path.
    else.
      concatenate i_path i_name into l_url.
    endif.

    data lr_api type ref to if_mr_api.
    lr_api ?= cl_mime_repository_api=>get_api( ).

    lr_api->put(
      exporting
        i_url     = l_url
        i_content = i_data ).

  endmethod.
ENDCLASS.
