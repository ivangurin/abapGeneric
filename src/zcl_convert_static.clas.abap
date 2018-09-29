class ZCL_CONVERT_STATIC definition
  public
  final
  create public .

public section.

  class-methods TEXT2XTEXT
    importing
      !I_TEXT type SIMPLE
      !I_ENCODING type ABAP_ENCODING default 'DEFAULT'
    returning
      value(E_XTEXT) type XSTRING .
  class-methods XTEXT2TEXT
    importing
      !I_XTEXT type SIMPLE
      !I_ENCODING type ABAP_ENCODING default 'DEFAULT'
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  class-methods TEXT2BASE64
    importing
      !I_TEXT type SIMPLE
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  class-methods BASE642TEXT
    importing
      !I_TEXT type SIMPLE
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  class-methods ITF2TEXT
    importing
      !IT_ITF type TLINETAB
    returning
      value(E_TEXT) type STRING .
  class-methods TEXT2ITF
    importing
      !I_TEXT type SIMPLE
    returning
      value(ET_ITF) type TLINETAB .
  class-methods CSV2TABLE
    importing
      !IT_DATA type STRINGTAB
    exporting
      !ET_DATA type TABLE .
  class-methods TABLE2CSV
    importing
      !IT_DATA type ANY TABLE
    returning
      value(ET_DATA) type STRINGTAB .
  class-methods DATA2JSON
    importing
      !I_DATA type DATA
    returning
      value(E_JSON) type STRING
    raising
      ZCX_GENERIC .
  class-methods JSON2DATA
    importing
      !I_JSON type SIMPLE
    exporting
      !E_DATA type DATA .
  class-methods DATA2XML
    importing
      !I_DATA type DATA
    returning
      value(E_XML) type STRING
    raising
      ZCX_GENERIC .
  class-methods XML2DATA
    importing
      !I_XML type SIMPLE
    exporting
      !E_DATA type DATA
    raising
      ZCX_GENERIC .
  class-methods XTABLE2XTEXT
    importing
      !I_LENGTH type SIMPLE
      !I_FIELD type SIMPLE optional
      !IT_DATA type TABLE
    returning
      value(E_DATA) type XSTRING
    raising
      ZCX_GENERIC .
  class-methods XTEXT2XTABLE
    importing
      !I_DATA type XSTRING
    exporting
      !ET_DATA type TABLE .
  class-methods XTEXT2VARCHAR
    importing
      !I_XTEXT type XSTRING
    returning
      value(E_VARCHAR) type STRING .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_CONVERT_STATIC IMPLEMENTATION.


  method base642text.

    if i_text is initial.
      return.
    endif.

    data l_text type string.
    l_text = i_text.

    constants lc_op_dec type x value 37.
    data l_xtext type xstring.
    call 'SSF_ABAP_SERVICE'
      id 'OPCODE'  field lc_op_dec
      id 'BINDATA' field l_xtext
      id 'B64DATA' field l_text.

    e_text = xtext2text( l_xtext ).

  endmethod.


  method csv2table.

    data lr_converter type ref to cl_rsda_csv_converter.
    lr_converter =
      cl_rsda_csv_converter=>create(
        i_separator = ';' ).

    data l_data like line of it_data.
    loop at it_data into l_data.

      data l_data_c(65535).
      l_data_c = l_data.

      field-symbols <ls_data> type any.
      append initial line to et_data assigning <ls_data>.

      lr_converter->csv_to_structure(
        exporting i_data   = l_data_c
        importing e_s_data = <ls_data> ).

      data lr_descr type ref to cl_abap_structdescr.
      if lr_descr is not bound.
        lr_descr ?= cl_abap_structdescr=>describe_by_data( <ls_data> ).
      endif.

      data ls_component like line of lr_descr->components.
      loop at lr_descr->components into ls_component
        where type_kind eq cl_abap_typedescr=>typekind_date.

        field-symbols <l_value> type any.
        assign component ls_component-name of structure <ls_data> to <l_value>.

        if <l_value> eq '0'.
          data l_date type d.
          <l_value> = l_date.
        endif.

      endloop.

    endloop.

  endmethod.


  method data2json.

    data lr_json type ref to cl_trex_json_serializer.
    create object lr_json
      exporting
        data = i_data.

    lr_json->serialize( ).

    e_json = lr_json->get_data( ).

  endmethod.


  method data2xml.

    try.
        call transformation id
          source data = i_data
          result xml e_xml.
      catch cx_root.
        zcx_generic=>raise( ).
    endtry.

  endmethod.


  method itf2text.

    data lt_stream type string_table.
    call function 'CONVERT_ITF_TO_STREAM_TEXT'
      exporting
        lf           = 'X'
      importing
        stream_lines = lt_stream
      tables
        itf_text     = it_itf.

    data l_stream like line of lt_stream.
    loop at lt_stream into l_stream.
      if sy-tabix = 1.
        e_text = l_stream.
      else.
        concatenate e_text l_stream into e_text separated by cl_abap_char_utilities=>newline.
      endif.
    endloop.

  endmethod.


  method json2data.

    data l_json type string.
    l_json = i_json.

    data lr_json type ref to cl_trex_json_deserializer.
    create object lr_json.

    lr_json->deserialize(
      exporting
        json = l_json
      importing
        abap = e_data ).

  endmethod.


  method table2csv.

    data lr_converter type ref to cl_rsda_csv_converter.
    lr_converter =
      cl_rsda_csv_converter=>create(
        i_separator = ';' ).

    field-symbols <ls_data> type any.
    loop at it_data assigning <ls_data>.

      data l_data_c(65353).
      lr_converter->structure_to_csv(
        exporting i_s_data = <ls_data>
        importing e_data   = l_data_c ).

      data l_data type string.
      l_data = l_data_c.

      insert l_data into table et_data.

    endloop.

  endmethod.


  method text2base64.

    if i_text is initial.
      return.
    endif.

    data l_text type string.
    l_text = i_text.

    data l_xtext type xstring.
    l_xtext = text2xtext( l_text ).

    constants lc_op_enc type x value 36.
    call 'SSF_ABAP_SERVICE'
      id 'OPCODE'  field lc_op_enc
      id 'BINDATA' field l_xtext
      id 'B64DATA' field e_text.

  endmethod.


  method text2itf.

    data l_text type string.
    l_text = i_text.

    data lt_stream type string_table.
    insert l_text into table lt_stream.

    call function 'CONVERT_STREAM_TO_ITF_TEXT'
      exporting
        stream_lines = lt_stream
        lf           = 'X'
      tables
        itf_text     = et_itf.

  endmethod.


  method text2xtext.

    if i_text is initial.
      return.
    endif.

    data l_text type string.
    l_text = i_text.

    data l_length type i.
    l_length = strlen( l_text ).

    data lr_conv type ref to cl_abap_conv_out_ce.
    lr_conv =
      cl_abap_conv_out_ce=>create(
        encoding    = i_encoding
        ignore_cerr = abap_true ).

    lr_conv->write(
      data = l_text
      n    = l_length ).

    e_xtext = lr_conv->get_buffer( ).

***  data lr_converter type ref to cl_abap_conv_out_ce.
***  lr_converter = cl_abap_conv_out_ce=>create( ).
***
***  lr_converter->convert(
***    exporting data   = l_text
***    importing buffer = e_xtext ).

  endmethod.


  method xml2data.

    check i_xml is not initial.

    try.
        call transformation id
          source xml i_xml
          result data = e_data.
      catch cx_root.
        zcx_generic=>raise( ).
    endtry.

  endmethod.


  method xtable2xtext.

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

  endmethod.


  method xtext2text.

    if i_xtext is initial.
      return.
    endif.

    data l_xtext type xstring.
    l_xtext = i_xtext.

    data lr_conv type ref to cl_abap_conv_in_ce.
    lr_conv =
      cl_abap_conv_in_ce=>create(
        input       = l_xtext
        encoding    = i_encoding
        ignore_cerr = abap_true ).

    lr_conv->read(
      importing
        data = e_text ).

***  data lr_converter type ref to cl_abap_conv_in_ce.
***  lr_converter = cl_abap_conv_in_ce=>create( ).
***
***  lr_converter->convert(
***    exporting input = l_xtext
***    importing data  = e_text ).

  endmethod.


  method xtext2varchar.

    data lt_data type tsfixml.
    xtext2xtable(
      exporting i_data  = i_xtext
      importing et_data = lt_data ).

    data l_data like line of lt_data.
    loop at lt_data into l_data.

      data l_string type string.
      l_string = l_data.

      concatenate e_varchar l_string into e_varchar.

    endloop.

    data l_length type i.
    l_length = xstrlen( i_xtext ) * 2.

    e_varchar = '0x' && e_varchar(l_length).

  endmethod.


  method xtext2xtable.

    call function 'SCMS_XSTRING_TO_BINARY'
      exporting
        buffer     = i_data
      tables
        binary_tab = et_data.

  endmethod.
ENDCLASS.
