class ZCL_TEXT_STATIC definition
  public
  final
  create public .

*"* public components of class ZCL_TEXT_STATIC
*"* do not include other source files here!!!
public section.

  constants XOR_BASE type X value 131 ##NO_TEXT.

  class-methods CONVERT_STR2XSTR
    importing
      !I_TEXT type SIMPLE
      !I_ENCODING type ABAP_ENCODING default 'DEFAULT'
    returning
      value(E_XTEXT) type XSTRING .
  class-methods CONVERT_XSTR2STR
    importing
      !I_XTEXT type SIMPLE
      !I_ENCODING type ABAP_ENCODING default 'DEFAULT'
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  class-methods BASE64_ENCODE
    importing
      !I_TEXT type SIMPLE
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  class-methods BASE64_DECODE
    importing
      !I_TEXT type SIMPLE
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  class-methods WORD_WRAP
    importing
      !I_STR type DATA
      !I_WIDTH type I default 40
    returning
      value(R_STR) type STRING
    raising
      ZCX_GENERIC .
  class-methods REMOVE_SYMBLOS
    importing
      !I_TEXT type SIMPLE
      !I_SYMBOLS type SIMPLE
      !I_ALLOWED type ABAP_BOOL default ABAP_TRUE
    returning
      value(E_TEXT) type STRING .
  class-methods CONVERT_ITF2STR
    importing
      !I_ITF type TLINETAB
    returning
      value(R_STR) type STRING .
  class-methods CONVERT_STR2ITF
    importing
      !I_STR type STRING
    exporting
      !E_ITF type TLINETAB .
  class-methods READ
    importing
      !I_NAME type SIMPLE
      !I_OBJECT type THEAD-TDOBJECT default 'TEXT'
      !I_ID type THEAD-TDID default 'ST'
      !I_LANGUAGE type THEAD-TDSPRAS default SY-LANGU
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  class-methods SAVE
    importing
      !I_NAME type THEAD-TDNAME
      !I_OBJECT type THEAD-TDOBJECT default 'TEXT'
      !I_ID type THEAD-TDID default 'ST'
      !I_LANGUAGE type THEAD-TDSPRAS default SY-LANGU
      !I_FORMAT type SIMPLE optional
      !I_TEXT type STRING
      !I_COMMIT type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_GENERIC .
  class-methods SHORTEN
    importing
      !I_TEXT type STRING
      !I_LENGTH type I default '40'
    returning
      value(E_TEXT) type STRING .
  class-methods SPELL
    importing
      !I_VALUE type SIMPLE
    returning
      value(E_TEXT) type STRING .
  class-methods PREPOSTFIX
    importing
      !I_TEXT type SIMPLE
      !I_PREFIX type SIMPLE optional
      !I_POSTFIX type SIMPLE optional
      !I_LENGTH type I default '40'
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  class-methods CONVERT_GUID_IN
    importing
      !I_GUID type SIMPLE
    returning
      value(E_GUID) type STRING .
  class-methods CONVERT_GUID_OUT
    importing
      !I_GUID type SIMPLE
    returning
      value(E_GUID) type STRING .
  class-methods WRITE
    importing
      !I_TEXT type SIMPLE
    returning
      value(E_TEXT) type STRING .
  class-methods UPPER_CASE
    importing
      !I_TEXT type SIMPLE
    returning
      value(E_TEXT) type STRING .
  class-methods LOWER_CASE
    importing
      !I_TEXT type SIMPLE
    returning
      value(E_TEXT) type STRING .
  class-methods DATE2DATE
    importing
      !I_DATE type SIMPLE
    returning
      value(E_DATE) type D .
  class-methods TRANSLIT
    importing
      !I_TEXT type SIMPLE
    returning
      value(E_TEXT) type STRING
    raising
      ZCX_GENERIC .
  protected section.
*"* protected components of class ZCL_TEXT_STATIC
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_TEXT_STATIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_TEXT_STATIC IMPLEMENTATION.


  method base64_decode.

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

    e_text = convert_xstr2str( l_xtext ).

  endmethod.


  method base64_encode.

    if i_text is initial.
      return.
    endif.

    data l_text type string.
    l_text = i_text.

    data l_xtext type xstring.
    l_xtext = convert_str2xstr( l_text ).

    constants lc_op_enc type x value 36.
    call 'SSF_ABAP_SERVICE'
      id 'OPCODE'  field lc_op_enc
      id 'BINDATA' field l_xtext
      id 'B64DATA' field e_text.

  endmethod.


  method convert_guid_in.

    data l_guid(36).
    l_guid = i_guid.

    translate l_guid to upper case.

    concatenate l_guid(8) l_guid+9(4) l_guid+14(4) l_guid+19(4) l_guid+24(12)
      into e_guid.

  endmethod.


  method convert_guid_out.

    data l_guid(32).
    l_guid = i_guid.

    concatenate l_guid(8) l_guid+8(4) l_guid+12(4) l_guid+16(4) l_guid+20(12)
      into e_guid separated by '-'.

    translate e_guid to lower case.

  endmethod.


  method convert_itf2str.

    data stream type string_table.
    call function 'CONVERT_ITF_TO_STREAM_TEXT'
      exporting
        lf           = 'X'
      importing
        stream_lines = stream
      tables
        itf_text     = i_itf.

    data: s type string.
    loop at stream into s.
      if sy-tabix = 1.
        r_str = s.
      else.
        concatenate r_str s into r_str separated by cl_abap_char_utilities=>newline.
      endif.
    endloop.

  endmethod.


  method convert_str2itf.

    data stream type string_table.
    append i_str to stream.

    call function 'CONVERT_STREAM_TO_ITF_TEXT'
      exporting
        stream_lines = stream
        lf           = 'X'
      tables
        itf_text     = e_itf.

  endmethod.


  method convert_str2xstr.

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


  method convert_xstr2str.

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


  method date2date.

    if i_date is initial.
      return.
    endif.

    data l_date(10).
    l_date = i_date.

    concatenate l_date+6(4) l_date+3(2) l_date(2) into e_date.

  endmethod.


  method lower_case.

    e_text = i_text.

    set locale language 'R'.
    translate e_text to lower case.
    set locale language ' '.

  endmethod.


  method prepostfix.

    data l_text(1024).
    l_text = i_text.

    data l_prefix type string.
    l_prefix = i_prefix.

    data l_postfix type string.
    l_postfix = i_postfix.

    if l_prefix is initial and l_postfix is initial.
      e_text = l_text(i_length).
      return.
    endif.

    data n1 type i.
    n1 = strlen( l_prefix ).

    data n2 type i.
    n2 = strlen( i_text ).

    data n3 type i.
    n3 = strlen( l_postfix ).

    data sum type i.
    sum = n1 + n3.

    if sum gt i_length.
      assert 1 = 2.
    endif.

    sum = n1 + n2 + n3.

    data exceed type i.
    exceed = sum - i_length.

    if exceed gt 0.
      n2 = n2 - exceed.
      l_text = l_text(n2).
    endif.

    concatenate l_prefix l_text l_postfix into e_text.

  endmethod.


  method READ.

    data l_name type thead-tdname.
    l_name = i_name.

    data lt_lines type table of tline.
    call function 'READ_TEXT'
      exporting
        object                  = i_object
        id                      = i_id
        name                    = l_name
        language                = i_language
      tables
        lines                   = lt_lines
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.

    if sy-subrc is not initial.
      zcx_generic=>raise( ).
    endif.

    e_text = convert_itf2str( lt_lines ).

  endmethod.


  method remove_symblos.

    if i_text is initial.
      return.
    endif.

    data l_symbols type string.
    l_symbols = i_symbols.
    condense l_symbols.

    data l_length type i.
    l_length = strlen( i_text ) .

    data lr_text type ref to zcl_text.
    create object lr_text.

    do.

      if sy-index gt l_length.
        exit.
      endif.

      data l_offset type i.
      l_offset = sy-index - 1.

      data l_symbol.
      l_symbol = i_text+l_offset(1).

      if l_symbol ca l_symbols.

        if i_allowed eq abap_true.
          lr_text->add( l_symbol ).
        endif.

      else.

        if i_allowed eq abap_false.
          lr_text->add( l_symbol ).
        endif.

      endif.

    enddo.

    e_text = lr_text->get( ).

  endmethod.


  method save.

    data lt_itf type tlinetab.
    convert_str2itf(
      exporting i_str = i_text
      importing e_itf = lt_itf ).

    data ls_thead type thead.
    ls_thead-tdobject = i_object.
    ls_thead-tdname   = i_name.
    ls_thead-tdid     = i_id.
    ls_thead-tdspras  = i_language.

    call function 'SAVE_TEXT'
      exporting
        header   = ls_thead
      tables
        lines    = lt_itf
      exceptions
        id       = 1
        language = 2
        name     = 3
        object   = 4
        others   = 5.
    if sy-subrc ne 0.
      zcx_generic=>raise( ).
    endif.

    call function 'COMMIT_TEXT'
      exporting
        object   = i_object
        name     = i_name
        id       = i_id
        language = i_language.

    if i_commit eq abap_true.
      zcl_abap_static=>commit( ).
    endif.

  endmethod.


  method shorten.

    data lv_text type text8192.
    lv_text = i_text.

    if i_length > 0
        and lv_text+i_length is not initial.
      data lv_length type i.
      lv_length = i_length - 3.
      lv_text+lv_length = '...'.
      e_text = lv_text.
    endif.

    e_text = lv_text.

  endmethod.


  method spell.

    data l_value type wertv9.
    l_value = i_value / 100.

    data ls_spell type spell.
    call function 'SPELL_AMOUNT'
      exporting
        amount    = l_value
      importing
        in_words  = ls_spell
      exceptions
        not_found = 1
        too_large = 2
        others    = 3.

    e_text = ls_spell-word.

  endmethod.


  method translit.

    try.

        data lr_conv_out type ref to cl_abap_conv_out_ce.
        lr_conv_out = cl_abap_conv_out_ce=>create( encoding = '1146' ).

        data l_text type xstring.
        lr_conv_out->convert(
          exporting data   = i_text
          importing buffer = l_text ).

        e_text = convert_xstr2str( l_text ).

      catch cx_root.
        zcx_generic=>raise( ).
    endtry.

  endmethod.


  method upper_case.

    e_text = i_text.

    set locale language 'R'.
    translate e_text to upper case.
    set locale language ' '.

  endmethod.


  method word_wrap.

    try.

        data: cstr type text1024.
        cstr = i_str.

        data: dummy1 type text1024.
        data: dummy2 type text1024.
        data: dummy3 type text1024.
        data: tlines type standard table of text1024.
        call function 'RSDG_WORD_WRAP'
          exporting
            textline            = cstr
            outputlen           = i_width
          importing
            out_line1           = dummy1
            out_line2           = dummy2
            out_line3           = dummy3
          tables
            out_lines           = tlines
          exceptions
            outputlen_too_large = 1
            others              = 2.
        if sy-subrc ne 0.
          zcx_generic=>raise( ).
        endif.

        field-symbols: <f> like line of tlines.
        loop at tlines assigning <f>.
          if r_str is initial.
            r_str = <f>.
          else.
            concatenate r_str <f> into r_str
              separated by cl_abap_char_utilities=>newline.
          endif.
        endloop.

      catch cx_root.

        r_str = i_str.

    endtry.

  endmethod.


  method write.

    data l_text(100).
    write i_text to l_text left-justified no-gap.

    e_text = l_text.

  endmethod.
ENDCLASS.
