class ZCL_ABAP_STATIC definition
  public
  final
  create public .

*"* public components of class ZCL_ABAP_STATIC
*"* do not include other source files here!!!
public section.

  types:
    begin of ts_field,
        name  type string,
        type  type string,
        value type string,
      end of ts_field .
  types:
    tt_fields type table of ts_field .

  class-data YES type CHAR1 value 'Y' ##NO_TEXT.                      " .
  class-data NO type CHAR1 value 'N' ##NO_TEXT.                       " .
  constants TRUE type TEXT10 value 'True' ##NO_TEXT.
  constants FALSE type TEXT10 value 'False' ##NO_TEXT.

  class-methods CREATE_GUID
    returning
      value(R_GUID) type GUID .
  class-methods VALUE2RANGE
    importing
      !I_SIGN type SIMPLE default 'I'
      !I_OPTION type SIMPLE default 'EQ'
      !I_VALUE type SIMPLE
      !I_TO type SIMPLE optional
      !I_STRICT type ABAP_BOOL default ABAP_FALSE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods VALUE2RANGE_E
    importing
      !I_VALUE type SIMPLE
      !I_TO type SIMPLE optional
      !I_STRICT type ABAP_BOOL default ABAP_FALSE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods VALUE2RANGE_LT
    importing
      !I_VALUE type SIMPLE
      !I_TO type SIMPLE optional
      !I_STRICT type ABAP_BOOL default ABAP_FALSE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods VALUE2RANGE_LE
    importing
      !I_VALUE type SIMPLE
      !I_TO type SIMPLE optional
      !I_STRICT type ABAP_BOOL default ABAP_FALSE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods VALUE2RANGE_GT
    importing
      !I_VALUE type SIMPLE
      !I_TO type SIMPLE optional
      !I_STRICT type ABAP_BOOL default ABAP_FALSE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods VALUE2RANGE_GE
    importing
      !I_VALUE type SIMPLE
      !I_TO type SIMPLE optional
      !I_STRICT type ABAP_BOOL default ABAP_FALSE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods VALUE2RANGE_NE
    importing
      !I_VALUE type SIMPLE
      !I_TO type SIMPLE optional
      !I_STRICT type ABAP_BOOL default ABAP_FALSE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods VALUE2LIST
    importing
      !I_VALUE type SIMPLE
    exporting
      !ET_LIST type TABLE .
  class-methods VALUE2TEXT
    importing
      !I_VALUE type SIMPLE
    returning
      value(E_TEXT) type STRING .
  class-methods VALUE2TABLE
    importing
      !I_VALUE type SIMPLE
      !I_FIELD type SIMPLE
    changing
      !CT_TABLE type ANY TABLE .
  class-methods VALUE2INPUT
    importing
      !I_VALUE type SIMPLE
    returning
      value(E_VALUE) type STRING .
  class-methods VALUE2OUTPUT
    importing
      !I_VALUE type SIMPLE
    returning
      value(E_VALUE) type STRING .
  class-methods VALUES2RANGE
    importing
      !IT_VALUES type ZIVALUES
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods LIST2RANGE
    importing
      !IT_LIST type TABLE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods LIST2LIST
    importing
      !IT_DATA type TABLE
    exporting
      !ET_DATA type TABLE .
  class-methods LIST2TEXT
    importing
      !IT_LIST type TABLE
      !I_SEPARATOR type SIMPLE default ','
    returning
      value(E_TEXT) type STRING .
  class-methods RANGE2RANGE
    importing
      !IT_RANGE type TABLE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods RANGE2UPPERCASE
    importing
      !IT_RANGE type ZIRANGE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods RANGE2LIST
    importing
      !IT_RANGE type TABLE
    returning
      value(ET_LIST) type STRINGTAB .
  class-methods TABLE2TABLE
    importing
      !IT_DATA type ANY TABLE
    exporting
      !ET_DATA type TABLE .
  class-methods TABLE2LIST
    importing
      !IT_DATA type ANY TABLE
      !I_FIELD type SIMPLE
    exporting
      !ET_LIST type TABLE .
  class-methods TABLE2STRINGLIST
    importing
      !IT_DATA type ANY TABLE
      !I_FIELD type SIMPLE
    returning
      value(ET_LIST) type STRINGTAB .
  class-methods TABLE2RANGE
    importing
      !IT_DATA type TABLE
      !I_FIELD type SIMPLE
    returning
      value(ET_RANGE) type ZIRANGE .
  class-methods TABLE2GUIDS
    importing
      !IT_DATA type ANY TABLE
      !I_FIELD type SIMPLE default 'GUID'
    returning
      value(ET_GUIDS) type ZIGUIDS .
  class-methods TABLE2TEXT
    importing
      !IT_DATA type ANY TABLE
      !I_FIELD type SIMPLE
    returning
      value(E_TEXT) type STRING .
  class-methods BOOL2INT
    importing
      !I_BOOL type SIMPLE
    returning
      value(E_INT) type I .
  class-methods BOOL2YESNO
    importing
      !I_BOOL type SIMPLE
    returning
      value(E_YESNO) type STRING .
  class-methods COMMON_LIST
    importing
      !IT_LIST type TABLE
    changing
      !CT_LIST type TABLE .
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
  class-methods WRITE
    importing
      !I_VALUE type SIMPLE
    returning
      value(E_TEXT) type STRING .
  class-methods CONDENSE
    importing
      !I_VALUE type SIMPLE
    returning
      value(E_VALUE) type STRING .
  class-methods INVERSE
    importing
      !I_BOOL type SIMPLE
    returning
      value(E_BOOL) type ABAP_BOOL .
  class-methods MOVE_CORRESPONDING
    importing
      !I_DATA type DATA
    exporting
      !E_DATA type DATA .
  class-methods IF
    importing
      !I_COND type SIMPLE
    returning
      value(E_BOOL) type ABAP_BOOL
    raising
      ZCX_GENERIC .
  class-methods CALC
    importing
      !I_EXPRESSION type SIMPLE
    returning
      value(E_RESULT) type STRING
    raising
      ZCX_GENERIC .
  class-methods TEXT_MD5
    importing
      !I_DATA type SIMPLE
    returning
      value(E_HASH) type MD5_FIELDS-HASH
    raising
      ZCX_GENERIC .
  class-methods BINARY_MD5
    importing
      !I_DATA type XSTRING
    returning
      value(E_HASH) type STRING
    raising
      ZCX_GENERIC .
  class-methods CREATE_STRUCTURE
    importing
      !IT_FIELDS type TT_FIELDS
    returning
      value(E_DATA) type ref to DATA .
  class-methods CREATE_TABLE
    importing
      !IT_FIELDS type TT_FIELDS
    returning
      value(E_DATA) type ref to DATA .
  class-methods COMMIT
    importing
      !I_WAIT type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_GENERIC .
  class-methods FLUSH
    raising
      ZCX_GENERIC .
  class-methods ROLLBACK .
  class-methods GET_TRANSACTION_ENQUEUE_OWNER
    returning
      value(E_OWNER) type EQEUSR .
  class-methods GET_UPDATE_TASK_ENQUEUE_OWNER
    returning
      value(E_OWNER) type EQEUSR .
  class-methods GET_LOGSYS
    returning
      value(E_LOGSYS) type STRING .
  protected section.
*"* protected components of class ZCL_ABAP_STATIC
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_ABAP_STATIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_ABAP_STATIC IMPLEMENTATION.


  method binary_md5.

    check i_data is not initial.

    data lt_data type solix_tab.
    zcl_binary_static=>value2table(
      exporting i_data  = i_data
      importing et_data = lt_data ).

    data l_length type i.
    l_length = xstrlen( i_data ).

    data l_hash type md5_fields-hash.
    call function 'MD5_CALCULATE_HASH_FOR_RAW'
      exporting
        length         = l_length
      importing
        hash           = l_hash
      tables
        data_tab       = lt_data
      exceptions
        internal_error = 1
        others         = 2.
    if sy-subrc ne 0.
      zcx_generic=>raise( ).
    endif.

    e_hash = l_hash.

  endmethod.


  method bool2int.

    check i_bool eq abap_true.

    e_int = 1.

  endmethod.


  method bool2yesno.

    if i_bool eq abap_true.
      e_yesno = yes.
    elseif i_bool eq abap_false.
      e_yesno = no.
    endif.

  endmethod.


  method calc.

    try.

        data l_expression type string.
        l_expression = i_expression.

        find 'SY-DATUM' in i_expression.
        if sy-subrc eq 0.

          data l_days type i.
          l_days = sy-datum.

          replace all occurrences of 'SY-DATUM' in l_expression with value2text( l_days ).

        endif.

        data lr_formula type ref to cl_fobu_formula.
        cl_fobu_formula=>create(
          importing
            ex_formula = lr_formula ).

        lr_formula->parse( l_expression ).

        data lr_runtime type ref to cl_foev_formula.
        lr_runtime =
          cl_foev_formula=>load_from_fobu(
            im_formula = lr_formula ).

        data lr_result type ref to data.
        lr_result = lr_runtime->evaluate( ).

        field-symbols <l_result> type any.
        assign lr_result->* to <l_result>.

        e_result = <l_result>.

        find 'SY-DATUM' in i_expression.
        if sy-subrc eq 0.

          l_days = e_result.

          data l_date type d.
          l_date = l_days.

          e_result = l_date.

        endif.

        data lx_root type ref to cx_root.
      catch cx_root into lx_root.

        zcx_generic=>raise(
          i_text = `Error on calc ` && i_expression ).

    endtry.

  endmethod.


  method COMMIT.

    data ls_return type bapiret2.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait   = i_wait
      importing
        return = ls_return.

    if ls_return-type ca 'EAX'.
      zcx_generic=>raise(
        is_return = ls_return ).
    endif.

  endmethod.


  method common_list.

    if it_list is initial.

      clear ct_list.

    else.

      field-symbols <l_value> type any.
      loop at ct_list assigning <l_value>.

        data l_value type string.
        l_value = <l_value>.

        read table it_list transporting no fields
          with key
            table_line = l_value.
        if sy-subrc ne 0.
          delete ct_list.
        endif.

      endloop.

    endif.

  endmethod.


  method condense.

    e_value = i_value.

    condense e_value.

  endmethod.


  method create_guid.

    call function 'SYSTEM_UUID_CREATE'
      importing
        uuid = r_guid.

  endmethod.


  method CREATE_STRUCTURE.

    data ls_field like line of it_fields.
    loop at it_fields into ls_field.

      data lr_element type ref to cl_abap_elemdescr.
      lr_element ?= cl_abap_elemdescr=>describe_by_name( ls_field-type ).

      data lt_components type cl_abap_structdescr=>component_table.
      data ls_component like line of lt_components.
      ls_component-name = ls_field-name.
      ls_component-type = lr_element.
      insert ls_component into table lt_components.

    endloop.

    data lr_structure type ref to cl_abap_structdescr.
    lr_structure = cl_abap_structdescr=>create( lt_components ).

    create data e_data type handle lr_structure.

  endmethod.


  method CREATE_TABLE.

    data lr_data type ref to data.
    lr_data = create_structure( it_fields ).

    data lr_structure type ref to cl_abap_structdescr.
    lr_structure ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).

    data lr_table type ref to cl_abap_tabledescr.
    lr_table = cl_abap_tabledescr=>create( lr_structure ).

    create data e_data type handle lr_table.

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


  method flush.

    cl_gui_cfw=>flush(
      exceptions
        cntl_system_error = 1
        cntl_error        = 2
        others            = 3 ).
    if sy-subrc ne 0.
      zcx_generic=>raise( ).
    endif.

  endmethod.


  method get_logsys.

    select single logsys
      from t000
      into e_logsys
      where mandt eq sy-mandt.

  endmethod.


  method get_transaction_enqueue_owner.

    call 'C_ENQUEUE'
      id 'OPCODE' field '7'
      id 'ENQKEY' field e_owner.

  endmethod.


  method get_update_task_enqueue_owner.

    call 'C_ENQUEUE'
      id 'OPCODE' field '7'
      id 'ENQKVB' field e_owner  .

  endmethod.


  method if.

    try.

        data l_cond type string.
        l_cond = i_cond.

        data lr_formula type ref to cl_fobu_formula.
        cl_fobu_formula=>create(
          exporting
            im_desired_type = 'BOOLEAN'
          importing
            ex_formula      = lr_formula ).

        lr_formula->parse( l_cond ).

        data lr_runtime type ref to cl_foev_formula.
        lr_runtime =
          cl_foev_formula=>load_from_fobu(
            im_formula = lr_formula ).

        data lr_result type ref to data.
        lr_result = lr_runtime->evaluate( ).

        field-symbols <l_result> type any.
        assign lr_result->* to <l_result>.

        e_bool = <l_result>.

        data lx_root type ref to cx_root.
      catch cx_root into lx_root.

        zcx_generic=>raise(
          i_text = `Error on check conditon ` && i_cond ).

    endtry.


  endmethod.


  method inverse.

    if i_bool is initial.
      e_bool = abap_true.
    endif.

  endmethod.


  method JSON2DATA.

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


  method list2list.

    field-symbols <i_data> type any.
    loop at it_data assigning <i_data>.

      field-symbols <e_data> type any.
      append initial line to et_data assigning <e_data>.

      <e_data> = <i_data>.

    endloop.

  endmethod.


  method list2range.

    field-symbols <l_value> type any.
    loop at it_list assigning <l_value>.

      data lt_range like et_range.
      lt_range = value2range( <l_value> ).

      insert lines of lt_range into table et_range.

    endloop.

  endmethod.


  method list2text.

    field-symbols <ls_list> type any.
    loop at it_list assigning <ls_list>.

      data l_text type string.
      l_text = <ls_list>.

      if e_text is initial.
        e_text = l_text.
      else.
        concatenate e_text i_separator into e_text.
        concatenate e_text l_text into e_text separated by space.
      endif.

    endloop.

  endmethod.


  method move_corresponding.

    data lr_type type ref to cl_abap_typedescr.
    lr_type = cl_abap_typedescr=>describe_by_data( i_data ).

    case lr_type->kind.
      when cl_abap_typedescr=>kind_table.

        field-symbols <lt_from> type any table.
        assign i_data to <lt_from>.
        check sy-subrc eq 0.

        check <lt_from> is not initial.

        field-symbols <lt_to> type standard table.
        assign e_data to <lt_to>.
        check sy-subrc eq 0.

        field-symbols <ls_from> type any.
        loop at <lt_from> assigning <ls_from>.

          field-symbols <ls_to> type any.
          append initial line to <lt_to> assigning <ls_to>.

          move_corresponding(
            exporting
              i_data = <ls_from>
            importing
              e_data = <ls_to> ).

        endloop.

      when cl_abap_typedescr=>kind_struct.

        data lr_strucure type ref to cl_abap_structdescr.
        lr_strucure ?= cl_abap_structdescr=>describe_by_data( i_data ).

        data ls_component like line of lr_strucure->components.
        loop at lr_strucure->components into ls_component.

          field-symbols <l_from> type any.
          unassign <l_from>.
          assign component ls_component-name of structure i_data to <l_from>.
          check <l_from> is assigned.

          check <l_from> is not initial.

          field-symbols <l_to> type any.
          unassign <l_to>.
          assign component ls_component-name of structure e_data to <l_to>.
          check <l_to> is assigned.

          move_corresponding(
            exporting
              i_data = <l_from>
            importing
              e_data = <l_to> ).

        endloop.

      when others.
        e_data = i_data.

    endcase.

  endmethod.


  method RANGE2LIST.
  endmethod.


  method range2range.

    field-symbols <is_range> type any.
    loop at it_range assigning <is_range>.

      field-symbols <es_range> like line of et_range.
      append initial line to et_range assigning <es_range>.

      move-corresponding <is_range> to <es_range>.

    endloop.

  endmethod.


  method RANGE2UPPERCASE.

    data ls_range like line of it_range.
    loop at it_range into ls_range.
      ls_range-low  = zcl_text_static=>upper_case( ls_range-low ).
      ls_range-high = zcl_text_static=>upper_case( ls_range-high ).
      insert ls_range into table et_range.
    endloop.

  endmethod.


  method ROLLBACK.

    call function 'BAPI_TRANSACTION_ROLLBACK'.

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


  method table2guids.

    table2list(
      exporting
        it_data = it_data
        i_field = i_field
      importing
        et_list = et_guids ).

  endmethod.


  method TABLE2LIST.

    field-symbols <is_data> type any.
    loop at it_data assigning <is_data>.

      field-symbols <l_value> type any.
      assign component i_field of structure <is_data> to <l_value>.

      field-symbols <ls_list> type any.
      append initial line to et_list assigning <ls_list>.

      <ls_list> = <l_value>.

    endloop.

  endmethod.


  method table2range.

    field-symbols <is_data> type any.
    loop at it_data assigning <is_data>.

      field-symbols <l_value> type any.
      assign component i_field of structure <is_data> to <l_value>.
      check sy-subrc eq 0.

      insert lines of value2range( <l_value> ) into table et_range.

    endloop.

  endmethod.


  method table2stringlist.

    table2list(
      exporting
        it_data = it_data
        i_field = i_field
      importing
        et_list = et_list ).

  endmethod.


  method table2table.

    field-symbols <is_data> type any.
    loop at it_data assigning <is_data>.

      field-symbols <es_data> type any.
      append initial line to et_data assigning <es_data>.

      move-corresponding <is_data> to <es_data>.

    endloop.

  endmethod.


  method table2text.

    field-symbols <ls_data> type any.
    loop at it_data assigning <ls_data>.

      if i_field is initial.

        e_text = e_text && value2text( <ls_data> ).

      else.

        field-symbols <l_value> type any.
        assign component i_field of structure <ls_data> to <l_value>.
        check sy-subrc eq 0.

        e_text = e_text && value2text( <l_value> ).

      endif.

    endloop.

  endmethod.


  method text_md5.

    call function 'MD5_CALCULATE_HASH_FOR_CHAR'
      exporting
        data           = i_data
      importing
        hash           = e_hash
      exceptions
        no_data        = 1
        internal_error = 2
        others         = 3.
    if sy-subrc ne 0.
      zcx_generic=>raise( ).
    endif.

  endmethod.


  method value2input.

    try.

        data lr_value type ref to data.
        create data lr_value like i_value.

        field-symbols <l_value> type any.
        assign lr_value->* to <l_value>.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = i_value
          importing
            output = <l_value>.

        e_value = <l_value>.

      catch cx_root.

        e_value = i_value.
        condense e_value.

    endtry.

  endmethod.


  method value2list.

    field-symbols <l_value> type any.
    append initial line to et_list assigning <l_value>.

    <l_value> = i_value.

  endmethod.


  method value2output.

    try.

        data l_value(100).
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = i_value
          importing
            output = l_value.

      catch cx_root.

        write i_value to l_value left-justified.

    endtry.

    e_value = l_value.

    condense e_value.

  endmethod.


  method value2range.

    field-symbols <ls_range> like line of et_range.
    append initial line to et_range assigning <ls_range>.

    <ls_range>-sign   = i_sign.
    <ls_range>-option = i_option.
    <ls_range>-low    = i_value.

  if i_to is initial.

    check i_strict eq abap_false.

    find '*' in <ls_range>-low.
    if sy-subrc eq 0.
      <ls_range>-option = 'CP'.
    endif.

  else.

    <ls_range>-option = 'BT'.
    <ls_range>-high   = i_to.

  endif.

  endmethod.


method value2range_e.

  et_range =
    value2range(
      i_value  = i_value
      i_to     = i_to
      i_sign   = 'E'
      i_strict = i_strict ).

endmethod.


method value2range_ge.

  et_range =
    value2range(
      i_value  = i_value
      i_to     = i_to
      i_sign   = 'I'
      i_option = 'GE'
      i_strict = i_strict ).

endmethod.


method value2range_gt.

  et_range =
    value2range(
      i_value  = i_value
      i_to     = i_to
      i_sign   = 'I'
      i_option = 'GT'
      i_strict = i_strict ).

endmethod.


method value2range_le.

  et_range =
    value2range(
      i_value  = i_value
      i_to     = i_to
      i_sign   = 'I'
      i_option = 'LE'
      i_strict = i_strict ).

endmethod.


method value2range_lt.

  et_range =
    value2range(
      i_value  = i_value
      i_to     = i_to
      i_sign   = 'I'
      i_option = 'LT'
      i_strict = i_strict ).

endmethod.


method value2range_ne.

  et_range =
    value2range(
      i_value  = i_value
      i_to     = i_to
      i_sign   = 'I'
      i_option = 'NE'
      i_strict = i_strict ).

endmethod.


  method value2table.

    check i_field is not initial.

    field-symbols <ls> type any.
    loop at ct_table assigning <ls>.

      field-symbols <l_value> type any.
      assign component i_field of structure <ls> to <l_value>.
      check sy-subrc eq 0.

      <l_value> = i_value.

    endloop.

  endmethod.


  method value2text.

    e_text = i_value.

    condense e_text.

  endmethod.


  method values2range.

    et_range =
      table2range(
        it_data = it_values
        i_field = 'ID' ).

  endmethod.


  method write.

    data l_text(100).
    write i_value to l_text left-justified no-gap.

    e_text = l_text.

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
ENDCLASS.
