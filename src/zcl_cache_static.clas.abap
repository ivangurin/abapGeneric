class zcl_cache_static definition
  public
  final
  create public .

*"* public components of class ZCL_CACHE_STATIC
*"* do not include other source files here!!!
  public section.
    type-pools abap .

    class-data enabled type abap_bool value abap_true.      "#EC NOTEXT

    class-methods free
      importing
        !i_name type simple optional
        !i_id   type simple optional
          preferred parameter i_name .
    class-methods set_object
      importing
        !i_name         type simple optional
        !i_id           type simple
        value(i_object) type ref to object .
    class-methods get_object
      importing
        !i_name         type simple optional
        !i_id           type simple
      returning
        value(e_object) type ref to object
      raising
        zcx_generic .
    class-methods set_data
      importing
        !i_name type simple optional
        !i_id   type simple optional
        !i_data type data .
    class-methods get_data
      importing
        !i_name       type string optional
        !i_id         type data optional
      exporting
        value(e_data) type data
      raising
        zcx_generic .
    class-methods set_value
      importing
        !i_name  type simple optional
        !i_id    type simple
        !i_value type simple .
    class-methods get_value
      importing
        !i_name        type simple optional
        !i_id          type simple
      returning
        value(e_value) type string
      raising
        zcx_generic .
  protected section.
*"* protected components of class ZCL_CACHE_STATIC
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_CACHE_STATIC
*"* do not include other source files here!!!

    types:
      begin of ts_cache,
        name   type        string,
        id     type        string,
        data   type ref to data,
        object type ref to object,
      end of ts_cache .
    types:
      tt_cache type hashed table of ts_cache
        with unique key
          name id .

    class-data t_cache type tt_cache .
ENDCLASS.



CLASS ZCL_CACHE_STATIC IMPLEMENTATION.


  method free.

    if i_name is supplied.

      if i_id is initial.
        delete t_cache
          where
            name eq i_name.
      else.
        delete t_cache
          where
            name eq i_name and
            id   eq i_id.
      endif.

    else.

      clear t_cache.

    endif.

  endmethod.


  method get_data.

    if enabled eq abap_false.
      zcx_generic=>raise( ).
    endif.

    field-symbols <ls_cache> like line of t_cache.
    read table t_cache assigning <ls_cache>
      with table key
        name = i_name
                     id   = i_id.
    if sy-subrc eq 0.

      field-symbols <data> type any.
      assign <ls_cache>-data->* to <data>.
      if <data> is assigned.
        e_data = <data>.
      else.
        assert 1 = 2.
      endif.

    else.
      zcx_generic=>raise( ).
    endif.

  endmethod.


  method get_object.

    if enabled eq abap_false.
      zcx_generic=>raise( ).
    endif.

    field-symbols <ls_cache> like line of t_cache.
    read table t_cache assigning <ls_cache>
      with table key
        name = i_name
        id   = i_id.
    if sy-subrc eq 0.
      e_object = <ls_cache>-object.
    else.
      zcx_generic=>raise( ).
    endif.

  endmethod.


  method get_value.

    get_data(
      exporting
        i_name = i_name
        i_id   = i_id
      importing
        e_data = e_value ).

  endmethod.


  method set_data.

    if enabled eq abap_false.
      return.
    endif.

    delete t_cache
      where
        name eq i_name and
        id   eq i_id.

    field-symbols <i_data> type any.
    assign i_data to <i_data>.

    data ls_cache like line of t_cache.
    ls_cache-name = i_name.
    ls_cache-id   = i_id.

    create data ls_cache-data like <i_data>.

    field-symbols <l_data> type any.
    assign ls_cache-data->* to <l_data>.

    <l_data> = <i_data>.

    insert ls_cache into table t_cache.

  endmethod.


  method set_object.

    if enabled eq abap_false.
      return.
    endif.

    delete t_cache
      where
        name eq i_name and
        id   eq i_id.

    data ls_cache like line of t_cache.
    ls_cache-name   = i_name.
    ls_cache-id     = i_id.
    ls_cache-object = i_object.
    insert ls_cache into table t_cache.

  endmethod.


  method set_value.

    set_data(
      i_name = i_name
      i_id   = i_id
      i_data = i_value ).

  endmethod.
ENDCLASS.
