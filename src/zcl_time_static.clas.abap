class ZCL_TIME_STATIC definition
  public
  final
  create public .

*"* public components of class ZCL_TIME_STATIC
*"* do not include other source files here!!!
public section.

  types:
    begin of ts_period,
        id   type char8,
        from type d,
        to   type d,
        text type string,
      end of ts_period .
  types:
    tt_periods type table of ts_period .

  constants YEAR type CHAR1 value 'Y' ##NO_TEXT.
  constants QUARTER type CHAR1 value 'Q' ##NO_TEXT.
  constants MONTH type CHAR1 value 'M' ##NO_TEXT.
  constants WEEK type CHAR1 value 'W' ##NO_TEXT.
  constants DAY type CHAR1 value 'D' ##NO_TEXT.
  constants RANGE type CHAR1 value 'R' ##NO_TEXT.
  constants LAST_DATE type DATUM value '99991231' ##NO_TEXT.
  constants INIT_TIME type T value '000000' ##NO_TEXT.
  constants NOON_TIME type T value '120000' ##NO_TEXT.
  constants WORK_FROM_TIME type T value '090000' ##NO_TEXT.
  constants WORK_TO_TIME type T value '180000' ##NO_TEXT.
  constants LAST_TIME type TIMS value '235959' ##NO_TEXT.
  constants LAST_TIMESTAMP type TIMESTAMP value '99991231195959' ##NO_TEXT.
  constants TZ_MSK type TZNZONE value 'RUS03' ##NO_TEXT.
  constants TZ_UTC type TZNZONE value 'UTC' ##NO_TEXT.

  class-methods GET_TIMESTAMP
    importing
      !I_DATE type SIMPLE default SY-DATLO
      !I_TIME type UZEIT default SY-TIMLO
      !I_ZONE type TZNZONE default SY-ZONLO
    returning
      value(E_TIMESTAMP) type TIMESTAMP .
  class-methods GET_TIMESTAMP_TEXT
    importing
      !I_TIMESTAMP type TIMESTAMP
      !I_ZONE type SIMPLE
      !I_ZONE_X type ABAP_BOOL default ABAP_FALSE
    returning
      value(E_TEXT) type STRING .
  class-methods GET_DATE_AND_TIME
    importing
      !I_TIMESTAMP type TIMESTAMP
      !I_ZONE type TZNZONE default SY-ZONLO
    exporting
      !E_DATE type DATUM
      !E_TIME type UZEIT .
  class-methods GET_DATE
    importing
      !I_TIMESTAMP type TIMESTAMP
      !I_ZONE type TZNZONE default SY-ZONLO
    returning
      value(E_DATE) type DATUM .
  class-methods GET_TIME
    importing
      !I_TIMESTAMP type TIMESTAMP
      !I_ZONE type TZNZONE default SY-ZONLO
    returning
      value(E_TIME) type UZEIT .
  class-methods ROUND_PERIOD
    importing
      !I_PERIOD type DATA
      !I_FROM type CHAR1
      !I_TO type CHAR1
    returning
      value(E_PERIOD) type CHAR8 .
  class-methods GET_PERIOD_LIST
    importing
      !I_TYPE type CHAR1
      !I_FROM type DATUM
      !I_TO type DATUM
      !I_SIMPLE_NAMES type ABAP_BOOL default ABAP_TRUE
    exporting
      value(ET_LIST) type TT_PERIODS
    raising
      ZCX_GENERIC .
  class-methods GET_PERIOD_TEXT
    importing
      !I_TYPE type CHAR1
      !I_PERIOD type DATA
      !I_SIMPLE type ABAP_BOOL default ABAP_FALSE
    returning
      value(E_TEXT) type STRING .
  class-methods GET_PERIOD_RANGE
    importing
      !I_TYPE type CHAR1
      !I_PERIOD type DATA
    returning
      value(ET_RANGE) type ZIRANGE
    raising
      ZCX_GENERIC .
  class-methods GET_PERIOD_TIMESTAMP_RANGE
    importing
      !I_TYPE type CHAR1
      !I_PERIOD type DATA
    returning
      value(ET_RANGE) type ZIRANGE
    raising
      ZCX_GENERIC .
  class-methods GET_PERIOD_VALUES
    importing
      !I_TYPE type CHAR1
      !I_FROM type DATUM
      !I_TO type DATUM
      !I_SIMPLE_NAMES type ABAP_BOOL default ABAP_TRUE
    returning
      value(ET_VALUES) type ZIVALUES
    raising
      ZCX_GENERIC .
  class-methods GET_PERIOD_FROM_TO_TEXT
    importing
      !I_FROM type SIMPLE
      !I_TO type SIMPLE
    returning
      value(E_TEXT) type STRING .
  class-methods GET_MONTH_TEXT
    importing
      !I_MONTH type DATA
    returning
      value(E_TEXT) type STRING .
  class-methods GET_MONTH_DAYS
    importing
      !I_DATE type DATA
    returning
      value(E_DAYS) type CHAR2 .
  class-methods GET_MONTH_RANGE
    importing
      !I_PERIOD type DATA
    returning
      value(ET_RANGE) type ZIRANGE
    raising
      ZCX_GENERIC .
  class-methods GET_MONTH_LIST
    returning
      value(ET_MONTHES) type ZIVALUES .
  class-methods GET_MONTH_LAST_DATE
    importing
      !I_DATE type SIMPLE
    returning
      value(E_DATE) type DATUM .
  class-methods IS_END_OF_MONTH
    importing
      !I_DATE type SIMPLE
    returning
      value(E_IS) type ABAP_BOOL .
  class-methods GET_QUARTER
    importing
      !I_PERIOD type DATA
    returning
      value(E_QUARTER) type CHAR6 .
  class-methods GET_QUARTER_TEXT
    importing
      !I_QUARTER type SIMPLE
      !I_FULL type ABAP_BOOL default ABAP_TRUE
    returning
      value(E_TEXT) type STRING .
  class-methods GET_QUARTER_DATE
    importing
      !I_YEAR type SIMPLE
      !I_MONTH type NUM2 optional
      !I_QUARTER type NUM1
    returning
      value(E_DATE) type DATUM .
  class-methods GET_YEAR_LIST
    importing
      !I_YEARS type I default '5'
    preferred parameter I_YEARS
    returning
      value(ET_YEARS) type ZIVALUES .
  class-methods GET_WEEK_FIRST_DAY
    importing
      !I_WEEK type DATA
    returning
      value(E_DATE) type DATUM
    raising
      ZCX_GENERIC .
  class-methods GET_WEEK_LAST_DAY
    importing
      !I_WEEK type DATA
    returning
      value(E_DATE) type DATUM
    raising
      ZCX_GENERIC .
  class-methods GET_WEEK_FOR_DATE
    importing
      !I_DATE type DATUM
    returning
      value(E_WEEK) type CHAR8
    raising
      ZCX_GENERIC .
  class-methods HAS_TIME_COME
    importing
      !I_TIME type TIMESTAMP
      !I_NOW type TIMESTAMP optional
    returning
      value(E_HAS) type ABAP_BOOL .
  class-methods GET_SECONDS
    importing
      !I_SECONDS type I optional
      !I_MINUTES type I optional
      !I_HOURS type I optional
      !I_DAYS type I optional
    returning
      value(E_SECONDS) type I .
  class-methods ADD
    importing
      !I_TIME type TIMESTAMP optional
      !I_SECONDS type I optional
      !I_MINUTES type I optional
      !I_HOURS type I optional
      !I_DAYS type I optional
    returning
      value(E_TIME) type TIMESTAMP .
  class-methods SUBTRACT
    importing
      !I_TIME type TIMESTAMP optional
      !I_SECONDS type I optional
      !I_MINUTES type I optional
      !I_HOURS type I optional
      !I_DAYS type I optional
    returning
      value(E_TIME) type TIMESTAMP .
  class-methods DATE_ADD
    importing
      !I_DATE type D
      !I_DAYS type I optional
      !I_MONTHES type I optional
      !I_YEARS type I optional
    returning
      value(E_DATE) type D .
  class-methods GET_WORK_DAYS
    importing
      !I_FROM type D
      !I_TO type D
    returning
      value(E_DAYS) type INT4 .
  class-methods IS_WORK_DAY
    importing
      !I_DATE type SIMPLE
    returning
      value(E_IS) type ABAP_BOOL .
  class-methods GET_NEXT_WORK_DAY
    importing
      !I_DATE type D
    returning
      value(E_DATE) type D .
  class-methods ADD_WORK_DAYS
    importing
      !I_DATE type D
      !I_DAYS type I
    returning
      value(E_DATE) type D .
  class-methods SUBTRACT_WORK_DAYS
    importing
      !I_DATE type D
      !I_DAYS type I
    returning
      value(E_DATE) type D .
  class-methods IS_WORK_HOUR
    importing
      !I_TIME type T
    returning
      value(E_IS) type ABAP_BOOL .
  class-methods GET_NEXT_WORK_HOUR
    importing
      !I_TIMESTAMP type TIMESTAMP optional
      !I_ZONE type SIMPLE
    returning
      value(E_TIMESTAMP) type TIMESTAMP .
  class-methods CONVERT_TIMESTAMP
    importing
      !I_TIMESTAMP type TIMESTAMP
      !I_ZONE type SYSTZONLO default SY-ZONLO
    returning
      value(E_TIMESTAMP) type TIMESTAMP .
  class-methods CONVERT_RANGE_DATE2TIME
    importing
      !IT_RANGE type TABLE
    returning
      value(ET_RANGE) type ZIRANGE .
  protected section.
*"* protected components of class ZCL_TIME_STATIC
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_TIME_STATIC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_TIME_STATIC IMPLEMENTATION.


  method add.

    if i_time is initial.
      data l_time like i_time.
      l_time = get_timestamp( ).
    else.
      l_time = i_time.
    endif.

    data l_seconds type i.
    l_seconds =
      get_seconds(
        i_seconds = i_seconds
        i_minutes = i_minutes
        i_hours   = i_hours
        i_days    = i_days ).

    e_time =
      cl_abap_tstmp=>add(
        tstmp = l_time
        secs  = l_seconds ).

  endmethod.


method add_work_days.

  e_date = i_date.

  do 999 times.

    data l_days type i.
    if l_days ge i_days.
      return.
    endif.

    add 1 to e_date.

    if is_work_day( e_date ) eq abap_true.
      add 1 to l_days.
    endif.

  enddo.

  assert 1 = 2.

endmethod.


  method convert_range_date2time.

    et_range = zcl_abap_static=>range2range( it_range ).

    field-symbols <ls_range> like line of et_range.
    loop at et_range assigning <ls_range>.

      if <ls_range>-option eq 'EQ' or
         <ls_range>-option eq 'NE'.

        <ls_range>-option = 'BT'.

        if <ls_range>-option eq 'NE'.
          <ls_range>-sign = 'E'.
        endif.

        data l_date type d.
        l_date = <ls_range>-low.

        <ls_range>-low =
          get_timestamp(
            i_date = l_date
            i_time = init_time ).

        <ls_range>-high =
          get_timestamp(
            i_date = l_date
            i_time = last_time ).

      else.

        if <ls_range>-low is not initial.
          <ls_range>-low = get_timestamp( i_date = <ls_range>-low ).
        endif.

        if <ls_range>-high is not initial.
          <ls_range>-high = get_timestamp( i_date = <ls_range>-high ).
        endif.

      endif.

      condense <ls_range>-low.
      condense <ls_range>-high.

    endloop.

  endmethod.


  method CONVERT_TIMESTAMP.

    data l_date type d.
    data l_time type t.

    convert time stamp i_timestamp time zone i_zone
      into date l_date time l_time.

    convert date l_date time l_time into time stamp e_timestamp
      time zone tz_utc.

  endmethod.


method date_add.

  check i_date is not initial.

  e_date = i_date.

  if i_days is not initial.

    add i_days to e_date.

  elseif i_monthes is not initial.

    data l_years type i.
    l_years = i_monthes div 12.

    if l_years gt 0.
      add l_years to e_date(4).
    endif.

    data l_monthes type i.
    l_monthes = i_monthes mod 12.

    add l_monthes to e_date+4(2).

    if e_date+4(2) gt 12.
      subtract 12 from e_date+4(2).
      add 1 to e_date(4).
    endif.

    do 100 times.

      if cl_rs_time_service=>is_valid_date( e_date ) eq abap_true.
        exit.
      endif.

      subtract 1 from e_date+6(2).

    enddo.

  elseif i_years is not initial.

    add i_years to e_date(4).

  endif.

endmethod.


  method GET_DATE.

    convert time stamp i_timestamp time zone i_zone into date e_date.

  endmethod.


  method GET_DATE_AND_TIME.
  endmethod.


  method GET_MONTH_DAYS.

    data l_date type d.
    l_date = i_date.
    l_date+6 = '01'.

    call function 'END_OF_MONTH_DETERMINE_2'
      exporting
        i_datum = l_date
      importing
        e_tt    = e_days.

  endmethod.


  method GET_MONTH_LAST_DATE.

    data l_date type d.
    l_date   = i_date.
    l_date+6 = get_month_days( i_date ).

    e_date = l_date.

  endmethod.


  method get_month_list.

    data lt_t247 type table of t247.
    select * from t247 into table lt_t247
      where spras eq sy-langu.

    data ls_t247 like line of lt_t247.
    loop at lt_t247 into ls_t247.

      data ls_month like line of et_monthes.
      ls_month-id    = ls_t247-mnr.
      ls_month-text  = ls_t247-ltx.
      insert ls_month into table et_monthes.

    endloop.

  endmethod.


  method get_month_range.

    et_range = get_period_range(
      i_type   = month
      i_period = i_period ).

  endmethod.


  method GET_MONTH_TEXT.

    try.
        zcl_cache_static=>get_data(
          exporting i_name = 'ZCLSRM_TIME_STATIC=>GET_MONTH_TEXT'
                    i_id   = i_month
          importing e_data = e_text ).
        return.
      catch cx_root.
    endtry.

    select single ltx from t247 into e_text
     where mnr eq i_month and spras eq sy-langu.

    zcl_cache_static=>set_data(
      i_name = 'ZCLSRM_TIME_STATIC=>GET_MONTH_TEXT'
      i_id   = i_month
      i_data = e_text ).

  endmethod.


method get_next_work_day.

  e_date = i_date.

  do 999 times.

    add 1 to e_date.

    if is_work_day( e_date ) eq abap_true.
      return.
    endif.

  enddo.

  assert 1 = 2.

endmethod.


method get_next_work_hour.

  data l_zone type tznzone.
  l_zone = i_zone.

  if i_timestamp is initial.
    data l_timestamp like i_timestamp.
    l_timestamp = get_timestamp( ).
  else.
    l_timestamp = i_timestamp.
  endif.

  data l_date type d.
  data l_time type t.
  convert time stamp l_timestamp
    time zone l_zone
    into date l_date time l_time.

  l_time+2 = '0000'.

  convert date l_date time l_time
    into time stamp l_timestamp
    time zone l_zone.

  do 100500 times.

    l_timestamp =
      add(
        i_time  = l_timestamp
        i_hours = 1 ).

    convert time stamp l_timestamp
      time zone l_zone
      into date l_date time l_time.

    if is_work_day( l_date ) eq abap_true and
       is_work_hour( l_time ) eq abap_true.
      e_timestamp = l_timestamp.
      return.
    endif.

  enddo.

endmethod.


method get_period_from_to_text.

  data l_from type d.
  l_from = i_from.

  data l_to type d.
  l_to = i_to.

  if l_from is not initial and
     l_to   is initial.

    e_text = zcl_abap_static=>write( l_from ).

  elseif
    l_from is not initial and
    l_to   is not initial.

    if l_from(6) eq l_to(6) and
     ( l_from+6 eq '01' and
       is_end_of_month( l_to ) eq abap_true ).

      e_text =
        get_period_text(
          i_type   = month
          i_period = i_from ).

    elseif
      l_from(4) eq l_to(4) and
    ( l_from+4 eq '0101' and
      l_to+4   eq '0331' ).

      l_from+4(2) = '01'.

      e_text =
        get_period_text(
          i_type   = quarter
          i_period = i_from ).

    elseif
      l_from(4) eq l_to(4) and
    ( l_from+4 eq '0401' and
      l_to+4   eq '0631' ).

      l_from+4(2) = '02'.

      e_text =
        get_period_text(
          i_type   = quarter
          i_period = l_from ).

    elseif
      l_from(4) eq l_to(4) and
    ( l_from+4 eq '0701' and
      l_to+4   eq '0930' ).

      l_from+4(2) = '03'.

      e_text =
        get_period_text(
          i_type   = quarter
          i_period = l_from ).

    elseif
      l_from(4) eq l_to(4) and
    ( l_from+4 eq '1001' and
      l_to+4   eq '1231' ).

      l_from+4(2) = '04'.

      e_text =
        get_period_text(
          i_type   = quarter
          i_period = l_from ).

    elseif
      l_from(4) eq l_to(4) and
      ( l_from+4 eq '0101' and
        l_to+4   eq '1231' ).

      e_text =
        get_period_text(
          i_type   = year
          i_period = i_from ).

    else.

      e_text = `период с ` && zcl_abap_static=>write( l_from ) && ` по ` && zcl_abap_static=>write( l_to ).

    endif.

  endif.

endmethod.


  method get_period_list.

    data l_from type d.
    l_from = i_from.

    data l_to type d.
    l_to = i_to.
    if l_to is initial.
      l_to = l_from.
    endif.

    if l_from is initial.
      return.
    endif.

    data l_id type string.
    concatenate i_type '/' l_from '/' l_to into l_id.

    try.
        zcl_cache_static=>get_data(
          exporting i_name = 'ZCLSRM_TIME_STATIC=>GET_PERIOD_LIST'
                    i_id   = l_id
          importing e_data = et_list ).
      catch cx_root.
    endtry.

    if et_list is initial.

      case i_type.
        when year.

          data l_year type n length 4.
          l_year = i_from(4).

          do.

            if l_year gt l_to(4).
              exit.
            endif.

            data ls_list like line of et_list.
            ls_list-id = l_year.
            insert ls_list into table et_list.

            add 1 to l_year.

          enddo.

        when quarter.

          do.

            if l_from gt l_to.
              exit.
            endif.

            ls_list-id = get_quarter( l_from ).
            collect ls_list into et_list.

            add 1 to l_from.

          enddo.

        when month.

          do.

            if l_from gt l_to.
              exit.
            endif.

            ls_list-id = l_from(6).
            collect ls_list into et_list.

            add 1 to l_from.

          enddo.

        when week.

          do.

            if l_from gt l_to.
              exit.
            endif.

            ls_list-id = get_week_for_date( l_from ).
            collect ls_list into et_list.

            add 1 to l_from.

          enddo.

        when day.

          do.

            if l_from gt l_to.
              exit.
            endif.

            ls_list-id = l_from.
            insert ls_list into table et_list.

            add 1 to l_from.

          enddo.

        when range.

***          ls_list-from = l_from.
***          ls_list-to   = l_to.
***          insert ls_list into table et_list.

        when others.

          return.

      endcase.

      zcl_cache_static=>set_data(
        i_name = 'ZCLSRM_TIME_STATIC=>GET_PERIOD_LIST'
        i_id   = l_id
        i_data = et_list ).

    endif.

    if i_type eq range.
      return.
    endif.

    data l_lines type i.
    l_lines = lines( et_list ).

    field-symbols <ls_list> like line of et_list.
    loop at et_list assigning <ls_list>.

      data l_tabix type i.
      l_tabix = sy-tabix.

      <ls_list>-text =
        get_period_text(
          i_type   = i_type
          i_period = <ls_list>-id
          i_simple = i_simple_names ).

      data lt_range type zirange.
      lt_range =
        get_period_range(
          i_type   = i_type
          i_period = <ls_list>-id ).

      data ls_range like line of lt_range.
      read table lt_range into ls_range index 1.
      if sy-subrc eq 0.
        <ls_list>-from = ls_range-low.
        if ls_range-option eq 'EQ'.
          <ls_list>-to = ls_range-low.
        else.
          <ls_list>-to = ls_range-high.
        endif.
      endif.

      if l_tabix eq 1.
        if <ls_list>-from lt i_from.
          <ls_list>-from = i_from.
        endif.
      endif.

      if l_tabix eq l_lines.
        if <ls_list>-to gt l_to.
          <ls_list>-to = l_to.
        endif.
      endif.

    endloop.

    sort et_list by id.

  endmethod.


  method get_period_range.

    data l_range(8).
    l_range = i_period.

    case i_type.
      when day.

        data ls_range like line of et_range.
        ls_range     = 'IEQ'.
        ls_range-low  = l_range.
        ls_range-high = l_range.

      when week.

        ls_range      = 'IBT'.
        ls_range-low  = get_week_first_day( l_range ).
        ls_range-high = get_week_last_day( l_range ).

      when month.

        ls_range        = 'IBT'.
        ls_range-low    = l_range.
        ls_range-low+6  = '01'.
        ls_range-high   = l_range.
        ls_range-high+6 = get_month_days( l_range ).

      when quarter.

        ls_range      = 'IBT'.
        ls_range-low  = l_range.
        ls_range-high = l_range.
        case l_range+4(2).
          when '01'.
            ls_range-low+4  = '0101'.
            ls_range-high+4 = '0331'.
          when '02'.
            ls_range-low+4  = '0401'.
            ls_range-high+4 = '0630'.
          when '03'.
            ls_range-low+4  = '0701'.
            ls_range-high+4 = '0930'.
          when '04'.
            ls_range-low+4  = '1001'.
            ls_range-high+4 = '1231'.
          when others.
            assert 1 = 2. " Unknown case
        endcase.

      when year.

        ls_range        = 'IBT'.
        ls_range-low    = l_range.
        ls_range-low+4  = '0101'.
        ls_range-high   = l_range.
        ls_range-high+4 = '1231'.

      when others.
        assert 1 = 2. " Unknown case
    endcase.

    insert ls_range into table et_range.

  endmethod.


  method get_period_text.

    data l_period(10).
    l_period = i_period.

    case i_type.
      when day.

        if i_simple eq abap_true.

          data l_day type i.
          l_day = l_period+6.

          data l_day_txt(2).
          write l_day to l_day_txt left-justified.

          e_text = l_day_txt.

        else.

          data l_date type d.
          l_date = l_period.

          data l_date_txt(10).
          write l_date to l_date_txt.

          e_text = l_date_txt.

        endif.

      when week.

        data l_week type i.
        l_week = l_period+4(2).

        data l_week_txt(2).
        write l_week to l_week_txt left-justified.

        if i_simple = abap_true.
          concatenate l_week_txt text-003 into e_text separated by space.
        else.
          concatenate l_week_txt text-003 l_period(4) text-004 into e_text separated by space.
        endif.

      when month.

        data l_month type string.
        l_month = get_month_text( l_period+4(2) ).

        if i_simple = abap_true.
          e_text = l_month.
        else.
          concatenate l_month l_period(4) text-001 into e_text separated by space.
        endif.

      when quarter.

        if i_simple = abap_true.
          concatenate l_period+5(1) text-002 into e_text separated by space.
        else.
          concatenate l_period+5(1) text-002 l_period(4) text-001 into e_text separated by space.
        endif.

      when year.

        concatenate l_period text-001 into e_text separated by space.

      when others.
        assert 1 = 2. " Unknown case
    endcase.

  endmethod.


  method get_period_timestamp_range.

    et_range =
      get_period_range(
        i_type   = i_type
        i_period = i_period ).

    et_range = convert_range_date2time( et_range ).

  endmethod.


  method get_period_values.

    data lt_periods type tt_periods.
    get_period_list(
      exporting
        i_type         = i_type
        i_from         = i_from
        i_to           = i_to
        i_simple_names = i_simple_names
      importing
        et_list        = lt_periods ).

    data ls_period like line of lt_periods.
    loop at lt_periods into ls_period.
      data ls_value like line of et_values.
      ls_value-id    = ls_period-id.
      ls_value-text  = ls_period-text.
      insert ls_value into table et_values.
    endloop.

  endmethod.


  method get_quarter.

    data l_period(6).
    l_period = i_period.

    data l_month type i.
    l_month = l_period+4(2).
    if l_month is initial.
      return.
    endif.

    e_quarter = l_period(4).

    if l_month between 1 and 3.
      e_quarter+4(2) = '01'.
    elseif l_month between 4 and 6.
      e_quarter+4(2) = '02'.
    elseif l_month between 7 and 9.
      e_quarter+4(2) = '03'.
    elseif l_month between 10 and 12.
      e_quarter+4(2) = '04'.
    endif.

  endmethod.


  method GET_QUARTER_DATE.

    data l_year(4).
    l_year = i_year.

    if i_month is not initial and i_month <> 0.

      data l_month(2).
      l_month = i_month.

    else.

      case i_quarter.
        when 1.
          l_month = '01'.

        when 2.
          l_month = '04'.

        when 3.
          l_month = '07'.

        when 4.
          l_month = '10'.

        when others.
          assert 1 = 2 .
      endcase.

    endif.

    concatenate l_year l_month '01' into e_date.

  endmethod.


  method GET_QUARTER_TEXT.

    data l_quarter(6).
    l_quarter = i_quarter.

    e_text = l_quarter+5(1) && ` квартал`.

    if i_full eq abap_true.

      e_text = e_text && ` ` && l_quarter(4) && ` г.`.

    endif.

  endmethod.


  method get_seconds.

    if i_seconds is not initial.

      e_seconds = i_seconds.

    elseif i_minutes is not initial.

      e_seconds = i_minutes * 60.

    elseif i_hours is not initial.

      e_seconds = i_hours * 60 * 60.

    elseif i_days is not initial.

      e_seconds = i_days * 24 * 60 * 60.

    endif.

  endmethod.


  method GET_TIME.

    convert time stamp i_timestamp time zone i_zone into time e_time.

  endmethod.


  method GET_TIMESTAMP.

    if i_date is supplied or i_time is supplied or i_zone is supplied.

      if i_date is initial and
         i_time is initial.
        return.
      endif.

      data l_date type d.
      l_date = i_date.

      convert date l_date time i_time into time stamp e_timestamp time zone i_zone.

    else.

      get time stamp field e_timestamp.

    endif.

  endmethod.


  method get_timestamp_text.

    data l_date type d.
    data l_time type t.
    convert time stamp i_timestamp time zone i_zone
      into date l_date time l_time.

    e_text =
      zcl_abap_static=>write( l_date ) && ` ` &&
      zcl_abap_static=>write( l_time ).

***    if i_zone_x eq abap_true.
***      e_text = e_text && ` ` && zalrcl_time_static=>get_zone_text( i_zone ).
***    endif.

  endmethod.


  method get_week_first_day.

    data l_week type kweek.
    l_week = i_week.

    data l_date type datum.
    call function 'WEEK_GET_FIRST_DAY'
      exporting
        week         = l_week
      importing
        date         = e_date
      exceptions
        week_invalid = 1
        others       = 2.
    if sy-subrc ne 0.
      zcx_generic=>raise( ).
    endif.

  endmethod.


  method get_week_for_date.

    data l_week type kweek.
    call function 'DATE_GET_WEEK'
      exporting
        date         = i_date
      importing
        week         = l_week
      exceptions
        date_invalid = 1
        others       = 2.
    if sy-subrc <> 0.
      zcx_generic=>raise( ).
    endif.

    e_week = l_week.

  endmethod.


  method get_week_last_day.

    data l_first type datum.
    l_first = get_week_first_day( i_week ).

    e_date = l_first + 6.

  endmethod.


method get_work_days.

  check i_to ge i_from.

***  cl_dpr_distribution_services=>calculate_no_of_workdays(
***    exporting
***      iv_begda                   = i_from
***      iv_endda                   = i_to
***      iv_calendar_id             = 'RU'
***    importing
***      ev_no_workdays             = e_days ).

endmethod.


  method get_year_list.

    do i_years times.

      data l_year type n length 4.
      l_year = sy-datum(4) - sy-index + 1.

      data ls_year like line of et_years.
      ls_year-id    = l_year.
      ls_year-text  = l_year.
      insert ls_year into table et_years.

    enddo.

  endmethod.


  method has_time_come.

    if i_now is initial.
      data l_now type timestamp.
      l_now = get_timestamp( ).
    else.
      l_now = i_now.
    endif.

    if l_now ge i_time.
      e_has = abap_true.
    endif.

  endmethod.


method is_end_of_month.

  check i_date is not initial.

  data l_date1 type d.
  data l_date2 type d.
  l_date1 = l_date2 = i_date.

  add 1 to l_date2.

  check l_date1+4(2) ne l_date2+4(2).

  e_is = abap_true.

endmethod.


method is_work_day.

  data l_date type datum.
  l_date = i_date.

  e_is =
    cl_rs_time_service=>is_factory_workday(
      i_ident = 'AL'
      i_date  = l_date ).

endmethod.


method is_work_hour.

  data lt_values type table of string.
  field-symbols <l_value> like line of lt_values.

  append initial line to lt_values assigning <l_value>.
  <l_value> = '090000'.

  append initial line to lt_values assigning <l_value>.
  <l_value> = '180000'.

  data l_from type t.
  read table lt_values into l_from index 1.

  data l_to type t.
  read table lt_values into l_to index 2.

  check i_time between l_from and l_to.

  e_is = abap_true.

endmethod.


  method round_period.

    data l_period(8).
    l_period = i_period.

    case i_from.
      when day.

        case i_to.
          when day.
            e_period = l_period.

          when month.
            e_period = l_period(6).

          when quarter.

            e_period = l_period(4).

            if l_period+4(2) between 1 and 3.
              e_period+4(2) = '01'.
            elseif l_period+4(2) between 4 and 6.
              e_period+4(2) = '02'.
            elseif l_period+4(2) between 7 and 9.
              e_period+4(2) = '03'.
            elseif l_period+4(2) between 10 and 12.
              e_period+4(2) = '04'.
            endif.

          when year.
            e_period = l_period(4).

        endcase.

      when month.

        case i_to.
          when day.
            e_period   = l_period.
            e_period+6 = '01'.

          when month.
            e_period = l_period.

          when quarter.

            e_period = l_period(4).

            if l_period+4(2) between 1 and 3.
              e_period+4(2) = '01'.
            elseif l_period+4(2) between 4 and 6.
              e_period+4(2) = '02'.
            elseif l_period+4(2) between 7 and 9.
              e_period+4(2) = '03'.
            elseif l_period+4(2) between 10 and 12.
              e_period+4(2) = '04'.
            endif.

          when year.
            e_period = l_period(4).

        endcase.

      when quarter.

        case i_to.
          when day.

            e_period   = l_period(4).
            e_period+6 = '01'.

            if l_period+4(2) eq '01'.
              e_period+4(2) = '01'.
            elseif l_period+4(2) eq '02'.
              e_period+4(2) = '04'.
            elseif l_period+4(2) eq '03'.
              e_period+4(2) = '07'.
            elseif l_period+4(2) eq '04'.
              e_period+4(2) = '10'.
            endif.

          when month.

            e_period   = l_period(4).

            if l_period+4(2) eq '01'.
              e_period+4(2) = '01'.
            elseif l_period+4(2) eq '02'.
              e_period+4(2) = '04'.
            elseif l_period+4(2) eq '03'.
              e_period+4(2) = '07'.
            elseif l_period+4(2) eq '04'.
              e_period+4(2) = '10'.
            endif.

          when quarter.

            e_period = l_period.

          when year.
            e_period = l_period(4).

        endcase.

      when year.

        case i_to.
          when day.
            e_period   = l_period.
            e_period+4 = '0101'.

          when month.
            e_period   = l_period.
            e_period+4 = '01'.

          when quarter.
            e_period   = l_period.
            e_period+4 = '01'.

          when year.
            e_period = l_period.

        endcase.

    endcase.

  endmethod.


  method subtract.

    if i_time is initial.
      data l_time like i_time.
      l_time = get_timestamp( ).
    else.
      l_time = i_time.
    endif.

    data l_seconds type i.
    l_seconds =
      get_seconds(
        i_seconds = i_seconds
        i_minutes = i_minutes
        i_hours   = i_hours
        i_days    = i_days ).

    e_time =
      cl_abap_tstmp=>subtractsecs(
        tstmp = l_time
        secs  = l_seconds ).

  endmethod.


method subtract_work_days.

  e_date = i_date.

  do 999 times.

    data l_days type i.
    if l_days ge i_days.
      return.
    endif.

    subtract 1 from e_date.

    if is_work_day( e_date ) eq abap_true.
      add 1 to l_days.
    endif.

  enddo.

  assert 1 = 2.

endmethod.
ENDCLASS.
