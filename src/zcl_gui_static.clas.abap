class ZCL_GUI_STATIC definition
  public
  final
  create public .

*"* public components of class ZCL_GUI_STATIC
*"* do not include other source files here!!!
*"* protected components of class ZCL_GUI_STATIC
*"* do not include other source files here!!!
public section.

  class-methods OPEN_URL
    importing
      !I_URL type STRING
    raising
      ZCX_GENERIC .
  class-methods GET_URL
    importing
      !I_TRANS type SIMPLE
      !I_ACTION type SIMPLE optional
      !IT_PARAMS type TIHTTPNVP optional
    returning
      value(E_URL) type STRING .
  class-methods POPUP_TO_CONFIRM
    importing
      !I_TITLE type SIMPLE optional
      !I_TEXT type SIMPLE optional
    returning
      value(E_ANSWER) type ABAP_BOOL
    raising
      ZCX_GENERIC .
  class-methods PROGRESS_INDICATOR
    importing
      !I_STEP type I
      !I_STEPS type I
      !I_EVERY type I default 1
      !I_TEXT type STRING optional .
  class-methods SHOW_CLASS_METHOD
    importing
      !I_CLASS type SIMPLE
      !I_METHOD type SIMPLE
    raising
      ZCX_GENERIC .
  protected section.
*"* private components of class ZCL_GUI_STATIC
*"* do not include other source files here!!!
  private section.
ENDCLASS.



CLASS ZCL_GUI_STATIC IMPLEMENTATION.


  method get_url.

    data l_trans type sy-tcode.
    l_trans = i_trans.

    cl_its_runtime=>get_url(
      exporting in_transaction = l_trans
      importing out_abs_url    = e_url ).

    data ls_param like line of it_params.
    loop at it_params into ls_param.

      data l_param type string.
      concatenate ls_param-name '=' ls_param-value into l_param.

      data l_params type string.
      if l_params is initial.
        l_params = l_param.
      else.
        concatenate l_params l_param into l_params separated by ';'.
      endif.

    endloop.

    if l_params is not initial.
      concatenate e_url l_params into e_url separated by space.
    endif.

    if i_action is not initial.
      concatenate e_url '&~okcode=' i_action into e_url.
    endif.

  endmethod.


  method open_url.

    type-pools ole2.

    data lr_explorer type ole2_object.
    create object lr_explorer 'InternetExplorer.Application'. "#EC NOTEXT

    set property of lr_explorer 'Visible' = 1.              "#EC NOTEXT

    call method of lr_explorer 'Navigate2' "#EC NOTEXT
      exporting
        #1 = i_url.

  endmethod.


  method popup_to_confirm.

    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar              = i_title
        text_question         = i_text
        display_cancel_button = abap_false
      importing
        answer                = e_answer
      exceptions
        text_not_found        = 1
        others                = 2.
    if sy-subrc ne 0.
      zcx_generic=>raise( ).
    endif.

    case e_answer.
      when '1'.
        e_answer = abap_true.
      when '2'.
        e_answer = abap_false.
    endcase.

  endmethod.


  method progress_indicator.

    data l type i.
    l = i_step mod i_every.
    if l ne 0.
      return.
    endif.

    data l_percentage type f.
    l_percentage = i_step / i_steps * 100.

    if i_text is supplied.
      data l_text type string.
      l_text = i_text.
    else.
      data l_step(16).
      write i_step to l_step left-justified.
      data l_steps(16).
      write i_steps to l_steps left-justified.
      concatenate l_step 'from' l_steps into l_text separated by space.
    endif.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = l_percentage
        text       = l_text.

  endmethod.


  method show_class_method.

    check i_class is not initial.
    check i_method is not initial.

    call function 'RS_TOOL_ACCESS'
      exporting
        operation           = 'SHOW'
        object_name         = i_method
        object_type         = 'OM'
        enclosing_object    = i_class
        with_objectlist     = 'X'
      exceptions
        not_executed        = 1
        invalid_object_type = 2
        others              = 3.

  endmethod.
ENDCLASS.
