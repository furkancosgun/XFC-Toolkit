CLASS zcl_xfc_log_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_object     TYPE balobj_d
                iv_sub_object TYPE balobj_d OPTIONAL
                iv_extnum     TYPE balnrext OPTIONAL
                iv_autosave   TYPE boolean  DEFAULT abap_true
      RAISING   zcx_xfc_toolkit_error.

    METHODS i
      IMPORTING is_message TYPE bal_s_msg OPTIONAL
                iv_message TYPE clike     OPTIONAL
                  PREFERRED PARAMETER iv_message
      RAISING   zcx_xfc_toolkit_error.

    METHODS s
      IMPORTING is_message TYPE bal_s_msg OPTIONAL
                iv_message TYPE clike     OPTIONAL
                  PREFERRED PARAMETER iv_message
      RAISING   zcx_xfc_toolkit_error.

    METHODS w
      IMPORTING is_message TYPE bal_s_msg OPTIONAL
                iv_message TYPE clike     OPTIONAL
                  PREFERRED PARAMETER iv_message
      RAISING   zcx_xfc_toolkit_error.

    METHODS e
      IMPORTING is_message TYPE bal_s_msg OPTIONAL
                iv_message TYPE clike     OPTIONAL
                  PREFERRED PARAMETER iv_message
      RAISING   zcx_xfc_toolkit_error.

    METHODS a
      IMPORTING is_message TYPE bal_s_msg OPTIONAL
                iv_message TYPE clike     OPTIONAL
                  PREFERRED PARAMETER iv_message
      RAISING   zcx_xfc_toolkit_error.

    METHODS x
      IMPORTING is_message TYPE bal_s_msg OPTIONAL
                iv_message TYPE clike     OPTIONAL
                  PREFERRED PARAMETER iv_message
      RAISING   zcx_xfc_toolkit_error.

    METHODS display
      RAISING zcx_xfc_toolkit_error.

    METHODS save
      RAISING zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    METHODS log
      IMPORTING is_message TYPE bal_s_msg
                iv_message TYPE clike
                iv_type    TYPE msgty
      RAISING   zcx_xfc_toolkit_error.

    METHODS map_message
      IMPORTING iv_message TYPE char200
      CHANGING  cs_message TYPE bal_s_msg.

    DATA log_handle      TYPE balloghndl.
    DATA log_handle_tabl TYPE bal_t_logh.
    DATA log_autosave    TYPE boolean.
ENDCLASS.


CLASS zcl_xfc_log_toolkit IMPLEMENTATION.
  METHOD a.
    log( is_message = is_message
         iv_message = iv_message
         iv_type    = if_xo_const_message=>abort ).
  ENDMETHOD.

  METHOD constructor.
    DATA ls_log TYPE bal_s_log.

    ls_log-extnumber = iv_extnum.
    ls_log-object    = iv_object.
    ls_log-subobject = iv_sub_object.
    ls_log-aldate    = sy-datum.
    ls_log-altime    = sy-uzeit.
    ls_log-aluser    = sy-uname.
    ls_log-alprog    = sy-cprog.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log
      IMPORTING
        e_log_handle = log_handle
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.

    log_handle_tabl = VALUE #( ( log_handle ) ).
    log_autosave    = iv_autosave.
  ENDMETHOD.

  METHOD display.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle = log_handle_tabl
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD e.
    log( is_message = is_message
         iv_message = iv_message
         iv_type    = if_xo_const_message=>error ).
  ENDMETHOD.

  METHOD i.
    log( is_message = is_message
         iv_message = iv_message
         iv_type    = if_xo_const_message=>info ).
  ENDMETHOD.

  METHOD log.
    DATA ls_message TYPE bal_s_msg.

    IF is_message IS INITIAL AND iv_message IS INITIAL.
      RETURN.
    ENDIF.

    ls_message       = is_message.
    ls_message-msgty = iv_type.

    IF iv_message IS NOT INITIAL.
      map_message( EXPORTING iv_message = iv_message
                   CHANGING  cs_message = ls_message ).
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg      = ls_message
        i_log_handle = log_handle
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.

    IF me->log_autosave = abap_false.
      RETURN.
    ENDIF.

    save( ).
  ENDMETHOD.

  METHOD map_message.
    cs_message-msgid = 'DB'.
    cs_message-msgno = '000'.
    cs_message-msgv1 = iv_message(50).
    cs_message-msgv2 = iv_message+50(50).
    cs_message-msgv3 = iv_message+100(50).
    cs_message-msgv4 = iv_message+150(50).
  ENDMETHOD.

  METHOD s.
    log( is_message = is_message
         iv_message = iv_message
         iv_type    = if_xo_const_message=>success ).
  ENDMETHOD.

  METHOD save.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_save_all     = 'X'
        i_t_log_handle = log_handle_tabl
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD w.
    log( is_message = is_message
         iv_message = iv_message
         iv_type    = if_xo_const_message=>warning ).
  ENDMETHOD.

  METHOD x.
    log( is_message = is_message
         iv_message = iv_message
         iv_type    = if_xo_const_message=>exit ).
  ENDMETHOD.
ENDCLASS.
