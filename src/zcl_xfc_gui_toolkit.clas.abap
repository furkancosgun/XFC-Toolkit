CLASS zcl_xfc_gui_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_fields TYPE STANDARD TABLE OF dynpread WITH DEFAULT KEY.

    CLASS-METHODS dynp_values_read
      IMPORTING iv_prog   TYPE sy-repid
                iv_dynp   TYPE sy-dynnr
      CHANGING  ct_fields TYPE tt_fields
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS dynp_values_update
      IMPORTING iv_prog   TYPE sy-repid
                iv_dynp   TYPE sy-dynnr
                it_fields TYPE tt_fields
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS icon_create
      IMPORTING iv_icon          TYPE csequence
                iv_text          TYPE csequence OPTIONAL
                iv_quickinfo     TYPE csequence OPTIONAL
                iv_add_qinfo     TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_result) TYPE icon_text
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS is_gui_available
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_html_client_running
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.


CLASS zcl_xfc_gui_toolkit IMPLEMENTATION.
  METHOD dynp_values_read.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = iv_prog
        dynumb             = iv_dynp
        translate_to_upper = abap_true
      TABLES
        dynpfields         = ct_fields
      EXCEPTIONS
        OTHERS             = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD dynp_values_update.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = iv_prog
        dynumb     = iv_dynp
      TABLES
        dynpfields = it_fields
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD icon_create.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name       = iv_icon
        text       = iv_text
        info       = iv_quickinfo
        add_stdinf = CONV icon_int( iv_add_qinfo )
      IMPORTING
        result     = rv_result
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD is_gui_available.
    " sy-batch to check for background process
    " sy-binpt to check for batch processing
    " sy-oncom = 'P' to check for commit operation going on
    " sy-oncom = 'X' to check for RFC call
    IF    sy-batch  = abap_true
       OR sy-binpt <> space
       OR sy-oncom CA 'PX'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_result.
  ENDMETHOD.

  METHOD is_html_client_running.
    rv_result = cl_gui_object=>www_active.
  ENDMETHOD.
ENDCLASS.
