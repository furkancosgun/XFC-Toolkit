CLASS zcl_xfc_gui_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_fields TYPE STANDARD TABLE OF dynpread WITH DEFAULT KEY.

    METHODS dynp_values_read
      IMPORTING iv_prog   TYPE sy-repid
                iv_dynp   TYPE sy-dynnr
      CHANGING  ct_fields TYPE tt_fields
      RAISING   zcx_xfc_toolkit_error.

    METHODS dynp_values_update
      IMPORTING iv_prog   TYPE sy-repid
                iv_dynp   TYPE sy-dynnr
                it_fields TYPE tt_fields
      RAISING   zcx_xfc_toolkit_error.
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
ENDCLASS.
