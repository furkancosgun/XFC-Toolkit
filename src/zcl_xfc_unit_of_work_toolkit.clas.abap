CLASS zcl_xfc_unit_of_work_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS bapi_transaction_commit
      IMPORTING iv_wait TYPE abap_bool DEFAULT abap_true
                iv_safe TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS bapi_transaction_rollback
      IMPORTING iv_safe TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS commit
      IMPORTING iv_wait TYPE abap_bool DEFAULT abap_true
                iv_safe TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS rollback
      IMPORTING iv_safe TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS is_in_update_task
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.


CLASS zcl_xfc_unit_of_work_toolkit IMPLEMENTATION.
  METHOD bapi_transaction_commit.
    return_if_safe_on.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = iv_wait.
  ENDMETHOD.

  METHOD bapi_transaction_rollback.
    return_if_safe_on.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDMETHOD.

  METHOD commit.
    return_if_safe_on.

    IF iv_wait = abap_true.
      COMMIT WORK AND WAIT.
    ELSE.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD rollback.
    return_if_safe_on.

    ROLLBACK WORK.
  ENDMETHOD.

  METHOD is_in_update_task.
    DATA lv_in_update_task TYPE sy-subrc.

    CALL FUNCTION 'TH_IN_UPDATE_TASK'
      IMPORTING in_update_task = lv_in_update_task.

    rv_result = xsdbool( lv_in_update_task = 1 ).
  ENDMETHOD.
ENDCLASS.
