CLASS zcl_xfc_unit_of_work_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS bapi_transaction_commit
      IMPORTING iv_wait TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS bapi_transaction_rollback.

    CLASS-METHODS commit
      IMPORTING iv_wait TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS rollback.
ENDCLASS.


CLASS zcl_xfc_unit_of_work_toolkit IMPLEMENTATION.
  METHOD bapi_transaction_commit.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = iv_wait.
  ENDMETHOD.

  METHOD bapi_transaction_rollback.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDMETHOD.

  METHOD commit.
    IF iv_wait = abap_true.
      COMMIT WORK AND WAIT.
    ELSE.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD rollback.
    ROLLBACK WORK.
  ENDMETHOD.
ENDCLASS.
