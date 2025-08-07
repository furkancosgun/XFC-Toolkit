CLASS zcl_xfc_bdc_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF mc_display_modes,
        nothing     TYPE c LENGTH 1 VALUE 'N',
        all_screens TYPE c LENGTH 1 VALUE 'A',
        if_error    TYPE c LENGTH 1 VALUE 'E',
      END OF mc_display_modes.

    CONSTANTS:
      BEGIN OF mc_update_modes,
        async TYPE c LENGTH 1 VALUE 'A',
        sync  TYPE c LENGTH 1 VALUE 'S',
        local TYPE c LENGTH 1 VALUE 'L',
      END OF mc_update_modes.

    CLASS-METHODS create
      IMPORTING iv_tcode           TYPE sy-tcode
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_bdc_toolkit.

    METHODS constructor
      IMPORTING iv_tcode TYPE sy-tcode.

    METHODS set_dynpro
      IMPORTING iv_program         TYPE bdcdata-program
                iv_dynpro          TYPE bdcdata-dynpro
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_bdc_toolkit.

    METHODS set_field
      IMPORTING iv_fieldname       TYPE bdcdata-fnam
                iv_fieldvalue      TYPE any
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_bdc_toolkit.

    METHODS set_cursor
      IMPORTING iv_fieldname       TYPE bdcdata-fnam
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_bdc_toolkit.

    METHODS set_okcode
      IMPORTING iv_okcode          TYPE bdcdata-fval
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_bdc_toolkit.

    METHODS call
      IMPORTING iv_display_mode  TYPE ctu_params-dismode DEFAULT mc_display_modes-nothing
                iv_update_mode   TYPE ctu_params-updmode DEFAULT mc_update_modes-async
                is_params        TYPE ctu_params         OPTIONAL
      RETURNING VALUE(rt_return) TYPE ettcd_msg_tabtype.

  PRIVATE SECTION.
    DATA tcode   TYPE sy-tcode.
    DATA bdcdata TYPE TABLE OF bdcdata.
ENDCLASS.


CLASS zcl_xfc_bdc_toolkit IMPLEMENTATION.
  METHOD constructor.
    tcode = iv_tcode.
  ENDMETHOD.

  METHOD create.
    ro_instance = NEW #( iv_tcode = iv_tcode ).
  ENDMETHOD.

  METHOD set_dynpro.
    APPEND VALUE #( program  = iv_program
                    dynpro   = iv_dynpro
                    dynbegin = abap_true ) TO me->bdcdata.
    ro_instance = me.
  ENDMETHOD.

  METHOD set_field.
    APPEND INITIAL LINE TO bdcdata ASSIGNING FIELD-SYMBOL(<fs_bdcdata>).
    WRITE iv_fieldname TO <fs_bdcdata>-fnam LEFT-JUSTIFIED.
    WRITE iv_fieldvalue TO <fs_bdcdata>-fval LEFT-JUSTIFIED.
    ro_instance = me.
  ENDMETHOD.

  METHOD set_cursor.
    ro_instance = set_field( iv_fieldname  = 'BDC_CURSOR'
                             iv_fieldvalue = iv_fieldname ).
  ENDMETHOD.

  METHOD set_okcode.
    ro_instance = set_field( iv_fieldname  = 'BDC_OKCODE'
                             iv_fieldvalue = iv_okcode ).
  ENDMETHOD.

  METHOD call.
    DATA ls_params TYPE ctu_params.

    ls_params = is_params.

    IF ls_params-dismode IS INITIAL.
      ls_params-dismode = iv_display_mode.
    ENDIF.
    IF ls_params-updmode IS INITIAL.
      ls_params-updmode = iv_update_mode.
    ENDIF.

    CALL TRANSACTION tcode
         USING bdcdata
         OPTIONS FROM ls_params
         MESSAGES INTO rt_return.
  ENDMETHOD.
ENDCLASS.
