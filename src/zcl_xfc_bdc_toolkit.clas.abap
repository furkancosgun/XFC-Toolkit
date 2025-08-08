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

    TYPES tt_string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.

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
      IMPORTING iv_cursor          TYPE bdcdata-fnam
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_bdc_toolkit.

    METHODS set_okcode
      IMPORTING iv_okcode          TYPE bdcdata-fval
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_bdc_toolkit.

    METHODS call
      IMPORTING iv_display_mode  TYPE ctu_params-dismode DEFAULT mc_display_modes-nothing
                iv_update_mode   TYPE ctu_params-updmode DEFAULT mc_update_modes-async
                is_params        TYPE ctu_params         OPTIONAL
      RETURNING VALUE(rt_return) TYPE ettcd_msg_tabtype.

    CLASS-METHODS generate
      IMPORTING iv_qid           TYPE apqd-qid
      RETURNING VALUE(rt_result) TYPE tt_string_table
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    DATA tcode   TYPE sy-tcode.
    DATA bdcdata TYPE TABLE OF bdcdata.
ENDCLASS.


CLASS zcl_xfc_bdc_toolkit IMPLEMENTATION.
  METHOD constructor.
    tcode = iv_tcode.
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
                             iv_fieldvalue = iv_cursor ).
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

  METHOD generate.
    DATA lt_bdctab TYPE STANDARD TABLE OF bdcdata.

    SELECT SINGLE * FROM apqi WHERE qid = @iv_qid INTO @DATA(ls_apqi).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BDC_OBJECT_READ'
      EXPORTING  queue_id  = ls_apqi-qid
                 datatype  = ls_apqi-datatyp
      TABLES     dynprotab = lt_bdctab
      EXCEPTIONS OTHERS    = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.

    ASSIGN lt_bdctab[ 1 ] TO FIELD-SYMBOL(<fs_bdctab>).
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise( 'BDC record not found.' ).
    ENDIF.

    APPEND |DATA(lo_bdc_tool) = NEW zcl_xfc_bdc_toolkit( '{ <fs_bdctab>-fnam }' ).| TO rt_result.
    LOOP AT lt_bdctab ASSIGNING <fs_bdctab> FROM 2.
      CASE <fs_bdctab>-dynbegin.
        WHEN abap_true.
          APPEND |lo_bdc_tool->set_dynpro(| TO rt_result.
          APPEND |  EXPORTING| TO rt_result.
          APPEND |    iv_program = '{ <fs_bdctab>-program }'| TO rt_result.
          APPEND |    iv_dynpro  = '{ <fs_bdctab>-dynpro }'| TO rt_result.
          APPEND |).| TO rt_result.
        WHEN abap_false.
          CASE <fs_bdctab>-fnam.
            WHEN 'BDC_CURSOR'.
              APPEND |lo_bdc_tool->set_cursor( '{ <fs_bdctab>-fval }' ).| TO rt_result.
            WHEN 'BDC_OKCODE'.
              APPEND |lo_bdc_tool->set_okcode( '{ <fs_bdctab>-fval }' ).| TO rt_result.
            WHEN OTHERS.
              APPEND |lo_bdc_tool->set_field(| TO rt_result.
              APPEND |  EXPORTING| TO rt_result.
              APPEND |    iv_fieldname  = '{ <fs_bdctab>-fnam }'| TO rt_result.
              APPEND |    iv_fieldvalue = '{ <fs_bdctab>-fval }'| TO rt_result.
              APPEND |).| TO rt_result.
          ENDCASE.
      ENDCASE.
    ENDLOOP.

    APPEND |DATA(lt_return) = lo_bdc_tool->call(| TO rt_result.
    APPEND |*                    iv_display_mode = zcl_xfc_bdc_toolkit=>mc_display_modes-nothing| TO rt_result.
    APPEND |*                    iv_update_mode  = zcl_xfc_bdc_toolkit=>mc_update_modes-async| TO rt_result.
    APPEND |*                    is_params       = | TO rt_result.
    APPEND |                  ).| TO rt_result.
  ENDMETHOD.
ENDCLASS.
