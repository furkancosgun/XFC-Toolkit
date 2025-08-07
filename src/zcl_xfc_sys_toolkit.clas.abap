CLASS zcl_xfc_sys_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_cache TYPE REF TO zcl_xfc_lru_cache_toolkit DEFAULT zcl_xfc_lru_cache_toolkit=>default.

    METHODS is_production
      IMPORTING iv_client        TYPE sy-mandt DEFAULT sy-mandt
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_xfc_toolkit_error.

    METHODS is_test
      IMPORTING iv_client        TYPE sy-mandt DEFAULT sy-mandt
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_xfc_toolkit_error.

    METHODS is_customizing
      IMPORTING iv_client        TYPE sy-mandt DEFAULT sy-mandt
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_xfc_toolkit_error.

    METHODS is_demo
      IMPORTING iv_client        TYPE sy-mandt DEFAULT sy-mandt
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_xfc_toolkit_error.

    METHODS is_education
      IMPORTING iv_client        TYPE sy-mandt DEFAULT sy-mandt
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    METHODS get_client
      IMPORTING iv_client        TYPE sy-mandt DEFAULT sy-mandt
      RETURNING VALUE(rs_result) TYPE t000
      RAISING   zcx_xfc_toolkit_error.

    DATA cache TYPE REF TO zcl_xfc_lru_cache_toolkit.

ENDCLASS.


CLASS zcl_xfc_sys_toolkit IMPLEMENTATION.
  METHOD constructor.
    cache = io_cache.
  ENDMETHOD.

  METHOD get_client.
    DATA lv_key TYPE zcl_xfc_lru_cache_toolkit=>ty_key.

    lv_key = |T000:{ iv_client }|.

    cache->get( EXPORTING  iv_key   = lv_key
                IMPORTING  ev_value = rs_result
                EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM t000 INTO rs_result WHERE mandt = iv_client.
      IF sy-subrc <> 0.
        zcx_xfc_toolkit_error=>raise( |{ iv_client } client not found.| ).
      ENDIF.
      cache->put( iv_key   = lv_key
                  iv_value = rs_result ).
    ENDIF.
  ENDMETHOD.

  METHOD is_customizing.
    rv_result = xsdbool( get_client( iv_client = iv_client )-cccategory = 'C' ).
  ENDMETHOD.

  METHOD is_demo.
    rv_result = xsdbool( get_client( iv_client = iv_client )-cccategory = 'D' ).
  ENDMETHOD.

  METHOD is_education.
    rv_result = xsdbool( get_client( iv_client = iv_client )-cccategory = 'E' ).
  ENDMETHOD.

  METHOD is_production.
    rv_result = xsdbool( get_client( iv_client = iv_client )-cccategory = 'P' ).
  ENDMETHOD.

  METHOD is_test.
    rv_result = xsdbool( get_client( iv_client = iv_client )-cccategory = 'T' ).
  ENDMETHOD.
ENDCLASS.
