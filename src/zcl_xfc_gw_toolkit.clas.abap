CLASS zcl_xfc_gw_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_identifier TYPE RANGE OF /iwfnd/med_mdl_srg_identifier.

    METHODS constructor
      IMPORTING io_cache TYPE REF TO zcl_xfc_lru_cache_toolkit DEFAULT zcl_xfc_lru_cache_toolkit=>default.

    METHODS invalidate_client_caches
      IMPORTING iv_uname TYPE clike OPTIONAL
      RAISING   zcx_xfc_toolkit_error.

    METHODS invalidate_global_caches
      RAISING zcx_xfc_toolkit_error.

    METHODS cleanup_metadata_cache
      IMPORTING it_identifier TYPE tt_identifier
      RAISING   zcx_xfc_toolkit_error.

    METHODS app_index_calculate
      IMPORTING iv_repository TYPE /ui5/ui5_repository_ui OPTIONAL
      RAISING   zcx_xfc_toolkit_error.

    METHODS invalidate_backend_contexts
      RAISING zcx_xfc_toolkit_error.

    METHODS clear_metadata_cache
      IMPORTING iv_group_id    TYPE /iwfnd/v4_med_group_id
                iv_service_key TYPE /iwfnd/s_v4_med_service_key
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    METHODS check_tadir
      IMPORTING iv_pgmid         TYPE clike DEFAULT 'R3TR'
                iv_object        TYPE clike DEFAULT 'PROG'
                iv_obj_name      TYPE clike
      RETURNING VALUE(rv_result) TYPE abap_bool.

    DATA !cache TYPE REF TO zcl_xfc_lru_cache_toolkit.
ENDCLASS.


CLASS zcl_xfc_gw_toolkit IMPLEMENTATION.
  METHOD constructor.
    cache = io_cache.
  ENDMETHOD.

  METHOD invalidate_client_caches.
    CONSTANTS lc_prog TYPE tadir-obj_name VALUE '/UI2/INVALIDATE_CLIENT_CACHES'.

    IF NOT check_tadir( lc_prog ).
      zcx_xfc_toolkit_error=>raise( |Program '{ lc_prog }' does not exist.| ).
    ENDIF.

    IF iv_uname IS INITIAL.
      SUBMIT (lc_prog) WITH gv_all = abap_true AND RETURN.
    ELSE.
      SUBMIT (lc_prog) WITH gv_all  = abap_false
                       WITH gv_user = abap_true
                       WITH g_uname = iv_uname AND RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD check_tadir.
    DATA lv_key TYPE zcl_xfc_lru_cache_toolkit=>ty_key.

    lv_key = |TADIR:{ iv_pgmid }:{ iv_object }:{ iv_obj_name }|.

    cache->get( EXPORTING  iv_key   = lv_key
                IMPORTING  ev_value = rv_result
                EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      SELECT SINGLE COUNT(*) FROM tadir
        WHERE pgmid    = iv_pgmid
          AND object   = iv_object
          AND obj_name = iv_obj_name.
      rv_result = xsdbool( sy-subrc = 0 ).

      cache->put( iv_key   = lv_key
                  iv_value = rv_result ).
    ENDIF.
  ENDMETHOD.

  METHOD invalidate_global_caches.
    CONSTANTS lc_prog TYPE tadir-obj_name VALUE '/UI2/INVALIDATE_GLOBAL_CACHES'.

    IF NOT check_tadir( lc_prog ).
      zcx_xfc_toolkit_error=>raise( |Program '{ lc_prog }' does not exist.| ).
    ENDIF.

    SUBMIT (lc_prog) WITH gv_test = abap_false
                     WITH gv_exe = abap_true AND RETURN.
  ENDMETHOD.

  METHOD cleanup_metadata_cache.
    SELECT * FROM /iwfnd/i_med_srh
      INTO TABLE @DATA(lt_services)
      WHERE srv_identifier IN @it_identifier.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise( 'Services not found.' ).
    ENDIF.

    LOOP AT lt_services ASSIGNING FIELD-SYMBOL(<fs_service>).
      /iwfnd/cl_sutil_moni=>cleanup_metadata_cache( EXPORTING iv_mode            = 'A'
                                                              iv_multi_origin    = abap_true
                                                              iv_namespace       = <fs_service>-namespace
                                                              iv_service_name    = <fs_service>-service_name
                                                              iv_service_version = <fs_service>-service_version
                                                    IMPORTING ev_error_text      = DATA(lv_error_text) ).
      IF lv_error_text IS INITIAL.
        CONTINUE.
      ENDIF.
      zcx_xfc_toolkit_error=>raise( lv_error_text ).
    ENDLOOP.
  ENDMETHOD.

  METHOD app_index_calculate.
    CONSTANTS lc_prog TYPE tadir-obj_name VALUE '/UI5/APP_INDEX_CALCULATE'.

    IF NOT check_tadir( lc_prog ).
      zcx_xfc_toolkit_error=>raise( |Program '{ lc_prog }' does not exist.| ).
    ENDIF.

    IF iv_repository IS INITIAL.
      SUBMIT (lc_prog) WITH p_all_a = abap_true
                       WITH p_all_d = abap_false AND RETURN.
    ELSE.
      SUBMIT (lc_prog) WITH p_all   = abap_false
                       WITH p_distl = abap_false
                       WITH p_repo  = iv_repository AND RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD invalidate_backend_contexts.
    DATA lo_app_index TYPE REF TO /ui5/cl_ui5_app_index.

    lo_app_index ?= /ui5/cl_ui5_app_api_factory=>get_app_index_instance( ).

    TRY.
        lo_app_index->invalidate_backend_contexts( ).
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD clear_metadata_cache.
    /iwfnd/cl_v2_v4_config_facade=>clear_metadata_cache( EXPORTING iv_group_id    = iv_group_id
                                                                   is_service_key = iv_service_key
                                                         IMPORTING ev_error_text  = DATA(lv_error_text) ).
    IF lv_error_text IS NOT INITIAL.
      zcx_xfc_toolkit_error=>raise( lv_error_text ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
