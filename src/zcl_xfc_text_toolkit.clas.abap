CLASS zcl_xfc_text_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS constructor
      IMPORTING io_cache TYPE REF TO zcl_xfc_lru_cache_toolkit DEFAULT zcl_xfc_lru_cache_toolkit=>default.

    METHODS read_text
      IMPORTING iv_client        TYPE sy-mandt      DEFAULT sy-mandt
                iv_id            TYPE thead-tdid
                iv_language      TYPE thead-tdspras DEFAULT sy-langu
                iv_name          TYPE thead-tdname
                iv_object        TYPE thead-tdobject
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    METHODS save_text
      IMPORTING iv_client        TYPE sy-mandt DEFAULT sy-mandt
                iv_id            TYPE thead-tdid
                iv_language      TYPE thead-tdspras DEFAULT sy-langu
                iv_name          TYPE thead-tdname
                iv_object        TYPE thead-tdobject
                iv_text          TYPE string
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_xfc_toolkit_error.

    METHODS text_to_table
      IMPORTING iv_text          TYPE string
                iv_length        TYPE i
      RETURNING VALUE(rt_result) TYPE tt_string_table.

  PRIVATE SECTION.
    DATA cache TYPE REF TO zcl_xfc_lru_cache_toolkit.

ENDCLASS.


CLASS zcl_xfc_text_toolkit IMPLEMENTATION.
  METHOD constructor.
    cache = io_cache.
  ENDMETHOD.

  METHOD read_text.
    DATA lv_key   TYPE zcl_xfc_lru_cache_toolkit=>ty_key.
    DATA lt_lines TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.

    lv_key = |READ_TEXT:{ iv_client }:{ iv_id }:{ iv_language }:{ iv_name }:{ iv_object }|.

    cache->get( EXPORTING  iv_key   = lv_key
                IMPORTING  ev_value = lt_lines
                EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client   = iv_client
          id       = iv_id
          language = iv_language
          name     = iv_name
          object   = iv_object
        TABLES
          lines    = lt_lines
        EXCEPTIONS
          OTHERS   = 1.
      IF sy-subrc <> 0.
        zcx_xfc_toolkit_error=>raise_syst( ).
      ENDIF.
      cache->put( iv_key   = lv_key
                  iv_value = lt_lines ).
    ENDIF.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>).
      IF sy-tabix = 0.
        rv_result = <fs_line>-tdline.
      ELSE.
        rv_result = |{ rv_result } { <fs_line>-tdline }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_text.
    DATA lv_key    TYPE zcl_xfc_lru_cache_toolkit=>ty_key.
    DATA ls_header TYPE thead.
    DATA lt_texts  TYPE tt_string_table.
    DATA lt_lines  TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.

    lv_key = |READ_TEXT:{ iv_client }:{ iv_id }:{ iv_language }:{ iv_name }:{ iv_object }|.

    ls_header-mandt    = iv_client.
    ls_header-tdid     = iv_id.
    ls_header-tdname   = iv_name.
    ls_header-tdobject = iv_object.
    ls_header-tdspras  = iv_language.

    lt_texts = text_to_table( iv_text   = iv_text
                              iv_length = 132 ).

    LOOP AT lt_texts ASSIGNING FIELD-SYMBOL(<fs_text>).
      APPEND VALUE #( tdline = <fs_text> ) TO lt_lines.
    ENDLOOP.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = iv_client
        header          = ls_header
        insert          = abap_true
        savemode_direct = abap_true
      TABLES
        lines           = lt_lines
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.

    cache->put( iv_key   = lv_key
                iv_value = lt_lines ).
  ENDMETHOD.

  METHOD text_to_table.
    DATA lv_off TYPE i.
    DATA lv_len TYPE i.
    DATA lv_pos TYPE i.

    lv_off = 0.
    lv_len = strlen( iv_text ).
    WHILE lv_off < lv_len.
      lv_pos = lv_len - lv_off.
      IF lv_pos > iv_length.
        APPEND substring( val = iv_text
                          off = lv_off
                          len = lv_len ) TO rt_result.
        lv_off = lv_off + iv_length.
      ELSE.
        APPEND substring( val = iv_text
                          off = lv_off
                          len = lv_pos ) TO rt_result.
        lv_off = lv_off + lv_pos.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
