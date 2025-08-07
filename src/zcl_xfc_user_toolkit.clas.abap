CLASS zcl_xfc_user_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_cache TYPE REF TO zcl_xfc_lru_cache_toolkit DEFAULT zcl_xfc_lru_cache_toolkit=>default.

    METHODS get_full_name
      IMPORTING iv_uname         TYPE clike
      RETURNING VALUE(rv_result) TYPE string.

    METHODS get_email
      IMPORTING iv_uname         TYPE clike
      RETURNING VALUE(rv_result) TYPE string.

    METHODS get_mobile_number
      IMPORTING iv_uname         TYPE clike
      RETURNING VALUE(rv_result) TYPE string.

    METHODS get_language
      IMPORTING iv_uname         TYPE clike
      RETURNING VALUE(rv_result) TYPE string.

    METHODS can_debug
      IMPORTING iv_uname         TYPE clike
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS can_develop
      IMPORTING iv_uname         TYPE clike
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS get_address
      IMPORTING iv_uname         TYPE clike
      RETURNING VALUE(rs_result) TYPE bapiaddr3.

  PRIVATE SECTION.
    DATA cache TYPE REF TO zcl_xfc_lru_cache_toolkit.
ENDCLASS.


CLASS zcl_xfc_user_toolkit IMPLEMENTATION.
  METHOD constructor.
    cache = io_cache.
  ENDMETHOD.

  METHOD can_debug.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP' FOR USER iv_uname
                    ID 'ACTVT'    FIELD '02'
                    ID 'OBJTYPE'  FIELD 'DEBUG'
                    ID 'DEVCLASS' DUMMY
                    ID 'OBJNAME'  DUMMY
                    ID 'P_GROUP'  DUMMY.
    rv_result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD can_develop.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP' FOR USER iv_uname
                    ID 'ACTVT'    FIELD '02'
                    ID 'OBJTYPE'  DUMMY
                    ID 'DEVCLASS' DUMMY
                    ID 'OBJNAME'  DUMMY
                    ID 'P_GROUP'  DUMMY.
    rv_result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD get_email.
    rv_result = get_address( iv_uname )-e_mail.
  ENDMETHOD.

  METHOD get_full_name.
    rv_result = get_address( iv_uname )-fullname.
  ENDMETHOD.

  METHOD get_language.
    rv_result = get_address( iv_uname )-langu.
  ENDMETHOD.

  METHOD get_mobile_number.
    rv_result = get_address( iv_uname )-tel1_numbr.
  ENDMETHOD.

  METHOD get_address.
    DATA lv_key    TYPE zcl_xfc_lru_cache_toolkit=>ty_key.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2.

    lv_key = |BAPI_USER_GET_DETAIL:{ iv_uname }|.

    cache->get( EXPORTING  iv_key   = lv_key
                IMPORTING  ev_value = rs_result
                EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING username = iv_uname
        IMPORTING address  = rs_result
        TABLES    return   = lt_return.
      cache->put( iv_key   = lv_key
                  iv_value = rs_result ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
