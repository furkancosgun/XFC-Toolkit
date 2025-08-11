CLASS zcl_xfc_ftp_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_text_result TYPE c LENGTH 1024.
    TYPES tt_text_result TYPE STANDARD TABLE OF ty_text_result WITH EMPTY KEY.

    TYPES ty_blob_result TYPE x LENGTH 255.
    TYPES tt_blob_result TYPE STANDARD TABLE OF ty_blob_result WITH EMPTY KEY.

    CLASS-METHODS create
      IMPORTING iv_user            TYPE c
                iv_pswd            TYPE c
                iv_host            TYPE c
                iv_dest            TYPE c DEFAULT 'SAPFTPA'
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_ftp_toolkit.

    METHODS command
      IMPORTING iv_command       TYPE c
                iv_compress      TYPE c OPTIONAL
      RETURNING VALUE(rt_result) TYPE tt_text_result
      RAISING   zcx_xfc_toolkit_error.

    METHODS connect
      RAISING zcx_xfc_toolkit_error.

    METHODS disconnect.

    METHODS download_file
      IMPORTING iv_file TYPE c
      EXPORTING et_text TYPE tt_text_result
                et_blob TYPE tt_blob_result
      RAISING   zcx_xfc_toolkit_error.

    METHODS upload_file
      IMPORTING iv_file    TYPE c
                iv_xstring TYPE xstring
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    DATA user TYPE e_dexcommusername.
    DATA pswd TYPE wt_authval.
    DATA host TYPE /isdfps/hostname.
    DATA dest TYPE rfcdes-rfcdest.
    DATA hndl TYPE i.

    METHODS constructor
      IMPORTING  iv_user TYPE c
                 iv_pswd TYPE c
                 iv_host TYPE c
                 iv_dest TYPE c
      EXCEPTIONS not_connected.

    METHODS scramble_password
      IMPORTING iv_pswd          TYPE c
      RETURNING VALUE(rv_result) TYPE wt_authval.
ENDCLASS.


CLASS zcl_xfc_ftp_toolkit IMPLEMENTATION.
  METHOD create.
    ro_instance = NEW zcl_xfc_ftp_toolkit( iv_user = iv_user
                                           iv_pswd = iv_pswd
                                           iv_host = iv_host
                                           iv_dest = iv_dest ).
  ENDMETHOD.

  METHOD constructor.
    user = iv_user.
    pswd = scramble_password( iv_pswd ).
    host = iv_host.
    dest = iv_dest.
  ENDMETHOD.

  METHOD connect.
    CALL FUNCTION 'FTP_CONNECT'
      EXPORTING
        user            = user
        password        = pswd
        host            = host
        rfc_destination = dest
      IMPORTING
        handle          = hndl
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD disconnect.
    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        handle = hndl.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination = dest
      EXCEPTIONS
        OTHERS      = 1.
  ENDMETHOD.

  METHOD command.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle   = hndl
        command  = iv_command
        compress = iv_compress
      TABLES
        data     = rt_result
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD scramble_password.
    DATA lv_len TYPE i.

    lv_len = strlen( iv_pswd ).
    CALL FUNCTION 'HTTP_SCRAMBLE'
      EXPORTING
        source      = iv_pswd
        sourcelen   = lv_len
        key         = 26101957
      IMPORTING
        destination = rv_result.
  ENDMETHOD.

  METHOD download_file.
    IF et_text IS NOT REQUESTED AND et_blob IS NOT REQUESTED.
      RETURN.
    ENDIF.

    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle = hndl
        fname  = iv_file
      TABLES
        text   = et_text
        blob   = et_blob
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD upload_file.
    DATA lt_blob TYPE tt_blob_result.
    DATA lv_len  TYPE i.

    lv_len = xstrlen( iv_xstring ).

    zcl_xfc_conv_toolkit=>xstring_to_binary( EXPORTING iv_xstring = iv_xstring
                                             IMPORTING et_binary  = lt_blob ).
    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle      = hndl
        fname       = iv_file
        blob_length = lv_len
      TABLES
        blob        = lt_blob
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
