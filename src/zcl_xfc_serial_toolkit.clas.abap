CLASS zcl_xfc_serial_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS ole2.

    METHODS constructor
      IMPORTING iv_com_port    TYPE i
                iv_settings    TYPE string DEFAULT '9600,N,8,1'
                iv_retry_limit TYPE i      DEFAULT 50
                iv_wait_time   TYPE f      DEFAULT '0.1'
      RAISING   zcx_xfc_toolkit_error.

    METHODS open  RAISING zcx_xfc_toolkit_error.
    METHODS close RAISING zcx_xfc_toolkit_error.

    METHODS is_open
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS is_connected
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS get_buffer_count
      RETURNING VALUE(rv_result) TYPE i.

    METHODS write
      IMPORTING iv_command TYPE string
      RAISING   zcx_xfc_toolkit_error.

    METHODS flush
      RETURNING VALUE(rv_result) TYPE string.

    METHODS read
      IMPORTING iv_buffer_size   TYPE i
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    METHODS read_until
      IMPORTING iv_terminator    TYPE clike
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    METHODS read_line
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    TYPES:
      BEGIN OF ty_port_item,
        port TYPE string,
        name TYPE string,
      END OF ty_port_item.
    TYPES ty_port_items TYPE STANDARD TABLE OF ty_port_item WITH EMPTY KEY.

    CLASS-METHODS list_available_ports
      RETURNING VALUE(rt_result) TYPE ty_port_items
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    DATA mo_mscomm      TYPE ole2_object.
    DATA mv_retry_limit TYPE i           VALUE 50.
    DATA mv_wait_time   TYPE f           VALUE '0.1'.

ENDCLASS.


CLASS zcl_xfc_serial_toolkit IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_mscomm 'MSCOMMLib.MSComm.1'.
    IF sy-subrc <> 0 OR mo_mscomm-handle = 0.
      zcx_xfc_toolkit_error=>raise( 'System Error: MSComm32.ocx is not registered or missing on this PC.' ).
    ENDIF.

    SET PROPERTY OF mo_mscomm 'CommPort'  = iv_com_port.
    SET PROPERTY OF mo_mscomm 'Settings'  = iv_settings.
    SET PROPERTY OF mo_mscomm 'InputLen'  = 1.
    SET PROPERTY OF mo_mscomm 'RTSEnable' = 1.
    SET PROPERTY OF mo_mscomm 'DTREnable' = 1.

    mv_retry_limit = iv_retry_limit.
    mv_wait_time   = iv_wait_time.
  ENDMETHOD.

  METHOD open.
    SET PROPERTY OF mo_mscomm 'PortOpen' = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise(
          'Hardware Error: Could not open COM port. Check cable or if another app is using it.' ).
    ENDIF.
  ENDMETHOD.

  METHOD close.
    SET PROPERTY OF mo_mscomm 'PortOpen' = 0.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise(
          'Hardware Error: Could not close the COM port properly. The connection might be lost.' ).
    ENDIF.
  ENDMETHOD.

  METHOD is_open.
    DATA lv_open TYPE i.

    GET PROPERTY OF mo_mscomm 'PortOpen' = lv_open.
    rv_result = xsdbool( lv_open = 1 ).
  ENDMETHOD.

  METHOD is_connected.
    DATA lv_conn TYPE i.

    GET PROPERTY OF mo_mscomm 'DSRHolding' = lv_conn.
    rv_result = xsdbool( lv_conn = 1 ).
  ENDMETHOD.

  METHOD get_buffer_count.
    GET PROPERTY OF mo_mscomm 'InBufferCount' = rv_result.
  ENDMETHOD.

  METHOD flush.
    SET PROPERTY OF mo_mscomm 'InputLen' = 0.
    GET PROPERTY OF mo_mscomm 'Input'    = rv_result.
    SET PROPERTY OF mo_mscomm 'InputLen' = 1.
  ENDMETHOD.

  METHOD write.
    SET PROPERTY OF mo_mscomm 'Output' = iv_command.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise( 'Write Error: Failed to send data. Is the port still open?' ).
    ENDIF.
  ENDMETHOD.

  METHOD read.
    DATA lv_bytes_ready TYPE i.
    DATA lv_input_chunk TYPE c LENGTH 1.

    DO mv_retry_limit TIMES.
      lv_bytes_ready = get_buffer_count( ).
      IF lv_bytes_ready > 0.
        DO lv_bytes_ready TIMES.
          GET PROPERTY OF mo_mscomm 'Input' = lv_input_chunk.
          CONCATENATE rv_result lv_input_chunk INTO rv_result IN CHARACTER MODE RESPECTING BLANKS.
          IF strlen( rv_result ) >= iv_buffer_size.
            RETURN.
          ENDIF.
        ENDDO.
      ENDIF.
      WAIT UP TO mv_wait_time SECONDS.
    ENDDO.

    zcx_xfc_toolkit_error=>raise(
        |Read Timeout: Data incomplete or not received within { mv_retry_limit * mv_wait_time }s.| ).
  ENDMETHOD.

  METHOD read_until.
    DATA lv_term_len    TYPE i.
    DATA lv_bytes_ready TYPE i.
    DATA lv_input_chunk TYPE c LENGTH 1.

    lv_term_len = strlen( iv_terminator ).

    DO mv_retry_limit TIMES.
      lv_bytes_ready = get_buffer_count( ).
      IF lv_bytes_ready > 0.
        DO lv_bytes_ready TIMES.
          GET PROPERTY OF mo_mscomm 'Input' = lv_input_chunk.
          CONCATENATE rv_result lv_input_chunk INTO rv_result IN CHARACTER MODE RESPECTING BLANKS.
          IF strlen( rv_result ) >= lv_term_len.
            IF substring( val = rv_result
                          off = strlen( rv_result ) - lv_term_len
                          len = lv_term_len ) = iv_terminator.
              RETURN.
            ENDIF.
          ENDIF.
        ENDDO.
      ENDIF.
      WAIT UP TO mv_wait_time SECONDS.
    ENDDO.

    zcx_xfc_toolkit_error=>raise( |Read Timeout: Expected terminator not found. Received so far: [{ rv_result }]| ).
  ENDMETHOD.

  METHOD read_line.
    rv_result = read_until( cl_abap_char_utilities=>cr_lf ).
  ENDMETHOD.

  METHOD list_available_ports.
    DATA lo_locator   TYPE ole2_object.
    DATA lo_service   TYPE ole2_object.
    DATA lo_objects   TYPE ole2_object.
    DATA lv_count     TYPE i.
    DATA lv_index     TYPE i.
    DATA lo_object    TYPE ole2_object.
    DATA lv_port_id   TYPE string.
    DATA lv_port_name TYPE string.

    CREATE OBJECT lo_locator 'WbemScripting.SWbemLocator'.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise( 'WMI Locator could not be created.' ).
    ENDIF.

    CALL METHOD OF lo_locator 'ConnectServer' = lo_service.

    CALL METHOD OF lo_service 'ExecQuery' = lo_objects
      EXPORTING #1 = 'SELECT DeviceID, Name FROM Win32_SerialPort'.

    GET PROPERTY OF lo_objects 'Count' = lv_count.

    DO lv_count TIMES.
      lv_index = sy-index - 1.
      CALL METHOD OF lo_objects 'ItemIndex' = lo_object
        EXPORTING #1 = lv_index.

      GET PROPERTY OF lo_object 'DeviceID' = lv_port_id.
      IF sy-subrc <> 0 OR lv_port_id NS 'COM'.
        CONTINUE.
      ENDIF.

      GET PROPERTY OF lo_object 'Name' = lv_port_name.
      IF sy-subrc <> 0.
        lv_port_name = lv_port_id.
      ENDIF.

      APPEND VALUE #( port = lv_port_id
                      name = lv_port_name ) TO rt_result.

      FREE OBJECT lo_object.

      CLEAR lv_port_id.
      CLEAR lv_port_name.
    ENDDO.

    FREE OBJECT lo_objects.
    FREE OBJECT lo_service.
    FREE OBJECT lo_locator.
  ENDMETHOD.
ENDCLASS.
