CLASS zcl_xfc_serial_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS ole2.

    CONSTANTS mc_retry_limit TYPE i VALUE 50.
    CONSTANTS mc_wait_time   TYPE f VALUE '0.1'.

    CLASS-METHODS read
      IMPORTING iv_com_port      TYPE i
                iv_settings      TYPE string DEFAULT '9600,N,8,1'
                iv_buffer_size   TYPE i
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS write
      IMPORTING iv_com_port TYPE i
                iv_settings TYPE string DEFAULT '9600,N,8,1'
                iv_command  TYPE string
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    CLASS-METHODS get_mscomm_instance
      RETURNING VALUE(ro_mscomm) TYPE ole2_object
      RAISING   zcx_xfc_toolkit_error.
ENDCLASS.


CLASS zcl_xfc_serial_toolkit IMPLEMENTATION.
  METHOD read.
    DATA lo_mscomm      TYPE ole2_object.
    DATA lv_input_chunk TYPE string.
    DATA lv_bytes_ready TYPE i.

    lo_mscomm = get_mscomm_instance( ).

    SET PROPERTY OF lo_mscomm 'CommPort' = iv_com_port.
    SET PROPERTY OF lo_mscomm 'Settings' = iv_settings.
    SET PROPERTY OF lo_mscomm 'InputLen' = 0.
    SET PROPERTY OF lo_mscomm 'RTSEnable' = 1.
    SET PROPERTY OF lo_mscomm 'DTREnable' = 1.

    SET PROPERTY OF lo_mscomm 'PortOpen' = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise( |Could not open COM port { iv_com_port }. Check if it is already in use.| ).
    ENDIF.

    DO mc_retry_limit TIMES.
      GET PROPERTY OF lo_mscomm 'InBufferCount' = lv_bytes_ready.
      IF lv_bytes_ready >= iv_buffer_size.
        EXIT.
      ENDIF.
      WAIT UP TO mc_wait_time SECONDS.
    ENDDO.

    DO mc_retry_limit TIMES.
      GET PROPERTY OF lo_mscomm 'Input' = lv_input_chunk.
      rv_result = |{ rv_result }{ lv_input_chunk }|.
      IF strlen( rv_result ) >= iv_buffer_size.
        EXIT.
      ENDIF.
      WAIT UP TO mc_wait_time SECONDS.
    ENDDO.

    SET PROPERTY OF lo_mscomm 'PortOpen' = 0.

    IF rv_result IS INITIAL.
      zcx_xfc_toolkit_error=>raise( 'Timeout: No data received from the device.' ).
    ENDIF.
  ENDMETHOD.

  METHOD write.
    DATA lo_mscomm TYPE ole2_object.

    lo_mscomm = get_mscomm_instance( ).

    SET PROPERTY OF lo_mscomm 'CommPort' = iv_com_port.
    SET PROPERTY OF lo_mscomm 'Settings' = iv_settings.
    SET PROPERTY OF lo_mscomm 'RTSEnable' = 1.
    SET PROPERTY OF lo_mscomm 'DTREnable' = 1.

    SET PROPERTY OF lo_mscomm 'PortOpen' = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise( |Could not open COM port { iv_com_port } for writing.| ).
    ENDIF.

    SET PROPERTY OF lo_mscomm 'Output' = iv_command.

    WAIT UP TO mc_wait_time SECONDS.

    SET PROPERTY OF lo_mscomm 'PortOpen' = 0.
  ENDMETHOD.

  METHOD get_mscomm_instance.
    CREATE OBJECT ro_mscomm 'MSCOMMLib.MSComm.1'.
    IF sy-subrc <> 0 OR ro_mscomm-handle = 0.
      zcx_xfc_toolkit_error=>raise( 'ActiveX component MSComm could not be created. Please check your installation.' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.