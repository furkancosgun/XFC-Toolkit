CLASS zcl_xfc_file_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES xline TYPE x LENGTH 255.
    TYPES xtabl TYPE STANDARD TABLE OF xline WITH EMPTY KEY.

    CLASS-METHODS write_to_client
      IMPORTING iv_file TYPE string
                iv_data TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS write_x_to_client
      IMPORTING iv_file  TYPE string
                iv_datax TYPE xstring
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS read_from_client
      IMPORTING iv_file          TYPE string
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS read_x_from_client
      IMPORTING iv_file          TYPE string
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS write_to_server
      IMPORTING iv_file     TYPE string
                iv_data     TYPE string
                iv_encoding TYPE abap_encoding DEFAULT zcl_xfc_conv_toolkit=>mc_default_encoding
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS write_x_to_server
      IMPORTING iv_file  TYPE string
                iv_datax TYPE xstring
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS read_from_server
      IMPORTING iv_file          TYPE string
                iv_encoding      TYPE abap_encoding DEFAULT zcl_xfc_conv_toolkit=>mc_default_encoding
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS read_x_from_server
      IMPORTING iv_file          TYPE string
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   zcx_xfc_toolkit_error.

ENDCLASS.


CLASS zcl_xfc_file_toolkit IMPLEMENTATION.
  METHOD read_from_client.
    DATA lt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename = iv_file
                                          CHANGING   data_tab = lt_string
                                          EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.

    rv_result = concat_lines_of( table = lt_string
                                 sep   = cl_abap_char_utilities=>newline  ).
  ENDMETHOD.

  METHOD read_x_from_client.
    DATA lt_binary TYPE xtabl.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename = iv_file
                                                     filetype = 'BIN'
                                          CHANGING   data_tab = lt_binary
                                          EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
    rv_result = zcl_xfc_conv_toolkit=>binary_to_xstring( lt_binary ).
  ENDMETHOD.

  METHOD write_to_client.
    DATA lx_data TYPE xstring.

    lx_data = zcl_xfc_conv_toolkit=>string_to_xstring( iv_string = iv_data ).

    write_x_to_client( iv_file  = iv_file
                       iv_datax = lx_data ).
  ENDMETHOD.

  METHOD write_x_to_client.
    DATA lv_size TYPE i.
    DATA lt_strm TYPE xtabl.

    zcl_xfc_conv_toolkit=>xstring_to_binary( EXPORTING iv_xstring = iv_datax
                                             IMPORTING et_binary  = lt_strm ).

    lv_size = xstrlen( iv_datax ).

    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = lv_size
                                                       filetype     = 'BIN'
                                                       filename     = iv_file
                                            CHANGING   data_tab     = lt_strm
                                            EXCEPTIONS OTHERS       = 1 ).
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD read_from_server.
    DATA lv_xdata TYPE xstring.

    lv_xdata = read_x_from_server( iv_file = iv_file ).

    rv_result = zcl_xfc_conv_toolkit=>xstring_to_string( iv_xstring  = lv_xdata
                                                         iv_encoding = iv_encoding ).
  ENDMETHOD.

  METHOD read_x_from_server.
    DATA lv_message TYPE string.

    TRY.
        OPEN DATASET iv_file FOR INPUT IN BINARY MODE MESSAGE lv_message.
        IF sy-subrc <> 0.
          zcx_xfc_toolkit_error=>raise( lv_message ).
        ENDIF.
        READ DATASET iv_file INTO rv_result.
        CLOSE DATASET iv_file.
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD write_to_server.
    DATA lv_xdata TYPE xstring.

    lv_xdata = zcl_xfc_conv_toolkit=>string_to_xstring( iv_string   = iv_data
                                                        iv_encoding = iv_encoding ).

    write_x_to_server( iv_file  = iv_file
                       iv_datax = lv_xdata ).
  ENDMETHOD.

  METHOD write_x_to_server.
    DATA lv_message TYPE string.

    TRY.
        OPEN DATASET iv_file FOR OUTPUT IN BINARY MODE MESSAGE lv_message.
        IF sy-subrc <> 0.
          zcx_xfc_toolkit_error=>raise( lv_message ).
        ENDIF.
        TRANSFER iv_datax TO iv_file.
        CLOSE DATASET iv_file.
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
