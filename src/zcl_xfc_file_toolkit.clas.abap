CLASS zcl_xfc_file_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES xline TYPE x LENGTH 255.
    TYPES xtabl TYPE STANDARD TABLE OF xline WITH EMPTY KEY.

    CLASS-METHODS write
      IMPORTING iv_file TYPE string
                iv_data TYPE string.

    CLASS-METHODS write_x
      IMPORTING iv_file  TYPE string
                iv_datax TYPE xstring.

    CLASS-METHODS read
      IMPORTING iv_file          TYPE string
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS read_x
      IMPORTING iv_file          TYPE string
      RETURNING VALUE(rv_result) TYPE xstring.

ENDCLASS.


CLASS zcl_xfc_file_toolkit IMPLEMENTATION.
  METHOD read.
    DATA lt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename = iv_file
                                          CHANGING   data_tab = lt_string
                                          EXCEPTIONS OTHERS   = 1 ).

    rv_result = concat_lines_of( table = lt_string
                                 sep   = cl_abap_char_utilities=>newline  ).
  ENDMETHOD.

  METHOD read_x.
    DATA lt_binary TYPE xtabl.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename = iv_file
                                                     filetype = 'BIN'
                                          CHANGING   data_tab = lt_binary
                                          EXCEPTIONS OTHERS   = 1 ).

    rv_result = zcl_xfc_conv_toolkit=>binary_2_xstring( lt_binary ).
  ENDMETHOD.

  METHOD write.
    DATA lx_data TYPE xstring.

    lx_data = zcl_xfc_conv_toolkit=>string_2_xstring( iv_string = iv_data ).

    write_x( iv_file  = iv_file
             iv_datax = lx_data ).
  ENDMETHOD.

  METHOD write_x.
    DATA lv_size TYPE i.
    DATA lt_strm TYPE xtabl.

    zcl_xfc_conv_toolkit=>xstring_2_binary( EXPORTING iv_xstring = iv_datax
                                            IMPORTING et_binary  = lt_strm ).

    lv_size = xstrlen( iv_datax ).

    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = lv_size
                                                       filetype     = 'BIN'
                                                       filename     = iv_file
                                            CHANGING   data_tab     = lt_strm
                                            EXCEPTIONS OTHERS       = 1 ).
  ENDMETHOD.
ENDCLASS.
