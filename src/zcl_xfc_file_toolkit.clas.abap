CLASS zcl_xfc_file_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES xline TYPE x LENGTH 255.
    TYPES xtabl TYPE STANDARD TABLE OF xline WITH EMPTY KEY.

    METHODS write
      IMPORTING iv_file TYPE string
                iv_data TYPE string.

    METHODS write_x
      IMPORTING iv_file  TYPE string
                iv_datax TYPE xstring.

    METHODS read
      IMPORTING iv_file TYPE string
      EXPORTING ev_data TYPE string.

    METHODS read_x
      IMPORTING iv_file  TYPE string
      EXPORTING ev_datax TYPE xstring.

ENDCLASS.


CLASS zcl_xfc_file_toolkit IMPLEMENTATION.
  METHOD read.
    DATA lt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename = iv_file
                                          CHANGING   data_tab = lt_string
                                          EXCEPTIONS OTHERS   = 1 ).
    ev_data = concat_lines_of( table = lt_string
                               sep   = cl_abap_char_utilities=>newline  ).
  ENDMETHOD.

  METHOD read_x.
    DATA lv_length TYPE i.
    DATA lt_binary TYPE xtabl.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = iv_file
                                                     filetype   = 'BIN'
                                          IMPORTING  filelength = lv_length
                                          CHANGING   data_tab   = lt_binary
                                          EXCEPTIONS OTHERS     = 1 ).

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING  input_length = lv_length
      IMPORTING  buffer       = ev_datax
      TABLES     binary_tab   = lt_binary
      EXCEPTIONS OTHERS       = 1.
  ENDMETHOD.

  METHOD write.
    DATA lx_data TYPE xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING text   = iv_data
      IMPORTING buffer = lx_data.

    write_x( iv_file  = iv_file
             iv_datax = lx_data ).
  ENDMETHOD.

  METHOD write_x.
    DATA lv_size TYPE i.
    DATA lt_strm TYPE xtabl.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING buffer        = iv_datax
      IMPORTING output_length = lv_size
      TABLES    binary_tab    = lt_strm.

    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = lv_size
                                                       filetype     = 'BIN'
                                                       filename     = iv_file
                                            CHANGING   data_tab     = lt_strm
                                            EXCEPTIONS OTHERS       = 1 ).
  ENDMETHOD.
ENDCLASS.
