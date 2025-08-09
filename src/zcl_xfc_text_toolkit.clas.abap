CLASS zcl_xfc_text_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS read_text
      IMPORTING iv_client        TYPE sy-mandt      DEFAULT sy-mandt
                iv_id            TYPE thead-tdid
                iv_language      TYPE thead-tdspras DEFAULT sy-langu
                iv_name          TYPE thead-tdname
                iv_object        TYPE thead-tdobject
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS save_text
      IMPORTING iv_client        TYPE sy-mandt DEFAULT sy-mandt
                iv_id            TYPE thead-tdid
                iv_language      TYPE thead-tdspras DEFAULT sy-langu
                iv_name          TYPE thead-tdname
                iv_object        TYPE thead-tdobject
                iv_text          TYPE string
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_xfc_toolkit_error.

ENDCLASS.


CLASS zcl_xfc_text_toolkit IMPLEMENTATION.
  METHOD read_text.
    DATA lt_lines TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.

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

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>).
      IF sy-tabix = 1.
        rv_result = <fs_line>-tdline.
      ELSE.
        rv_result = |{ rv_result } { <fs_line>-tdline }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_text.
    DATA ls_header TYPE thead.
    DATA lt_texts  TYPE TABLE OF tline-tdline WITH EMPTY KEY.
    DATA lt_lines  TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.

    ls_header-mandt    = iv_client.
    ls_header-tdid     = iv_id.
    ls_header-tdname   = iv_name.
    ls_header-tdobject = iv_object.
    ls_header-tdspras  = iv_language.

    zcl_xfc_conv_toolkit=>string_to_ftext( EXPORTING iv_string = iv_text
                                           IMPORTING et_ftext  = lt_texts ).

    LOOP AT lt_texts ASSIGNING FIELD-SYMBOL(<fs_text>).
      APPEND VALUE #( tdformat = '*'
                      tdline   = <fs_text> ) TO lt_lines.
    ENDLOOP.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = iv_client
        header          = ls_header
        savemode_direct = abap_true
      TABLES
        lines           = lt_lines
      EXCEPTIONS
        OTHERS          = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.

    rv_result = abap_true.
  ENDMETHOD.
ENDCLASS.
