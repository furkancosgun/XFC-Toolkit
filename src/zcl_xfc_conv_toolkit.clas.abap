CLASS zcl_xfc_conv_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS mc_default_encoding TYPE abap_encoding VALUE 'DEFAULT'.

    CLASS-METHODS convert_ext_to_int
      IMPORTING VALUE(iv_source) TYPE any
                io_elemdescr     TYPE REF TO cl_abap_elemdescr OPTIONAL
      EXPORTING ev_target        TYPE any.

    CLASS-METHODS convert_int_to_ext
      IMPORTING iv_source        TYPE any
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS binary_to_xstring
      IMPORTING it_binary        TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE xstring.

    CLASS-METHODS xstring_to_binary
      IMPORTING iv_xstring TYPE xstring
      EXPORTING et_binary  TYPE STANDARD TABLE.

    CLASS-METHODS xstring_to_string
      IMPORTING iv_xstring       TYPE xstring
                iv_encoding      TYPE abap_encoding DEFAULT mc_default_encoding
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS string_to_xstring
      IMPORTING iv_string        TYPE string
                iv_encoding      TYPE abap_encoding DEFAULT mc_default_encoding
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS string_to_ftext
      IMPORTING iv_string TYPE string
      EXPORTING et_ftext  TYPE STANDARD TABLE.

    CLASS-METHODS ftext_to_string
      IMPORTING it_ftext         TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.


CLASS zcl_xfc_conv_toolkit IMPLEMENTATION.
  METHOD convert_int_to_ext.
    DATA lv_output TYPE c LENGTH 1024.

    WRITE iv_source TO lv_output LEFT-JUSTIFIED.

    rv_result = lv_output.
  ENDMETHOD.

  METHOD convert_ext_to_int.
    DATA lo_elemdescr TYPE REF TO cl_abap_elemdescr.
    DATA lv_funcname  TYPE funcname.

    CHECK iv_source IS NOT INITIAL.

    IF io_elemdescr IS BOUND.
      lo_elemdescr = io_elemdescr.
    ELSE.
      lo_elemdescr ?= cl_abap_typedescr=>describe_by_data( ev_target ).
    ENDIF.

    TRY.
        CASE lo_elemdescr->type_kind.
          WHEN cl_abap_typedescr=>typekind_packed
            OR cl_abap_typedescr=>typekind_decfloat
            OR cl_abap_typedescr=>typekind_decfloat16
            OR cl_abap_typedescr=>typekind_decfloat34.
            TRY.
                ev_target = iv_source.
              CATCH cx_root.
                IF iv_source CA ',.'.
                  REPLACE ALL OCCURRENCES OF '.' IN iv_source WITH ''.
                  REPLACE ALL OCCURRENCES OF ',' IN iv_source WITH '.'.
                ENDIF.
                ev_target = iv_source.
            ENDTRY.
          WHEN cl_abap_typedescr=>typekind_date.
            DO 6 TIMES.
              TRY.
                  cl_abap_datfm=>conv_date_ext_to_int( EXPORTING im_datext   = iv_source
                                                                 im_datfmdes = CONV #( sy-index )
                                                       IMPORTING ex_datint   = ev_target ).
                  EXIT.
                CATCH cx_root.
                  CONTINUE.
              ENDTRY.
            ENDDO.
          WHEN cl_abap_typedescr=>typekind_time.
            cl_abap_timefm=>conv_time_ext_to_int( EXPORTING time_ext = iv_source
                                                  IMPORTING time_int = ev_target ).
          WHEN OTHERS.
            lv_funcname = lo_elemdescr->edit_mask.
            IF lv_funcname IS NOT INITIAL.
              REPLACE '==' IN lv_funcname WITH ''.
              CONDENSE lv_funcname NO-GAPS.
              lv_funcname = |CONVERSION_EXIT_{ lv_funcname }_INPUT|.
              CALL FUNCTION lv_funcname
                EXPORTING input  = iv_source
                IMPORTING output = ev_target.
            ELSE.
              ev_target = iv_source.
            ENDIF.
        ENDCASE.
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( |Conversion error:{ lx_root->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD binary_to_xstring.
    FIELD-SYMBOLS <fs_binary> TYPE x.

    LOOP AT it_binary ASSIGNING FIELD-SYMBOL(<fs_line>).
      ASSIGN COMPONENT 1 OF STRUCTURE <fs_line> TO <fs_binary>.
      IF sy-subrc <> 0.
        ASSIGN COMPONENT 0 OF STRUCTURE <fs_line> TO <fs_binary>.
      ENDIF.
      CONCATENATE rv_result <fs_binary> INTO rv_result IN BYTE MODE.
    ENDLOOP.
  ENDMETHOD.

  METHOD xstring_to_binary.
    DATA lv_len  TYPE i.
    DATA lv_pos  TYPE i.
    DATA lv_size TYPE i.

    lv_pos = 0.
    lv_len = xstrlen( iv_xstring ).

    WHILE lv_len > lv_pos.
      APPEND INITIAL LINE TO et_binary ASSIGNING FIELD-SYMBOL(<fs_binary>).
      IF lv_size IS INITIAL.
        DESCRIBE FIELD <fs_binary> LENGTH lv_size IN BYTE MODE.
      ENDIF.
      <fs_binary> = iv_xstring+lv_pos.
      lv_pos      = lv_pos + lv_size.
    ENDWHILE.
  ENDMETHOD.

  METHOD xstring_to_string.
    DATA lo_converter TYPE REF TO cl_abap_conv_in_ce.

    TRY.
        lo_converter = cl_abap_conv_in_ce=>create( encoding = iv_encoding
                                                   input    = iv_xstring ).
        lo_converter->read( IMPORTING data = rv_result ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD string_to_xstring.
    DATA lo_converter TYPE REF TO cl_abap_conv_out_ce.

    TRY.
        lo_converter = cl_abap_conv_out_ce=>create( encoding    = iv_encoding
                                                    ignore_cerr = abap_true ).
        lo_converter->write( iv_string ).
        rv_result = lo_converter->get_buffer( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD string_to_ftext.
    DATA lv_len  TYPE i.
    DATA lv_pos  TYPE i.
    DATA lv_size TYPE i.

    lv_pos = 0.
    lv_len = strlen( iv_string ).

    WHILE lv_len > lv_pos.
      APPEND INITIAL LINE TO et_ftext ASSIGNING FIELD-SYMBOL(<fs_ftext>).
      IF lv_size IS INITIAL.
        DESCRIBE FIELD <fs_ftext> LENGTH lv_size IN CHARACTER MODE.
      ENDIF.
      <fs_ftext> = iv_string+lv_pos.
      lv_pos = lv_pos + lv_size.
    ENDWHILE.
  ENDMETHOD.

  METHOD ftext_to_string.
    FIELD-SYMBOLS <fs_ftext> TYPE c.

    LOOP AT it_ftext ASSIGNING FIELD-SYMBOL(<fs_line>).
      ASSIGN COMPONENT 1 OF STRUCTURE <fs_line> TO <fs_ftext>.
      IF sy-subrc <> 0.
        ASSIGN COMPONENT 0 OF STRUCTURE <fs_line> TO <fs_ftext>.
      ENDIF.
      CONCATENATE rv_result <fs_ftext> INTO rv_result RESPECTING BLANKS.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
