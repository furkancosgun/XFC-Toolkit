CLASS zcl_xfc_tsv_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS export
      IMPORTING VALUE(it_data) TYPE STANDARD TABLE
      EXPORTING ev_data        TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS import
      IMPORTING iv_data        TYPE string
      EXPORTING VALUE(et_data) TYPE STANDARD TABLE
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    CLASS-METHODS get_structdescr
      IMPORTING it_data            TYPE STANDARD TABLE
      RETURNING VALUE(ro_instance) TYPE REF TO cl_abap_structdescr.

    CLASS-METHODS build_header
      IMPORTING io_structdescr   TYPE REF TO cl_abap_structdescr

      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS build_item
      IMPORTING is_data          TYPE any
                io_structdescr   TYPE REF TO cl_abap_structdescr
      RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.


CLASS zcl_xfc_tsv_toolkit IMPLEMENTATION.
  METHOD export.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_content     TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    TRY.
        lo_structdescr = get_structdescr( it_data ).

        APPEND build_header( io_structdescr = lo_structdescr ) TO lt_content.

        LOOP AT it_data ASSIGNING FIELD-SYMBOL(<fs_data>).
          APPEND build_item( is_data        = <fs_data>
                             io_structdescr = lo_structdescr ) TO lt_content.
        ENDLOOP.
        ev_data = concat_lines_of( table = lt_content
                                   sep   = cl_abap_char_utilities=>newline ).
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD import.
    DATA lt_content     TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lt_fields      TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lo_datadescr   TYPE REF TO cl_abap_datadescr.
    DATA lo_elemdescr   TYPE REF TO cl_abap_elemdescr.

    TRY.
        SPLIT iv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_content.
        lo_structdescr = get_structdescr( et_data ).

        LOOP AT lt_content ASSIGNING FIELD-SYMBOL(<fs_content>) FROM 2.
          APPEND INITIAL LINE TO et_data ASSIGNING FIELD-SYMBOL(<fs_data>).

          SPLIT <fs_content> AT cl_abap_char_utilities=>horizontal_tab INTO TABLE lt_fields.

          LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_component>).
            DATA(lv_index) = sy-tabix.

            lo_datadescr = lo_structdescr->get_component_type( <fs_component>-name ).
            IF lo_datadescr->kind <> cl_abap_typedescr=>kind_elem.
              CONTINUE.
            ENDIF.

            lo_elemdescr ?= lo_datadescr.

            ASSIGN lt_fields[ lv_index ] TO FIELD-SYMBOL(<fs_source>).
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            ASSIGN COMPONENT lv_index OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_target>).
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            zcl_xfc_conv_toolkit=>convert_ext_to_int( EXPORTING iv_source    = <fs_source>
                                                                io_elemdescr = lo_elemdescr
                                                      IMPORTING ev_target    = <fs_target> ).
          ENDLOOP.
        ENDLOOP.

      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD build_header.
    DATA ls_ddicfield TYPE dfies.
    DATA lo_elemdescr TYPE REF TO cl_abap_elemdescr.

    LOOP AT io_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_component>).
      DATA(lo_datadescr) = io_structdescr->get_component_type( <fs_component>-name ).
      IF lo_datadescr->kind <> cl_abap_typedescr=>kind_elem.
        CONTINUE.
      ENDIF.

      lo_elemdescr ?= lo_datadescr.

      lo_elemdescr->get_ddic_field( RECEIVING  p_flddescr = ls_ddicfield
                                    EXCEPTIONS OTHERS     = 1 ).
      IF sy-subrc <> 0.
        ls_ddicfield-fieldtext = <fs_component>-name.
      ENDIF.

      IF rv_result IS INITIAL.
        rv_result = ls_ddicfield-fieldtext.
      ELSE.
        CONCATENATE rv_result ls_ddicfield-fieldtext
                    INTO rv_result
                    SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_item.
    DATA lo_datadescr TYPE REF TO cl_abap_datadescr.

    LOOP AT io_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_component>).
      lo_datadescr = io_structdescr->get_component_type( <fs_component>-name ).
      IF lo_datadescr->kind <> cl_abap_typedescr=>kind_elem.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <fs_component>-name OF STRUCTURE is_data TO FIELD-SYMBOL(<fs_data>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF rv_result IS INITIAL.
        rv_result = zcl_xfc_conv_toolkit=>convert_int_to_ext( iv_source = <fs_data> ).
      ELSE.
        rv_result = |{ rv_result }{ cl_abap_char_utilities=>horizontal_tab }{ zcl_xfc_conv_toolkit=>convert_int_to_ext(
                                                                                  <fs_data> ) }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_structdescr.
    ro_instance ?= CAST cl_abap_tabledescr(
        cl_abap_typedescr=>describe_by_data( it_data )
    )->get_table_line_type( ).
  ENDMETHOD.
ENDCLASS.
