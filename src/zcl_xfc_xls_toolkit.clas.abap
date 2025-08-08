CLASS zcl_xfc_xls_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS export
      IMPORTING VALUE(it_data)   TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE xstring
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS import
      IMPORTING iv_xdocument   TYPE xstring
      EXPORTING VALUE(et_data) TYPE STANDARD TABLE
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    CLASS-METHODS get_structdescr
      IMPORTING it_data            TYPE STANDARD TABLE
      RETURNING VALUE(ro_instance) TYPE REF TO cl_abap_structdescr.

    CLASS-METHODS convert_structure
      IMPORTING VALUE(is_source) TYPE any
                io_structdescr   TYPE REF TO cl_abap_structdescr
      EXPORTING VALUE(es_target) TYPE any.
ENDCLASS.


CLASS zcl_xfc_xls_toolkit IMPLEMENTATION.
  METHOD export.
    DATA lo_structdescr   TYPE REF TO cl_abap_structdescr.
    DATA lo_exporter      TYPE REF TO cl_salv_export_tool_xls.
    DATA lo_configuration TYPE REF TO if_salv_export_configuration.
    DATA lo_datadescr     TYPE REF TO cl_abap_datadescr.
    DATA lo_elemdescr     TYPE REF TO cl_abap_elemdescr.
    DATA ls_ddicfield     TYPE dfies.

    TRY.
        lo_structdescr = get_structdescr( it_data ).
        lo_exporter = cl_salv_export_tool=>create_for_excel( REF #( it_data ) ).
        lo_configuration = lo_exporter->configuration( ).

        LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_component>).
          lo_datadescr = lo_structdescr->get_component_type( <fs_component>-name ).
          IF lo_datadescr->kind <> cl_abap_typedescr=>kind_elem.
            CONTINUE.
          ENDIF.
          lo_elemdescr ?= lo_datadescr.

          lo_elemdescr->get_ddic_field( RECEIVING  p_flddescr = ls_ddicfield
                                        EXCEPTIONS OTHERS     = 1 ).
          IF sy-subrc <> 0.
            ls_ddicfield-fieldtext = <fs_component>-name.
          ENDIF.
          lo_configuration->add_column( header_text  = CONV #( ls_ddicfield-fieldtext )
                                        field_name   = CONV #( <fs_component>-name )
                                        display_type = if_salv_export_column_conf=>display_types-text_view ).
        ENDLOOP.

        lo_exporter->read_result( IMPORTING content = rv_result ).
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD import.
    DATA lo_spreadsheet TYPE REF TO if_fdt_doc_spreadsheet.
    DATA lt_worksheet   TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.
    DATA lr_data        TYPE REF TO data.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.

    TRY.
        lo_structdescr = get_structdescr( et_data ).

        lo_spreadsheet = NEW cl_fdt_xl_spreadsheet( document_name = ''
                                                    xdocument     = iv_xdocument ).

        lo_spreadsheet->get_worksheet_names( IMPORTING worksheet_names = lt_worksheet ).
        lr_data = lo_spreadsheet->get_itab_from_worksheet( worksheet_name = VALUE #( lt_worksheet[ 1 ] OPTIONAL ) ).
        ASSIGN lr_data->* TO <fs_table>.

        LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line>) FROM 2.
          APPEND INITIAL LINE TO et_data ASSIGNING FIELD-SYMBOL(<fs_data>).
          convert_structure( EXPORTING is_source      = <fs_line>
                                       io_structdescr = lo_structdescr
                             IMPORTING es_target      = <fs_data> ).
        ENDLOOP.
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_structdescr.
    ro_instance ?= CAST cl_abap_tabledescr(
        cl_abap_typedescr=>describe_by_data( it_data )
    )->get_table_line_type( ).
  ENDMETHOD.

  METHOD convert_structure.
    DATA lo_datadescr TYPE REF TO cl_abap_datadescr.
    DATA lo_elemdescr TYPE REF TO cl_abap_elemdescr.
    DATA lv_funcname  TYPE funcname.
    DATA lv_position  TYPE sy-tabix.

    LOOP AT io_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_component>).
      lv_position = sy-tabix.
      lo_datadescr = io_structdescr->get_component_type( <fs_component>-name ).
      IF lo_datadescr->kind <> cl_abap_typedescr=>kind_elem.
        CONTINUE.
      ENDIF.

      lo_elemdescr ?= lo_datadescr.

      ASSIGN COMPONENT lv_position OF STRUCTURE is_source TO FIELD-SYMBOL(<fs_source>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lv_position OF STRUCTURE es_target TO FIELD-SYMBOL(<fs_target>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE lo_datadescr->type_kind.
        WHEN cl_abap_typedescr=>typekind_packed
          OR cl_abap_typedescr=>typekind_decfloat
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34.
          TRY.
              <fs_target> = <fs_source>.
            CATCH cx_root.
              IF <fs_source> CA ',.'.
                REPLACE ALL OCCURRENCES OF '.' IN <fs_source> WITH ''.
                REPLACE ALL OCCURRENCES OF ',' IN <fs_source> WITH '.'.
              ENDIF.
              <fs_target> = <fs_source>.
          ENDTRY.
        WHEN cl_abap_typedescr=>typekind_date.
          IF <fs_source> CA '-'.
            REPLACE ALL OCCURRENCES OF '-' IN <fs_source> WITH ''.
          ENDIF.
          CONDENSE <fs_source> NO-GAPS.
          <fs_target> = <fs_source>.
        WHEN cl_abap_typedescr=>typekind_time.
          IF <fs_source> CA ':'.
            REPLACE ALL OCCURRENCES OF ':' IN <fs_source> WITH ''.
          ENDIF.
          CONDENSE <fs_source> NO-GAPS.
          <fs_target> = <fs_source>.
        WHEN OTHERS.
          TRY.
              lv_funcname = lo_elemdescr->edit_mask.
              IF lv_funcname IS NOT INITIAL.
                REPLACE '==' IN lv_funcname WITH ''.
                CONDENSE lv_funcname NO-GAPS.
                lv_funcname = |CONVERSION_EXIT_{ lv_funcname }_INPUT|.
                CALL FUNCTION lv_funcname
                  EXPORTING
                    input  = <fs_source>
                  IMPORTING
                    output = <fs_target>.
              ELSE.
                <fs_target> = <fs_source>.
              ENDIF.
            CATCH cx_sy_move_cast_error.
              <fs_target> = <fs_source>.
          ENDTRY.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
