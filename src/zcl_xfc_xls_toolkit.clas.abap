CLASS zcl_xfc_xls_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS export
      IMPORTING VALUE(it_data) TYPE STANDARD TABLE
      EXPORTING ev_xdata       TYPE xstring
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS import
      IMPORTING iv_xdata       TYPE xstring
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

        lo_exporter->read_result( IMPORTING content = ev_xdata ).
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
                                                    xdocument     = iv_xdata ).

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

      zcl_xfc_conv_toolkit=>convert_ext_2_int( EXPORTING iv_source    = <fs_source>
                                                           io_elemdescr = lo_elemdescr
                                                 IMPORTING ev_target    = <fs_target> ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
