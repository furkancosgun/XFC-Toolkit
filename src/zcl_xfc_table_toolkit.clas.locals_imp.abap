*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_html_converter DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_table_id    TYPE string OPTIONAL

                iv_table_class TYPE string OPTIONAL
                iv_table_style TYPE string OPTIONAL

                iv_tr_class    TYPE string OPTIONAL
                iv_tr_style    TYPE string OPTIONAL

                iv_th_class    TYPE string OPTIONAL
                iv_th_style    TYPE string OPTIONAL

                iv_td_class    TYPE string OPTIONAL
                iv_td_style    TYPE string OPTIONAL.

    METHODS convert
      IMPORTING it_data          TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_table_description,
        name TYPE string,
        text TYPE string,
        elem TYPE REF TO cl_abap_elemdescr,
      END OF ty_table_description,
      tt_table_description TYPE HASHED TABLE OF ty_table_description WITH UNIQUE KEY name.

    DATA cell_padding TYPE i.
    DATA cell_spacing TYPE i.

    DATA table_class  TYPE string.
    DATA table_id     TYPE string.
    DATA table_style  TYPE string.

    DATA tr_class     TYPE string.
    DATA tr_style     TYPE string.

    DATA th_class     TYPE string.
    DATA th_style     TYPE string.

    DATA td_class     TYPE string.
    DATA td_style     TYPE string.

    METHODS get_description
      IMPORTING it_data          TYPE STANDARD TABLE
      RETURNING VALUE(rt_result) TYPE tt_table_description.

    METHODS build_header
      IMPORTING it_description   TYPE tt_table_description
      RETURNING VALUE(rv_result) TYPE string.

    METHODS build_table
      RETURNING VALUE(rv_result) TYPE string.

    METHODS build_tr
      RETURNING VALUE(rv_result) TYPE string.

    METHODS build_th
      RETURNING VALUE(rv_result) TYPE string.

    METHODS build_td
      RETURNING VALUE(rv_result) TYPE string.

    METHODS build_footer
      RETURNING VALUE(rv_result) TYPE string.

    METHODS to_string
      IMPORTING iv_value         TYPE any
                io_elemdescr     TYPE REF TO cl_abap_elemdescr
      RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.


CLASS lcl_html_converter IMPLEMENTATION.
  METHOD constructor.
    table_id = iv_table_id.

    table_class = iv_table_class.
    table_style = iv_table_style.

    tr_class = iv_tr_class.
    tr_style = iv_tr_style.

    th_class = iv_th_class.
    th_style = iv_th_style.

    td_class = iv_td_class.
    td_style = iv_td_style.
  ENDMETHOD.

  METHOD convert.
    DATA lt_description TYPE tt_table_description.
    DATA lv_tr_tag      TYPE string.
    DATA lv_td_tag      TYPE string.
    DATA lv_row         TYPE string.

    lt_description = get_description( it_data ).
    lv_tr_tag = build_tr( ).
    lv_td_tag = build_td( ).

    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      CLEAR lv_row.
      LOOP AT lt_description ASSIGNING FIELD-SYMBOL(<fs_description>).
        ASSIGN COMPONENT <fs_description>-name OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_value>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        lv_row = |{ lv_row }\n <td{ lv_td_tag }>{ to_string( iv_value     = <fs_value>
                                                             io_elemdescr = <fs_description>-elem ) }</td>|.
      ENDLOOP.
      rv_result = |{ rv_result }\n <tr{ lv_tr_tag }>{ lv_row }\n </tr>|.
    ENDLOOP.

    rv_result = |{ build_header( lt_description ) }{ rv_result }{ build_footer( ) }|.
  ENDMETHOD.

  METHOD get_description.
    DATA lo_tabledescr  TYPE REF TO cl_abap_tabledescr.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lo_datadescr   TYPE REF TO cl_abap_datadescr.
    DATA lo_elemdescr   TYPE REF TO cl_abap_elemdescr.
    DATA ls_ddic_field  TYPE dfies.

    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data( it_data ).
    lo_structdescr ?= lo_tabledescr->get_table_line_type( ).

    LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<fs_component>).
      lo_datadescr = lo_structdescr->get_component_type( <fs_component>-name ).
      IF lo_datadescr->kind <> cl_abap_typedescr=>kind_elem.
        CONTINUE.
      ENDIF.
      lo_elemdescr ?= lo_datadescr.
      lo_elemdescr->get_ddic_field( RECEIVING  p_flddescr = ls_ddic_field
                                    EXCEPTIONS OTHERS     = 1 ).
      IF sy-subrc <> 0.
        ls_ddic_field-fieldtext = <fs_component>-name.
      ENDIF.
      INSERT VALUE #( name = <fs_component>-name
                      text = ls_ddic_field-fieldtext
                      elem = lo_elemdescr ) INTO TABLE rt_result.

    ENDLOOP.
  ENDMETHOD.

  METHOD build_footer.
    rv_result = |\n</table>|.
  ENDMETHOD.

  METHOD build_header.
    DATA lv_table_tag TYPE string.
    DATA lv_th_tag    TYPE string.
    DATA lv_tr_tag    TYPE string.

    lv_table_tag = build_table( ).
    lv_th_tag = build_th( ).
    lv_tr_tag = build_tr( ).

    LOOP AT it_description ASSIGNING FIELD-SYMBOL(<fs_description>).
      rv_result = |{ rv_result }\n  <th{ lv_th_tag }>{ <fs_description>-text }</th>|.
    ENDLOOP.

    rv_result = |<table{ lv_table_tag }>\n <tr{ lv_tr_tag }>{ rv_result }\n </tr>|.
  ENDMETHOD.

  METHOD build_table.
    IF table_id <> ''.
      rv_result = |{ rv_result } id="{ table_id }"|.
    ENDIF.

    IF table_class <> ''.
      rv_result = |{ rv_result } class="{ table_class }"|.
    ENDIF.

    IF table_style <> ''.
      rv_result = |{ rv_result } style="{ table_style }"|.
    ENDIF.
  ENDMETHOD.

  METHOD build_td.
    IF td_class <> ''.
      rv_result = |{ rv_result } class="{ td_class }"|.
    ENDIF.

    IF td_style <> ''.
      rv_result = |{ rv_result } style="{ td_style }"|.
    ENDIF.
  ENDMETHOD.

  METHOD build_th.
    IF th_class <> ''.
      rv_result = |{ rv_result } class="{ th_class }"|.
    ENDIF.

    IF th_style <> ''.
      rv_result = |{ rv_result } style="{ th_style }"|.
    ENDIF.
  ENDMETHOD.

  METHOD build_tr.
    IF tr_class <> ''.
      rv_result = |{ rv_result } class="{ tr_class }"|.
    ENDIF.

    IF tr_style <> ''.
      rv_result = |{ rv_result } style="{ tr_style }"|.
    ENDIF.
  ENDMETHOD.

  METHOD to_string.
    rv_result = zcl_xfc_conv_toolkit=>convert_int_to_ext( iv_source    = iv_value
                                                          io_elemdescr = io_elemdescr ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_csv_converter DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS export
      IMPORTING iv_seperator     TYPE clike
                VALUE(it_data)   TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS import
      IMPORTING iv_data        TYPE string
                iv_seperator   TYPE clike
      EXPORTING VALUE(et_data) TYPE STANDARD TABLE
      RAISING   zcx_xfc_toolkit_error.

  PRIVATE SECTION.
    CLASS-METHODS get_structdescr
      IMPORTING it_data            TYPE STANDARD TABLE
      RETURNING VALUE(ro_instance) TYPE REF TO cl_abap_structdescr.

    CLASS-METHODS build_header
      IMPORTING iv_seperator     TYPE clike
                io_structdescr   TYPE REF TO cl_abap_structdescr

      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS build_item
      IMPORTING iv_seperator     TYPE clike
                is_data          TYPE any
                io_structdescr   TYPE REF TO cl_abap_structdescr
      RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.


CLASS lcl_csv_converter IMPLEMENTATION.
  METHOD export.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_content     TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    TRY.
        lo_structdescr = get_structdescr( it_data ).

        APPEND build_header( iv_seperator   = iv_seperator
                             io_structdescr = lo_structdescr ) TO lt_content.

        LOOP AT it_data ASSIGNING FIELD-SYMBOL(<fs_data>).
          APPEND build_item( is_data        = <fs_data>
                             iv_seperator   = iv_seperator
                             io_structdescr = lo_structdescr ) TO lt_content.
        ENDLOOP.
        rv_result = concat_lines_of( table = lt_content
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

          SPLIT <fs_content> AT iv_seperator INTO TABLE lt_fields.

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
        CONCATENATE rv_result ls_ddicfield-fieldtext INTO rv_result SEPARATED BY iv_seperator.
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
        rv_result = |{ rv_result }{ iv_seperator }{ zcl_xfc_conv_toolkit=>convert_int_to_ext( <fs_data> ) }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_structdescr.
    ro_instance ?= CAST cl_abap_tabledescr(
        cl_abap_typedescr=>describe_by_data( it_data )
    )->get_table_line_type( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_tsv_converter DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS export
      IMPORTING VALUE(it_data)   TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE string
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


CLASS lcl_tsv_converter IMPLEMENTATION.
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
        rv_result = concat_lines_of( table = lt_content
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


CLASS lcl_xlsx_converter DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS export
      IMPORTING VALUE(it_data)   TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE xstring
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


CLASS lcl_xlsx_converter IMPLEMENTATION.
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

      zcl_xfc_conv_toolkit=>convert_ext_to_int( EXPORTING iv_source    = <fs_source>
                                                          io_elemdescr = lo_elemdescr
                                                IMPORTING ev_target    = <fs_target> ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
