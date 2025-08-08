*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_itab_to_html DEFINITION.

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


CLASS lcl_itab_to_html IMPLEMENTATION.
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
    DATA lv_value TYPE char1024.

    IF io_elemdescr->type_kind = cl_abap_typedescr=>typekind_string.
      rv_result = iv_value.
      RETURN.
    ENDIF.

    WRITE iv_value TO lv_value LEFT-JUSTIFIED.
    rv_result = lv_value.
  ENDMETHOD.
ENDCLASS.
