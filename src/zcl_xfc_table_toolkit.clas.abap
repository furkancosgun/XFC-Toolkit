CLASS zcl_xfc_table_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS itab_to_html
      IMPORTING iv_table_id      TYPE string OPTIONAL
                iv_table_class   TYPE string OPTIONAL
                iv_table_style   TYPE string DEFAULT 'border-collapse: collapse; border: 1px solid #999;'
                iv_tr_class      TYPE string OPTIONAL
                iv_tr_style      TYPE string OPTIONAL
                iv_th_class      TYPE string OPTIONAL
                iv_th_style      TYPE string DEFAULT 'text-align: center; font-weight: bold; background: #eee; border: 1px solid #999; padding: 8px;'
                iv_td_class      TYPE string OPTIONAL
                iv_td_style      TYPE string DEFAULT 'text-align: center; border: 1px solid #999; padding: 8px;'
                it_data          TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS build_fcat_from_table
      IMPORTING VALUE(it_data)   TYPE STANDARD TABLE
      RETURNING VALUE(rt_result) TYPE lvc_t_fcat
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS build_fcat_from_struct
      IMPORTING VALUE(is_data)   TYPE any
      RETURNING VALUE(rt_result) TYPE lvc_t_fcat
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS itab_to_xlsx
      IMPORTING VALUE(it_data)    TYPE STANDARD TABLE
      RETURNING VALUE(rv_xstring) TYPE xstring
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS xlsx_to_itab
      IMPORTING iv_xstring TYPE xstring
      EXPORTING et_data    TYPE STANDARD TABLE
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS itab_to_csv
      IMPORTING VALUE(it_data)   TYPE STANDARD TABLE
                iv_seperator     TYPE clike DEFAULT ';'
      RETURNING VALUE(rv_string) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS csv_to_itab
      IMPORTING iv_string    TYPE string
                iv_seperator TYPE clike DEFAULT ';'
      EXPORTING et_data      TYPE STANDARD TABLE
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS itab_to_tsv
      IMPORTING VALUE(it_data)   TYPE STANDARD TABLE
      RETURNING VALUE(rv_string) TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS tsv_to_itab
      IMPORTING iv_string TYPE string
      EXPORTING et_data   TYPE STANDARD TABLE
      RAISING   zcx_xfc_toolkit_error.
ENDCLASS.


CLASS zcl_xfc_table_toolkit IMPLEMENTATION.
  METHOD build_fcat_from_table.
    DATA lo_salv TYPE REF TO cl_salv_table.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_salv
                                CHANGING  t_table      = it_data ).
        rt_result = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_salv->get_columns( )
                                                                       r_aggregations = lo_salv->get_aggregations( ) ).
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD build_fcat_from_struct.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_ddic_fields TYPE ddfields.

    TRY.
        lo_structdescr ?= cl_abap_typedescr=>describe_by_data( is_data ).
        lt_ddic_fields = cl_salv_data_descr=>read_structdescr( lo_structdescr ).
        MOVE-CORRESPONDING lt_ddic_fields TO rt_result.
      CATCH cx_root INTO DATA(lx_root).
        zcx_xfc_toolkit_error=>raise( lx_root->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD csv_to_itab.
    lcl_csv_converter=>import( EXPORTING iv_data      = iv_string
                                         iv_seperator = iv_seperator
                               IMPORTING et_data      = et_data ).
  ENDMETHOD.

  METHOD itab_to_csv.
    rv_string = lcl_csv_converter=>export( iv_seperator = iv_seperator
                                           it_data      = it_data ).
  ENDMETHOD.

  METHOD itab_to_html.
    rv_result = NEW lcl_html_converter( iv_table_id    = iv_table_id
                                        iv_table_class = iv_table_class
                                        iv_table_style = iv_table_style
                                        iv_tr_class    = iv_tr_class
                                        iv_tr_style    = iv_tr_style
                                        iv_th_class    = iv_th_class
                                        iv_th_style    = iv_th_style
                                        iv_td_class    = iv_td_class
                                        iv_td_style    = iv_td_style
     )->convert( it_data = it_data ).
  ENDMETHOD.

  METHOD itab_to_tsv.
    rv_string = lcl_tsv_converter=>export( it_data = it_data ).
  ENDMETHOD.

  METHOD itab_to_xlsx.
    rv_xstring = lcl_xlsx_converter=>export( it_data = it_data ).
  ENDMETHOD.

  METHOD tsv_to_itab.
    lcl_tsv_converter=>import( EXPORTING iv_data = iv_string
                               IMPORTING et_data = et_data ).
  ENDMETHOD.

  METHOD xlsx_to_itab.
    lcl_xlsx_converter=>import( EXPORTING iv_xdata = iv_xstring
                                IMPORTING et_data  = et_data ).
  ENDMETHOD.
ENDCLASS.
