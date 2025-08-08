CLASS zcl_xfc_table_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS itab_to_html
      IMPORTING iv_table_id      TYPE string OPTIONAL
                iv_table_class   TYPE string DEFAULT 'border-collapse: collapse; border: 1px solid #999;'
                iv_table_style   TYPE string OPTIONAL
                iv_tr_class      TYPE string OPTIONAL
                iv_tr_style      TYPE string OPTIONAL
                iv_th_class      TYPE string OPTIONAL
                iv_th_style      TYPE string DEFAULT 'text-align: center; font-weight: bold; background: #eee; border: 1px solid #999; padding: 8px;'
                iv_td_class      TYPE string OPTIONAL
                iv_td_style      TYPE string DEFAULT 'text-align: center; border: 1px solid #999; padding: 8px;'
                it_data          TYPE STANDARD TABLE
      RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.


CLASS zcl_xfc_table_toolkit IMPLEMENTATION.
  METHOD itab_to_html.
    DATA lo_converter TYPE REF TO lcl_itab_to_html.

    lo_converter = NEW lcl_itab_to_html( iv_table_id    = iv_table_id
                                         iv_table_class = iv_table_class
                                         iv_table_style = iv_table_style
                                         iv_tr_class    = iv_tr_class
                                         iv_tr_style    = iv_tr_style
                                         iv_th_class    = iv_th_class
                                         iv_th_style    = iv_th_style
                                         iv_td_class    = iv_td_class
                                         iv_td_style    = iv_td_style ).
    rv_result = lo_converter->convert( it_data ).
  ENDMETHOD.
ENDCLASS.
