CLASS zcl_xfc_nr_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS number_get_next
      IMPORTING iv_nrrangenr TYPE nriv-nrrangenr
                iv_object    TYPE nriv-object
                iv_subobject TYPE nriv-subobject OPTIONAL
                iv_toyear    TYPE nriv-toyear    OPTIONAL
      EXPORTING ev_number    TYPE any
      RAISING   zcx_xfc_toolkit_error.
ENDCLASS.


CLASS zcl_xfc_nr_toolkit IMPLEMENTATION.
  METHOD number_get_next.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING  nr_range_nr = iv_nrrangenr
                 object      = iv_object
                 subobject   = iv_subobject
                 toyear      = iv_toyear
      IMPORTING  number      = ev_number
      EXCEPTIONS OTHERS      = 1.
    IF sy-subrc <> 0.
      zcx_xfc_toolkit_error=>raise_syst( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
