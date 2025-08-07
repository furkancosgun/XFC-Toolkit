CLASS zcl_xfc_lru_node DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xfc_lru_linked_list
                 zcl_xfc_lru_cache_toolkit.

  PRIVATE SECTION.
    METHODS constructor
      IMPORTING iv_key       TYPE string
                iv_value     TYPE any
                io_prev_node TYPE REF TO zcl_xfc_lru_node OPTIONAL
                io_next_node TYPE REF TO zcl_xfc_lru_node OPTIONAL.

    DATA key       TYPE string.
    DATA ref       TYPE REF TO data.
    DATA prev_node TYPE REF TO zcl_xfc_lru_node.
    DATA next_node TYPE REF TO zcl_xfc_lru_node.

    METHODS set_value_to_ref
      IMPORTING iv_value TYPE any.

    METHODS get_value_from_ref
      EXPORTING ev_value TYPE any.
ENDCLASS.


CLASS zcl_xfc_lru_node IMPLEMENTATION.
  METHOD constructor.
    key = iv_key.
    CREATE DATA ref LIKE iv_value.
    prev_node = io_prev_node.
    next_node = io_next_node.

    set_value_to_ref( iv_value ).
  ENDMETHOD.

  METHOD set_value_to_ref.
    ASSIGN ref->* TO FIELD-SYMBOL(<fs_value>).
    <fs_value> = iv_value.
  ENDMETHOD.

  METHOD get_value_from_ref.
    ASSIGN ref->* TO FIELD-SYMBOL(<fs_value>).
    ev_value = <fs_value>.
  ENDMETHOD.
ENDCLASS.
