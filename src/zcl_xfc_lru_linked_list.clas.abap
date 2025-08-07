CLASS zcl_xfc_lru_linked_list DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_xfc_lru_cache_toolkit.

  PUBLIC SECTION.
    METHODS prepend
      IMPORTING io_node TYPE REF TO zcl_xfc_lru_node.

    METHODS unlink
      IMPORTING io_node TYPE REF TO zcl_xfc_lru_node.

    DATA head TYPE REF TO zcl_xfc_lru_node.
    DATA tail TYPE REF TO zcl_xfc_lru_node.
ENDCLASS.


CLASS zcl_xfc_lru_linked_list IMPLEMENTATION.
  METHOD prepend.
    IF me->head IS BOUND.
      io_node->next_node = head.
      head->prev_node = io_node.
    ENDIF.

    IF me->tail IS NOT BOUND.
      tail = io_node.
    ENDIF.

    head = io_node.
  ENDMETHOD.

  METHOD unlink.
    CHECK io_node IS BOUND.

    DATA(prev_node) = io_node->prev_node.
    DATA(next_node) = io_node->next_node.

    IF prev_node IS BOUND.
      prev_node->next_node = next_node.
    ENDIF.

    IF next_node IS BOUND.
      next_node->prev_node = prev_node.
    ENDIF.

    IF me->head = io_node.
      head = next_node.
    ENDIF.

    IF me->tail = io_node.
      tail = prev_node.
    ENDIF.

    CLEAR: io_node->prev_node,
           io_node->next_node.
  ENDMETHOD.
ENDCLASS.
