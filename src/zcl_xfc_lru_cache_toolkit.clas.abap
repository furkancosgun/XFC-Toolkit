CLASS zcl_xfc_lru_cache_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_key TYPE string.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_instance
      IMPORTING iv_capacity        TYPE i DEFAULT 100
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_lru_cache_toolkit.

    METHODS get
      IMPORTING  iv_key   TYPE string
      EXPORTING  ev_value TYPE any
      EXCEPTIONS cache_missed.

    METHODS put
      IMPORTING iv_key   TYPE string
                iv_value TYPE any.

    CLASS-DATA default TYPE REF TO zcl_xfc_lru_cache_toolkit.

  PRIVATE SECTION.
    METHODS constructor
      IMPORTING iv_capacity TYPE i DEFAULT 100.

    METHODS delete_node
      IMPORTING io_node TYPE REF TO zcl_xfc_lru_node.

    METHODS evict_lru_node.

    TYPES: BEGIN OF ty_map,
             k TYPE string,
             v TYPE REF TO zcl_xfc_lru_node,
           END OF ty_map.

    DATA capacity  TYPE i.
    DATA cache_map TYPE HASHED TABLE OF ty_map WITH UNIQUE KEY k.
    DATA history   TYPE REF TO zcl_xfc_lru_linked_list.
ENDCLASS.


CLASS zcl_xfc_lru_cache_toolkit IMPLEMENTATION.
  METHOD class_constructor.
    default = NEW zcl_xfc_lru_cache_toolkit( ).
  ENDMETHOD.

  METHOD get_instance.
    ro_instance = NEW zcl_xfc_lru_cache_toolkit( iv_capacity ).
  ENDMETHOD.

  METHOD constructor.
    capacity = iv_capacity.
    cache_map = VALUE #( ).
    history = NEW zcl_xfc_lru_linked_list( ).
  ENDMETHOD.

  METHOD get.
    ASSIGN me->cache_map[ k = iv_key ] TO FIELD-SYMBOL(<fs_map>).
    IF sy-subrc <> 0.
      RAISE cache_missed.
    ENDIF.

    IF me->history->head <> <fs_map>-v.
      history->unlink( <fs_map>-v ).
      history->prepend( <fs_map>-v ).
    ENDIF.

    <fs_map>-v->get_value_from_ref( IMPORTING ev_value = ev_value ).
  ENDMETHOD.

  METHOD put.
    ASSIGN me->cache_map[ k = iv_key ] TO FIELD-SYMBOL(<fs_map>).
    IF sy-subrc = 0.
      delete_node( <fs_map>-v ).
    ENDIF.

    IF lines( me->cache_map ) >= capacity.
      evict_lru_node( ).
    ENDIF.

    DATA(new_node) = NEW zcl_xfc_lru_node( iv_key   = iv_key
                                           iv_value = iv_value ).

    history->prepend( new_node ).
    INSERT VALUE #( k = iv_key
                    v = new_node ) INTO TABLE me->cache_map.
  ENDMETHOD.

  METHOD delete_node.
    history->unlink( io_node ).

    DELETE me->cache_map WHERE k = io_node->key.
  ENDMETHOD.

  METHOD evict_lru_node.
    DATA(lru_node) = history->tail.
    IF lru_node IS BOUND.
      delete_node( lru_node ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
