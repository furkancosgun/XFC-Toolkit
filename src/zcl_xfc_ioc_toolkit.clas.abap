CLASS zcl_xfc_ioc_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_xfc_ioc_toolkit.

    METHODS get
      IMPORTING iv_instance_type  TYPE clike
      RETURNING VALUE(ro_binding) TYPE REF TO object.

    METHODS singleton
      IMPORTING io_binding TYPE REF TO object.

    METHODS factory
      IMPORTING io_binding TYPE REF TO object.

  PRIVATE SECTION.
    TYPES ty_binding_type TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF mc_scopes,
        singleton TYPE ty_binding_type VALUE 'S',
        factory   TYPE ty_binding_type VALUE 'F',
      END OF mc_scopes.

    TYPES: BEGIN OF ty_binding,
             instance           TYPE REF TO object,
             instance_type      TYPE seoclsname,
             instance_typedescr TYPE REF TO cl_abap_typedescr,
             scope              TYPE ty_binding_type,
           END OF ty_binding.
    TYPES ty_bindings TYPE HASHED TABLE OF ty_binding WITH UNIQUE KEY instance_type.

    METHODS register_binding
      IMPORTING io_instance       TYPE REF TO object OPTIONAL
                iv_instance_type  TYPE seoclsname    OPTIONAL
                iv_scope          TYPE ty_binding_type
      RETURNING VALUE(ro_binding) TYPE REF TO ty_binding.

    METHODS get_constructor_parmbind
      IMPORTING io_typedescr        TYPE REF TO cl_abap_typedescr
      RETURNING VALUE(rt_parambind) TYPE REF TO abap_parmbind_tab.

    CLASS-DATA instance TYPE REF TO zcl_xfc_ioc_toolkit.

    DATA mt_bindings TYPE ty_bindings.
ENDCLASS.


CLASS zcl_xfc_ioc_toolkit IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      instance = NEW zcl_xfc_ioc_toolkit( ).
    ENDIF.
    ro_instance = instance.
  ENDMETHOD.

  METHOD singleton.
    register_binding( io_instance = io_binding
                      iv_scope    = mc_scopes-singleton ).
  ENDMETHOD.

  METHOD factory.
    register_binding( io_instance = io_binding
                      iv_scope    = mc_scopes-factory ).
  ENDMETHOD.

  METHOD register_binding.
    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.

    IF io_instance IS SUPPLIED.
      lo_typedescr = cl_abap_typedescr=>describe_by_object_ref( io_instance ).
    ELSE.
      lo_typedescr = cl_abap_typedescr=>describe_by_name( iv_instance_type ).
    ENDIF.

    INSERT VALUE #( instance           = io_instance
                    instance_type      = lo_typedescr->get_relative_name( )
                    instance_typedescr = lo_typedescr
                    scope              = iv_scope )
           INTO TABLE mt_bindings REFERENCE INTO ro_binding.
  ENDMETHOD.

  METHOD get.
    DATA lr_binding   TYPE REF TO ty_binding.
    DATA lr_parambind TYPE REF TO abap_parmbind_tab.

    READ TABLE mt_bindings WITH KEY instance_type = iv_instance_type REFERENCE INTO lr_binding.
    IF sy-subrc <> 0.
      lr_binding = register_binding( iv_instance_type = iv_instance_type
                                     iv_scope         = mc_scopes-factory ).
    ENDIF.

    IF lr_binding->scope = mc_scopes-singleton AND lr_binding->instance IS BOUND.
      ro_binding = lr_binding->instance.
      RETURN.
    ENDIF.

    lr_parambind = get_constructor_parmbind( lr_binding->instance_typedescr ).
    IF lr_parambind->* IS INITIAL.
      CREATE OBJECT ro_binding TYPE (lr_binding->instance_typedescr->absolute_name).
    ELSE.
      CREATE OBJECT ro_binding TYPE (lr_binding->instance_typedescr->absolute_name)
        PARAMETER-TABLE lr_parambind->*.
    ENDIF.

    IF lr_binding->scope = mc_scopes-singleton AND lr_binding->instance IS NOT BOUND.
      lr_binding->instance = ro_binding.
    ENDIF.
  ENDMETHOD.

  METHOD get_constructor_parmbind.
    CONSTANTS mc_constructor TYPE abap_methname VALUE 'CONSTRUCTOR'.

    DATA lo_classdescr  TYPE REF TO cl_abap_classdescr.
    DATA lo_constructor TYPE REF TO abap_methdescr.
    DATA lo_parmdescr   TYPE REF TO abap_parmdescr.
    DATA lo_refdescr    TYPE REF TO cl_abap_refdescr.
    DATA lo_parmbind    TYPE abap_parmbind.
    DATA lo_dependency  TYPE REF TO cl_abap_objectdescr.

    FIELD-SYMBOLS <fs_object> TYPE any.

    lo_classdescr ?= io_typedescr.

    CREATE DATA rt_parambind.

    READ TABLE lo_classdescr->methods REFERENCE INTO lo_constructor WITH KEY name = mc_constructor.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lo_constructor->parameters REFERENCE INTO lo_parmdescr WHERE     type_kind = cl_abap_objectdescr=>typekind_oref
                                                                         AND parm_kind = cl_abap_objectdescr=>importing.
      lo_refdescr ?= lo_classdescr->get_method_parameter_type( p_method_name    = lo_constructor->name
                                                               p_parameter_name = lo_parmdescr->name ).

      CREATE DATA lo_parmbind-value TYPE HANDLE lo_refdescr.
      ASSIGN lo_parmbind-value->* TO <fs_object>.
      lo_dependency ?= lo_refdescr->get_referenced_type( ).
      <fs_object> = get( iv_instance_type = lo_dependency->absolute_name ).
      lo_parmbind-name = lo_parmdescr->name.
      lo_parmbind-kind = cl_abap_objectdescr=>exporting.
      INSERT lo_parmbind INTO TABLE rt_parambind->*.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
