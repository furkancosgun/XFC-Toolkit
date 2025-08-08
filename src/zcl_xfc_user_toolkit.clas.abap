CLASS zcl_xfc_user_toolkit DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_full_name
      IMPORTING iv_uname         TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_email
      IMPORTING iv_uname         TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_mobile_number
      IMPORTING iv_uname         TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_language
      IMPORTING iv_uname         TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_computer_name
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_windows_directory
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_system_directory
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_temp_directory
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_domain_username
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_windows_platform
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_program_path
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_currenty_directory
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_desktop_directory
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS can_debug
      IMPORTING iv_uname         TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS can_develop
      IMPORTING iv_uname         TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(rv_result) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-METHODS gui_get_desktop_info
      IMPORTING iv_type          TYPE i
      RETURNING VALUE(rv_result) TYPE char255.

    CLASS-METHODS bapi_user_get_detail
      IMPORTING iv_uname         TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(rs_result) TYPE bapiaddr3.

ENDCLASS.


CLASS zcl_xfc_user_toolkit IMPLEMENTATION.
  METHOD can_debug.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP' FOR USER iv_uname
                    ID 'ACTVT'    FIELD '02'
                    ID 'OBJTYPE'  FIELD 'DEBUG'
                    ID 'DEVCLASS' DUMMY
                    ID 'OBJNAME'  DUMMY
                    ID 'P_GROUP'  DUMMY.
    rv_result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD can_develop.
    AUTHORITY-CHECK OBJECT 'S_DEVELOP' FOR USER iv_uname
                    ID 'ACTVT'    FIELD '02'
                    ID 'OBJTYPE'  DUMMY
                    ID 'DEVCLASS' DUMMY
                    ID 'OBJNAME'  DUMMY
                    ID 'P_GROUP'  DUMMY.
    rv_result = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD get_email.
    rv_result = bapi_user_get_detail( iv_uname )-e_mail.
  ENDMETHOD.

  METHOD get_full_name.
    rv_result = bapi_user_get_detail( iv_uname )-fullname.
  ENDMETHOD.

  METHOD get_language.
    rv_result = bapi_user_get_detail( iv_uname )-langu_p.
  ENDMETHOD.

  METHOD get_mobile_number.
    rv_result = bapi_user_get_detail( iv_uname )-tel1_numbr.
  ENDMETHOD.

  METHOD get_computer_name.
    rv_result = gui_get_desktop_info( sfes_info_computer_name ).
  ENDMETHOD.

  METHOD get_currenty_directory.
    rv_result = gui_get_desktop_info( sfes_info_current_directory ).
  ENDMETHOD.

  METHOD get_desktop_directory.
    rv_result = gui_get_desktop_info( sfes_info_desktop_directory ).
  ENDMETHOD.

  METHOD get_domain_username.
    rv_result = gui_get_desktop_info( sfes_info_user_name ).
  ENDMETHOD.

  METHOD get_windows_platform.
    rv_result = gui_get_desktop_info( sfes_info_windows_platform ).
  ENDMETHOD.

  METHOD get_program_path.
    rv_result = gui_get_desktop_info( sfes_info_program_path ).
  ENDMETHOD.

  METHOD get_system_directory.
    rv_result = gui_get_desktop_info( sfes_info_system_directory ).
  ENDMETHOD.

  METHOD get_temp_directory.
    rv_result = gui_get_desktop_info( sfes_info_temp_directory ).
  ENDMETHOD.

  METHOD get_windows_directory.
    rv_result = gui_get_desktop_info( sfes_info_windows_directory ).
  ENDMETHOD.

  METHOD bapi_user_get_detail.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_uname
      IMPORTING
        address  = rs_result
      TABLES
        return   = lt_return.
  ENDMETHOD.

  METHOD gui_get_desktop_info.
    CALL FUNCTION 'GUI_GET_DESKTOP_INFO'
      EXPORTING
        type   = iv_type
      CHANGING
        return = rv_result.
  ENDMETHOD.
ENDCLASS.
