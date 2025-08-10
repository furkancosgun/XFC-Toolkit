*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE return_if_safe_on.
  IF iv_safe = abap_true AND is_in_update_task( ).
    RETURN.
  ENDIF.
END-OF-DEFINITION.
