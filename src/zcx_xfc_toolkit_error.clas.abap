CLASS zcx_xfc_toolkit_error DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.

    CONSTANTS:
      BEGIN OF mc_general_error,
        msgid TYPE symsgid      VALUE '00',
        msgno TYPE symsgno      VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF mc_general_error.

    DATA msgv1 TYPE msgv1.
    DATA msgv2 TYPE msgv2.
    DATA msgv3 TYPE msgv3.
    DATA msgv4 TYPE msgv4.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL
                msgv1     TYPE msgv1                    OPTIONAL
                msgv2     TYPE msgv2                    OPTIONAL
                msgv3     TYPE msgv3                    OPTIONAL
                msgv4     TYPE msgv4                    OPTIONAL.

    CLASS-METHODS raise
      IMPORTING iv_message TYPE string
      RAISING   zcx_xfc_toolkit_error.

    CLASS-METHODS raise_syst
      RAISING zcx_xfc_toolkit_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_xfc_toolkit_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise.
    DATA lv_message TYPE char200.

    lv_message = iv_message.

    RAISE EXCEPTION TYPE zcx_xfc_toolkit_error
      EXPORTING
        textid = zcx_xfc_toolkit_error=>mc_general_error
        msgv1  = lv_message(50)
        msgv2  = lv_message+50(50)
        msgv3  = lv_message+100(50)
        msgv4  = lv_message+150(50).
  ENDMETHOD.

  METHOD raise_syst.
    DATA lv_message TYPE char200.

    MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            INTO lv_message
            WITH sy-msgv1
                 sy-msgv2
                 sy-msgv3
                 sy-msgv4.

    RAISE EXCEPTION TYPE zcx_xfc_toolkit_error
      EXPORTING
        textid = zcx_xfc_toolkit_error=>mc_general_error
        msgv1  = lv_message(50)
        msgv2  = lv_message+50(50)
        msgv3  = lv_message+100(50)
        msgv4  = lv_message+150(50).
  ENDMETHOD.
ENDCLASS.
