class ZCX_ACC16_WRITE_OFF_BAPI definition
  public
  inheriting from ZCX_ACC16_WRITE_OFF
  final
  create public .

public section.
*"* public components of class ZCX_ACC16_WRITE_OFF_BAPI
*"* do not include other source files here!!!

  constants ACC_POST type SOTR_CONC value '005056AF60B21EDBA89FAB4AE9824521'. "#EC NOTEXT
  constants BAPI_COMMIT type SOTR_CONC value '005056AF60B21EDBA89FABC26F2B8521'. "#EC NOTEXT
  data T_BAPIRET2 type BAPIRET2_TAB .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional
      !T100KEY type SCX_T100KEY optional
      !T_BAPIRET2 type BAPIRET2_TAB optional .
endclass.

class ZCX_ACC16_WRITE_OFF_BAPI implementation.
    method CONSTRUCTOR.
    CALL METHOD SUPER->CONSTRUCTOR
    EXPORTING
    PREVIOUS = PREVIOUS
    MSGV1 = MSGV1
    MSGV2 = MSGV2
    MSGV3 = MSGV3
    MSGV4 = MSGV4
    T100KEY = T100KEY
    .
    me->T_BAPIRET2 = T_BAPIRET2 .
    clear me->textid.
    if textid is initial.
      IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
    else.
      IF_T100_MESSAGE~T100KEY = TEXTID.
    endif.
    endmethod.
class 
