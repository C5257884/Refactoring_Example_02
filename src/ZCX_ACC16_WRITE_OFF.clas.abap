class ZCX_ACC16_WRITE_OFF definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.
*"* public components of class ZCX_ACC16_WRITE_OFF
*"* do not include other source files here!!!

  interfaces IF_T100_MESSAGE .

  aliases T100KEY
    for IF_T100_MESSAGE~T100KEY .

  constants:
    begin of COMMON,
      msgid type symsgid value 'ZMM2',
      msgno type symsgno value '024',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV3',
    end of COMMON .
  constants:
    begin of NRVRS_CORR16,
      msgid type symsgid value 'ZMM_ACC16_WRITE_OFF',
      msgno type symsgno value '003',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of NRVRS_CORR16 .
  constants:
    begin of NRVRS_WRITEOFF,
      msgid type symsgid value 'ZMM_ACC16_WRITE_OFF',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of NRVRS_WRITEOFF .
  constants:
    begin of PREV_NCALC,
      msgid type symsgid value 'ZMM_ACC16_WRITE_OFF',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PREV_NCALC .
  constants:
    begin of UNPOSTED_RETROCORR_DOCS,
      msgid type symsgid value 'ZMM2',
      msgno type symsgno value '136',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNPOSTED_RETROCORR_DOCS .
  constants:
    begin of BONUS_FOR_MATNR_NOT_FOUND,
      msgid type symsgid value 'ZMM_ACC16_WRITE_OFF',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of BONUS_FOR_MATNR_NOT_FOUND .
  constants:
    begin of NO_DATA_FOR_REVERSING,
      msgid type symsgid value 'ZMM_ACC16_WRITE_OFF',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_DATA_FOR_REVERSING .
  constants:
    begin of IN_NEXT_PERIOD_POSINGS_EXIST,
      msgid type symsgid value 'ZMM_ACC16_WRITE_OFF',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of IN_NEXT_PERIOD_POSINGS_EXIST .
  constants:
    begin of SO_MATNR_NOT_EMPTY,
      msgid type symsgid value 'ZMM_ACC16_WRITE_OFF',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SO_MATNR_NOT_EMPTY .
  constants:
    begin of NEG_W_STOCK_16_FIN,
      msgid type symsgid value 'ZMM_ACC16_WRITE_OFF',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value '',
    end of NEG_W_STOCK_16_FIN .
  data MSGV1 type SYMSGV .
  data MSGV2 type SYMSGV .
  data MSGV3 type SYMSGV .
  data MSGV4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional
      !T100KEY type SCX_T100KEY optional .
      

method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->T100KEY = T100KEY .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.
      
