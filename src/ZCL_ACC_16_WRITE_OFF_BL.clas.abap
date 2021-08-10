class ZCL_ACC_16_WRITE_OFF_BL definition
  public
  create public .

public section.
*"* public components of class ZCL_ACC_16_WRITE_OFF_BL
*"* do not include other source files here!!!
  type-pools ICON .

  constants:
    BEGIN OF cs_success_bapi_msg,
          id TYPE sy-msgid VALUE 'RW',
          no TYPE sy-msgno VALUE '605',
        END OF cs_success_bapi_msg .

  class ZCL_ACC16_WRITE_OFF_M definition load .
  methods CHECK_BEFORE_CORR16
    importing
      !IV_SPMON type SPMON
      !IV_LIFNR type LIFNR
      !IT_OUTDATA type ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
    raising
      ZCX_ACC16_WRITE_OFF .
  methods RESET_ANSWER .
  methods BAPI_ACC_DOCUMENT_REV_POST
    importing
      !IS_REVERSAL type BAPIACREV
      !IV_BUS_ACT type BAPIACHE09-BUS_ACT
    exporting
      !OBJ_TYPE type BAPIACREV-OBJ_TYPE
      !OBJ_KEY type BAPIACREV-OBJ_KEY
      !OBJ_SYS type BAPIACREV-OBJ_SYS
    changing
      !CT_RETURN type BAPIRET2_TAB
    raising
      ZCX_ACC16_WRITE_OFF_BAPI .
  methods CONSTRUCTOR
    importing
      !IO_SSC type ref to ZCL_ACC16_WRITE_OFF_SSC
      !IO_LOG type ref to ZIF_APP_LOG
      !IO_MODEL type ref to ZCL_ACC16_WRITE_OFF_M .
  methods CHECK_BEFORE_POSTING
    importing
      !IV_SPMON type SPMON
      !IV_LIFNR type LIFNR
      !IT_OUTDATA type ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
    raising
      ZCX_ACC16_WRITE_OFF .
  methods CHECK_BEFORE_REVERSING
    importing
      !IV_SPMON type SPMON
      !IV_LIFNR type LFA1-LIFNR
      !IT_OUTDATA type ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
    returning
      value(RT_FI_DOCS) type ZCL_ACC16_WRITE_OFF_M=>TY_T_FIDOC
    raising
      ZCX_ACC16_WRITE_OFF .
  methods BAPI_ACC_DOCUMENT_POST
    importing
      !IS_DOCUMENTHEADER type BAPIACHE09
      !IS_CUSTOMERCPD type BAPIACPA09 optional
      !IS_CONTRACTHEADER type BAPIACCAHD optional
    exporting
      !OBJ_TYPE type BAPIACREV-OBJ_TYPE
      !OBJ_KEY type BAPIACREV-OBJ_KEY
      !OBJ_SYS type BAPIACREV-OBJ_SYS
    changing
      !CT_ACCOUNTGL type BAPIACGL09_TAB optional
      !CT_ACCOUNTRECEIVABLE type BAPIACAR09_TAB optional
      !CT_ACCOUNTPAYABLE type BAPIACAP09_TAB optional
      !CT_ACCOUNTTAX type BAPIACTX09_TAB optional
      !CT_CURRENCYAMOUNT type BAPIACCR09_TAB optional
      !CT_CRITERIA type BAPIACKEC9_TAB optional
      !CT_VALUEFIELD type BAPIACKEV9_TAB optional
      !CT_EXTENSION1 type BAPIACEXTC_TAB optional
      !CT_RETURN type BAPIRET2_TAB optional
      !CT_PAYMENTCARD type BAPIACPC09_TAB optional
      !CT_CONTRACTITEM type BAPIACCAIT_TAB optional
      !CT_EXTENSION2 type BAPIPAREX_TAB optional
      !CT_REALESTATE type BAPIACRE09_TAB optional
      !CT_ACCOUNTWT type BAPIACWT09_TAB optional
    raising
      ZCX_ACC16_WRITE_OFF_BAPI .
  methods BAPI_COMMIT
    importing
      !WAIT type BAPITA-WAIT default SPACE
    returning
      value(RETURN) type BAPIRET2
    raising
      ZCX_ACC16_WRITE_OFF_BAPI .
  methods BAPI_ROLLBACK
    returning
      value(RETURN) type BAPIRET2 .
  PROTECTED SECTION.
*"* protected components of class ZCL_ACC_16_WRITE_OFF_BL
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_ACC_16_WRITE_OFF_BL
*"* do not include other source files here!!!

  data M_NEG_ANSW type BOOLE_D .
  data MO_MODEL type ref to ZCL_ACC16_WRITE_OFF_M .
  data MO_SSC type ref to ZCL_ACC16_WRITE_OFF_SSC .
  data MO_LOG type ref to ZIF_APP_LOG .

  class ZCL_ACC16_WRITE_OFF_M definition load .
  methods CHECK_NEG_VAL_W_STOCK_16_FIN
    importing
      !IT_OUTDATA type ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
    raising
      ZCX_ACC16_WRITE_OFF .
  methods CHECK_ALL_RETRO_IN_PER
    importing
      !IV_SPMON type SPMON
      !IT_DOCNUM type ZCL_ACC16_WRITE_OFF_M=>TY_T_DOCNUM
    raising
      ZCX_ACC16_WRITE_OFF .
  methods CHECK_NRVRS_RETRO_PREV
    importing
      !IT_ZMMCORR16 type ZT_ZMMCORR16
    raising
      ZCX_ACC16_WRITE_OFF .
  methods CHECK_NRVRS_CORR16
    importing
      !IV_SPMON type SPMON
      !IV_LIFNR type LIFNR
      !IT_OUTDATA type ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
    raising
      ZCX_ACC16_WRITE_OFF .
ENDCLASS.



CLASS ZCL_ACC_16_WRITE_OFF_BL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->BAPI_ACC_DOCUMENT_POST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DOCUMENTHEADER              TYPE        BAPIACHE09
* | [--->] IS_CUSTOMERCPD                 TYPE        BAPIACPA09(optional)
* | [--->] IS_CONTRACTHEADER              TYPE        BAPIACCAHD(optional)
* | [<---] OBJ_TYPE                       TYPE        BAPIACREV-OBJ_TYPE
* | [<---] OBJ_KEY                        TYPE        BAPIACREV-OBJ_KEY
* | [<---] OBJ_SYS                        TYPE        BAPIACREV-OBJ_SYS
* | [<-->] CT_ACCOUNTGL                   TYPE        BAPIACGL09_TAB(optional)
* | [<-->] CT_ACCOUNTRECEIVABLE           TYPE        BAPIACAR09_TAB(optional)
* | [<-->] CT_ACCOUNTPAYABLE              TYPE        BAPIACAP09_TAB(optional)
* | [<-->] CT_ACCOUNTTAX                  TYPE        BAPIACTX09_TAB(optional)
* | [<-->] CT_CURRENCYAMOUNT              TYPE        BAPIACCR09_TAB(optional)
* | [<-->] CT_CRITERIA                    TYPE        BAPIACKEC9_TAB(optional)
* | [<-->] CT_VALUEFIELD                  TYPE        BAPIACKEV9_TAB(optional)
* | [<-->] CT_EXTENSION1                  TYPE        BAPIACEXTC_TAB(optional)
* | [<-->] CT_RETURN                      TYPE        BAPIRET2_TAB(optional)
* | [<-->] CT_PAYMENTCARD                 TYPE        BAPIACPC09_TAB(optional)
* | [<-->] CT_CONTRACTITEM                TYPE        BAPIACCAIT_TAB(optional)
* | [<-->] CT_EXTENSION2                  TYPE        BAPIPAREX_TAB(optional)
* | [<-->] CT_REALESTATE                  TYPE        BAPIACRE09_TAB(optional)
* | [<-->] CT_ACCOUNTWT                   TYPE        BAPIACWT09_TAB(optional)
* | [!CX!] ZCX_ACC16_WRITE_OFF_BAPI
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bapi_acc_document_post.

    CLEAR ct_return[].

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = is_documentheader
        customercpd       = is_customercpd
        contractheader    = is_contractheader
      IMPORTING
        obj_type          = obj_type
        obj_key	          = obj_key
        obj_sys	          = obj_sys
      TABLES
        accountgl         = ct_accountgl[]
        accountreceivable = ct_accountreceivable[]
        accountpayable    = ct_accountpayable[]
        accounttax        = ct_accounttax[]
        currencyamount    = ct_currencyamount[]
        criteria          = ct_criteria[]
        valuefield        = ct_valuefield[]
        extension1        = ct_extension1[]
        return            = ct_return[]
        paymentcard       = ct_paymentcard[]
        contractitem      = ct_contractitem[]
        extension2        = ct_extension2[]
        realestate        = ct_realestate[]
        accountwt         = ct_accountwt[].
    READ TABLE ct_return[] TRANSPORTING NO FIELDS
                            WITH KEY id     = cs_success_bapi_msg-id
                                     number = cs_success_bapi_msg-no.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_acc16_write_off_bapi
        EXPORTING
          t_bapiret2 = ct_return[].
    ENDIF.
  ENDMETHOD.                    "bapi_acc_document_post


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->BAPI_ACC_DOCUMENT_REV_POST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_REVERSAL                    TYPE        BAPIACREV
* | [--->] IV_BUS_ACT                     TYPE        BAPIACHE09-BUS_ACT
* | [<---] OBJ_TYPE                       TYPE        BAPIACREV-OBJ_TYPE
* | [<---] OBJ_KEY                        TYPE        BAPIACREV-OBJ_KEY
* | [<---] OBJ_SYS                        TYPE        BAPIACREV-OBJ_SYS
* | [<-->] CT_RETURN                      TYPE        BAPIRET2_TAB
* | [!CX!] ZCX_ACC16_WRITE_OFF_BAPI
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bapi_acc_document_rev_post.

    CLEAR ct_return[].

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
      EXPORTING
        reversal = is_reversal
        bus_act  = iv_bus_act
      IMPORTING
        obj_type = obj_type
        obj_key  = obj_key
        obj_sys  = obj_sys
      TABLES
        return   = ct_return[].

    READ TABLE ct_return[] TRANSPORTING NO FIELDS
      WITH KEY id     = cs_success_bapi_msg-id
               number = cs_success_bapi_msg-no.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_acc16_write_off_bapi
        EXPORTING
          t_bapiret2 = ct_return[].
    ENDIF.

  ENDMETHOD.                    "bapi_acc_document_rev_post


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->BAPI_COMMIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] WAIT                           TYPE        BAPITA-WAIT (default =SPACE)
* | [<-()] RETURN                         TYPE        BAPIRET2
* | [!CX!] ZCX_ACC16_WRITE_OFF_BAPI
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bapi_commit.
    DATA: lt_return TYPE bapiret2_tab.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = wait
      IMPORTING
        return = return.
    IF return-id = 'E'.
      APPEND return TO lt_return[].
      RAISE EXCEPTION TYPE zcx_acc16_write_off_bapi
        EXPORTING
          t_bapiret2 = lt_return[].
    ENDIF.

  ENDMETHOD.                    "bapi_commit


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->BAPI_ROLLBACK
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RETURN                         TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD bapi_rollback.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = return.
  ENDMETHOD.                    "bapi_rollback


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ACC_16_WRITE_OFF_BL->CHECK_ALL_RETRO_IN_PER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SPMON                       TYPE        SPMON
* | [--->] IT_DOCNUM                      TYPE        ZCL_ACC16_WRITE_OFF_M=>TY_T_DOCNUM
* | [!CX!] ZCX_ACC16_WRITE_OFF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_all_retro_in_per.
    FIELD-SYMBOLS <ls_docnum> LIKE LINE OF it_docnum[].
    DATA: lv_docnum LIKE <ls_docnum>-docnum .

    LOOP AT it_docnum[] ASSIGNING <ls_docnum>.
      MESSAGE e136(zmm2) WITH iv_spmon <ls_docnum>-docnum INTO sy-msgli.
      mo_log->add_symsg( ).
      AT FIRST.
        lv_docnum = <ls_docnum>-docnum .
      ENDAT.
    ENDLOOP.

    IF NOT mo_log->is_empty( ) = abap_true.
      mo_log->show_bal_log( ).
      RAISE EXCEPTION TYPE zcx_acc16_write_off
        EXPORTING
          textid = zcx_acc16_write_off=>unposted_retrocorr_docs
          msgv1  = |{ iv_spmon+4(2) }.{ iv_spmon(4) }|
          msgv2  = |{ lv_docnum  }|.
    ENDIF.

  ENDMETHOD.                    "check_all_retro_in_per


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->CHECK_BEFORE_CORR16
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SPMON                       TYPE        SPMON
* | [--->] IV_LIFNR                       TYPE        LIFNR
* | [--->] IT_OUTDATA                     TYPE        ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
* | [!CX!] ZCX_ACC16_WRITE_OFF
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD check_before_corr16.
  FIELD-SYMBOLS: <ls_output> LIKE LINE OF it_outdata[].
  DATA: ls_message TYPE symsg ,
        lv_c_f16_fin_kor(16)  ,
        lv_excpt TYPE boole_d .
  DATA: lo_popup TYPE REF TO lcl_popup .

  lv_excpt = abap_false.

  LOOP AT it_outdata[] ASSIGNING <ls_output> WHERE lifnr = iv_lifnr " Вендор
                                               AND spmon = iv_spmon.

    IF ( NOT <ls_output>-belnr_kor16 IS INITIAL OR     " номер бух.документа Кор16
         NOT <ls_output>-gjahr_kor16 IS INITIAL )  AND " год   бух.документа Кор16
             <ls_output>-stock_16_fin_kor <> 0     AND " Сумма коректировки
       ( <ls_output>-belnr_kor16_st IS INITIAL OR      " номер бух.документа Кор16 сторно
         <ls_output>-gjahr_kor16_st IS INITIAL ).      " год   бух.документа Кор16 сторно

      CLEAR: ls_message.
      "Коррекция сальдо 16сч. & & & &
      ls_message-msgv2 = |{ 'Строка'(101) } { <ls_output>-spmon+4(2) }.{ <ls_output>-spmon(4) }/{ iv_lifnr }/{ <ls_output>-matnr }| .

      WRITE <ls_output>-stock_16_fin_kor TO lv_c_f16_fin_kor CURRENCY 'RUB'.
      CONDENSE lv_c_f16_fin_kor ##UOM_IN_MES. " MSGV3
      ls_message-msgv3 = |{ 'уже содержит значение корректировки'(102) } { lv_c_f16_fin_kor }|.
      MESSAGE e039(zmm2) WITH 'не разрешена.'(100) ls_message-msgv2 ls_message-msgv3 space INTO sy-msgli.
      mo_log->add_symsg( ).
      lv_excpt = abap_true.
    ENDIF.

    IF <ls_output>-w_stock_16_fin < <ls_output>-stock_16_fin_kor.

      ls_message-msgv1 = |{ <ls_output>-spmon+4(2) }.{ <ls_output>-spmon(4) }| .
      ls_message-msgv2 = |{ <ls_output>-lifnr }|          .
      ls_message-msgv3 = |{ <ls_output>-stock_16_fin_kor }| .
      ls_message-msgv4 = |{ <ls_output>-w_stock_16_fin }| .

      IF m_neg_answ IS INITIAL.
        CREATE OBJECT lo_popup.

        lo_popup->collect_params( param = 'PERI'     value = |{ ls_message-msgv1 }|
               )->collect_params( param = 'LIFNR'    value = |{ ls_message-msgv2 }|
               )->collect_params( param = 'SUMMKORR' value = |{ ls_message-msgv3 }|
               )->collect_params( param = 'SUMMEND'  value = |{ ls_message-msgv4 }| ).

        m_neg_answ =
          lo_popup->confirm( i_titlebar              = |{ 'Проверка'(003) }|
                             i_diagnose_object       = |ZZACC16_CHBP_CORR|
                             i_text_question         = |{ 'Выберите вариант действия'(006) }|
                             i_text_button_1         = |{ 'Предупреждение'(004) }|
                             i_icon_button_1         = |{ icon_status_ok }|
                             i_text_button_2         = |{ 'Ошибка'(005) }|
                             i_icon_button_2         = |{ icon_cancel }|
                             i_display_cancel_button = abap_false
                             it_params               = lo_popup->get_params( ) ).
      ENDIF.

      IF m_neg_answ = '1'.
        ls_message-msgty = 'W'.
      ELSE.
        ls_message-msgty = 'E'.
        lv_excpt = abap_true.
      ENDIF.

      MESSAGE
        ID      'ZMM_ACC16_WRITE_OFF' TYPE    ls_message-msgty NUMBER  '015'
        WITH    ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
        INTO    sy-msgli.
      mo_log->add_symsg(  ).
    ENDIF.

  ENDLOOP.

  IF lv_excpt = abap_true.
    RAISE EXCEPTION TYPE zcx_acc16_write_off.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->CHECK_BEFORE_POSTING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SPMON                       TYPE        SPMON
* | [--->] IV_LIFNR                       TYPE        LIFNR
* | [--->] IT_OUTDATA                     TYPE        ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
* | [!CX!] ZCX_ACC16_WRITE_OFF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_before_posting.
    DATA:
      lt_zmmcorr16  TYPE zt_zmmcorr16 ,
      lt_docnum     TYPE zcl_acc16_write_off_m=>ty_t_docnum.

    "-----------  Проверка на несторнированные распределения ------------------------------------*
    "             Проверка на отсутсвие несторнированных док.по корректировкам 16-го  -----------*
    check_nrvrs_corr16( iv_spmon   = iv_spmon
                        iv_lifnr   = iv_lifnr
                        it_outdata = it_outdata[] ).

    "----------- Проверка ретрокоррекции за прошлый период --------------------------------------*
    lt_zmmcorr16[] = mo_model->db_read_corr16_prev_mnth( iv_spmon = iv_spmon   iv_lifnr = iv_lifnr ).
    check_nrvrs_retro_prev( lt_zmmcorr16[] ).

    "--------- Проверка, что все документы ретрокоррекции проведены в периоде проводки ----------*
    lt_docnum[] = mo_model->db_read_bonus_docnum( iv_spmon = iv_spmon   iv_lifnr = iv_lifnr ).
    check_all_retro_in_per( iv_spmon = iv_spmon   it_docnum = lt_docnum[] ).

    check_neg_val_w_stock_16_fin( it_outdata[] ).

  ENDMETHOD.                    "check_before_reversing


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->CHECK_BEFORE_REVERSING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SPMON                       TYPE        SPMON
* | [--->] IV_LIFNR                       TYPE        LFA1-LIFNR
* | [--->] IT_OUTDATA                     TYPE        ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
* | [<-()] RT_FI_DOCS                     TYPE        ZCL_ACC16_WRITE_OFF_M=>TY_T_FIDOC
* | [!CX!] ZCX_ACC16_WRITE_OFF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_before_reversing.
    FIELD-SYMBOLS <ls_outdata> LIKE LINE OF it_outdata[].
    DATA: ls_fi_doc           LIKE LINE OF rt_fi_docs[],
          lv_future_post_cnt  TYPE sy-dbcnt.

*  Если имеюится неотсторнированные позиции KORR_16 основная проводка запрещается
*   в ЛЮБОЙ из строк по данному поставшику
* ----
    LOOP AT it_outdata[] ASSIGNING <ls_outdata> WHERE spmon = iv_spmon
                                                  AND lifnr = iv_lifnr.

      IF <ls_outdata>-belnr_kor16    IS NOT INITIAL AND
         <ls_outdata>-gjahr_kor16    IS NOT INITIAL AND
         <ls_outdata>-belnr_kor16_st IS INITIAL     AND
         <ls_outdata>-gjahr_kor16_st IS INITIAL.

        " MESSAGE e003(zmm_acc16_write_off)
        RAISE EXCEPTION TYPE zcx_acc16_write_off
          EXPORTING
            textid = zcx_acc16_write_off=>nrvrs_corr16
            msgv1  = |{ iv_spmon+4(2) }.{ iv_spmon(4) }|
            msgv2  = |{ iv_lifnr }|
            msgv3  = |{ <ls_outdata>-belnr_kor16 }|.
      ENDIF.

      "----- НУЖНО ЧТОБЫ СМЕНИЛСЯ LIFNR
      IF <ls_outdata>-belnr_fi IS NOT INITIAL AND
         <ls_outdata>-belnr_st IS INITIAL.

        READ TABLE rt_fi_docs[] TRANSPORTING NO FIELDS
          WITH TABLE KEY belnr = <ls_outdata>-belnr_fi
                         gjahr = <ls_outdata>-gjahr_fi
                         bukrs = <ls_outdata>-bukrs.
        IF sy-subrc <> 0.
          ls_fi_doc-belnr = <ls_outdata>-belnr_fi.    "
          ls_fi_doc-gjahr = <ls_outdata>-gjahr_fi.    " Доккументы
          ls_fi_doc-bukrs = <ls_outdata>-bukrs.       "   подлежащие
          ls_fi_doc-lifnr = <ls_outdata>-lifnr.       "     сторнированию
          ls_fi_doc-spmon = <ls_outdata>-spmon.       "
          INSERT ls_fi_doc INTO TABLE rt_fi_docs[].
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF rt_fi_docs[] IS INITIAL.
      " MESSAGE E009(ZMM_ACC16_WRITE_OFF)
      RAISE EXCEPTION TYPE zcx_acc16_write_off
        EXPORTING
          textid = zcx_acc16_write_off=>no_data_for_reversing
          msgv1  = |{ iv_spmon+4(2) }.{ iv_spmon(4) }|
          msgv2  = |{ iv_lifnr }|.
    ENDIF.

    DATA it_rng_lifnr TYPE RANGE OF lfa1-lifnr.

    " проверка на отсутствие проводок в следующих периодах
    lv_future_post_cnt = mo_model->db_read_corr16_nrvrs_future( it_rng_lifnr[] ).

    IF lv_future_post_cnt > 0.
      " MESSAGE E010(ZMM_ACC16_WRITE_OFF).
      RAISE EXCEPTION TYPE zcx_acc16_write_off
        EXPORTING
          textid = zcx_acc16_write_off=>in_next_period_posings_exist
          msgv1  = |{ iv_lifnr }|.
    ENDIF.

  ENDMETHOD.                    "check_before_reversing


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ACC_16_WRITE_OFF_BL->CHECK_NEG_VAL_W_STOCK_16_FIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_OUTDATA                     TYPE        ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
* | [!CX!] ZCX_ACC16_WRITE_OFF
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD check_neg_val_w_stock_16_fin.
  FIELD-SYMBOLS <ls_outdata> LIKE LINE OF it_outdata[].
  DATA ls_msg TYPE symsg.

  LOOP AT it_outdata[] ASSIGNING <ls_outdata> WHERE w_stock_16_fin < 0.

    ls_msg-msgv1  = |{ <ls_outdata>-spmon+4(2) }.{ <ls_outdata>-spmon(4) }| .
    ls_msg-msgv2  = |{ <ls_outdata>-lifnr }|                                .
    ls_msg-msgv3  = |{ <ls_outdata>-w_stock_16_fin }|                       .

    MESSAGE
      ID zcx_acc16_write_off=>neg_w_stock_16_fin-msgid
      TYPE 'W'
      NUMBER zcx_acc16_write_off=>neg_w_stock_16_fin-msgno
      WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 INTO sy-msgli.
    mo_log->add_symsg(  ).
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ACC_16_WRITE_OFF_BL->CHECK_NRVRS_CORR16
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SPMON                       TYPE        SPMON
* | [--->] IV_LIFNR                       TYPE        LIFNR
* | [--->] IT_OUTDATA                     TYPE        ZCL_ACC16_WRITE_OFF_M=>TY_T_OUTDATA
* | [!CX!] ZCX_ACC16_WRITE_OFF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_nrvrs_corr16.
    FIELD-SYMBOLS:
          <ls_output> LIKE LINE OF it_outdata[].

    LOOP AT it_outdata[] ASSIGNING <ls_output>  WHERE spmon = iv_spmon
                                                  AND lifnr = iv_lifnr.

      IF <ls_output>-belnr_kor16 IS NOT INITIAL AND
         <ls_output>-gjahr_kor16 IS NOT INITIAL AND
         <ls_output>-belnr_kor16_st IS INITIAL  AND
         <ls_output>-gjahr_kor16_st IS INITIAL.

        " MESSAGE e003(zmm_acc16_write_off)
        RAISE EXCEPTION TYPE zcx_acc16_write_off
          EXPORTING
            textid = zcx_acc16_write_off=>nrvrs_corr16
            msgv1  = |{ iv_spmon+4(2) }.{ iv_spmon(4) }|
            msgv2  = |{ iv_lifnr }|
            msgv3  = |{ <ls_output>-belnr_kor16 }|.

      ELSEIF <ls_output>-belnr_fi IS NOT INITIAL AND <ls_output>-belnr_st IS INITIAL.

        " В периоде & по поставщику & уже произведен расчет ретрокоррекции
        " MESSAGE e138(zmm2)
        RAISE EXCEPTION TYPE zcx_acc16_write_off
          EXPORTING
            textid = zcx_acc16_write_off=>nrvrs_writeoff
            msgv1  = |{ iv_spmon+4(2) }.{ iv_spmon(4) }|
            msgv2  = |{ iv_lifnr }|
            msgv3  = |{ <ls_output>-belnr_fi }|.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "check_nrvrs_corr16


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ACC_16_WRITE_OFF_BL->CHECK_NRVRS_RETRO_PREV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ZMMCORR16                   TYPE        ZT_ZMMCORR16
* | [!CX!] ZCX_ACC16_WRITE_OFF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_nrvrs_retro_prev.
    FIELD-SYMBOLS <ls_zmmcorr16> LIKE LINE OF it_zmmcorr16[].

    IF lines( it_zmmcorr16[] ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_zmmcorr16[] ASSIGNING <ls_zmmcorr16>
                            WHERE belnr_fi IS INITIAL
                               OR belnr_fi IS NOT INITIAL AND belnr_st IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_acc16_write_off
        EXPORTING
          textid = zcx_acc16_write_off=>prev_ncalc
          msgv1  = |{ <ls_zmmcorr16>-spmon+4(2) }.{ <ls_zmmcorr16>-spmon(4) }|
          msgv2  = |{ <ls_zmmcorr16>-lifnr }|.
    ENDLOOP.
  ENDMETHOD.                    "check_nrvrs_retro_prev


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SSC                         TYPE REF TO ZCL_ACC16_WRITE_OFF_SSC
* | [--->] IO_LOG                         TYPE REF TO ZIF_APP_LOG
* | [--->] IO_MODEL                       TYPE REF TO ZCL_ACC16_WRITE_OFF_M
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mo_ssc = io_ssc.
    mo_log = io_log.
    mo_model = io_model.
  ENDMETHOD.                    "constructor


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ACC_16_WRITE_OFF_BL->RESET_ANSWER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD reset_answer.
  CLEAR m_neg_answ.
ENDMETHOD.
ENDCLASS.
