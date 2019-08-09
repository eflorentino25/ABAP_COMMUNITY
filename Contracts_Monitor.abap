*&---------------------------------------------------------------------*
*& Report  ZBRSDR203
*&
*&---------------------------------------------------------------------*
*& Author: Eduardo D. Florentino
*& Date: 18/Jan/2018
*& Projeto: Proposta BIOSEV.
*&---------------------------------------------------------------------*

REPORT  ZBRSDR203.

*----------------------------------------------------------------------*
*                             TABELAS                                  *
*----------------------------------------------------------------------*
TABLES: vbak,vbap, rlgrap,zbrsdt600.

*----------------------------------------------------------------------*
*                          TELA DE SELEÇÃO                             *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS:
  s_bukrs       FOR vbak-bukrs_vf,
  s_vbeln       FOR vbak-vbeln,
  s_erdat       FOR vbak-erdat,
  s_vbtyp       FOR vbak-vbtyp.
SELECTION-SCREEN END OF BLOCK bl1.

*----------------------------------------------------------------------*
*                   EXECUÇÃO PRINCIPAL DO PROGRAMA                     *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM f_select_data.
  PERFORM f_exibit_alv.

*----------------------------------------------------------------------*
*                              INCLUDES                                *
*----------------------------------------------------------------------*

INCLUDE zzbrsdi203_top.
INCLUDE zzbrsdi203_class.
INCLUDE zzbrsdi203_o01.
INCLUDE zzbrsdi203_i01.
INCLUDE zzbrsdi203_f01.



*&---------------------------------------------------------------------*
*&  Include           ZZBRSDI203_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS : slis.

TYPES:
BEGIN OF ty_vbak,
  vbeln TYPE vbak-vbeln          ,
  erdat TYPE vbak-erdat          ,
  erzet TYPE vbak-erzet          ,
  ernam TYPE vbak-ernam          ,
  angdt TYPE vbak-angdt          ,
  bnddt TYPE vbak-bnddt          ,
  audat TYPE vbak-audat          ,
  vbtyp TYPE vbak-vbtyp          ,
  trvog TYPE vbak-trvog          ,
  auart TYPE vbak-auart          ,
  augru TYPE vbak-augru          ,
  vkorg TYPE vbak-vkorg          ,
  vtweg TYPE vbak-vtweg          ,
  spart TYPE vbak-spart          ,
  vgbel TYPE vbak-vgbel          ,
  bukrs_vf TYPE vbak-bukrs_vf    ,

  END OF ty_vbak,

BEGIN OF ty_veda,
  vbeln   TYPE veda-vbeln     ,
  vbegdat TYPE veda-vbegdat   ,
  venddat TYPE veda-venddat   ,
  END OF ty_veda,

 BEGIN OF ty_vbap,
  vbeln TYPE vbap-vbeln       ,
  posnr TYPE vbap-posnr       ,
  matnr TYPE vbap-matnr       ,
  zmeng TYPE vbap-zmeng       ,
  zieme TYPE vbap-zieme       ,
END OF ty_vbap,

  BEGIN OF ty_outtab,
    status         TYPE zbrsdt600-status    ,
    vbeln          TYPE vbak-vbeln          ,
    posnr          TYPE vbap-posnr          ,
    matnr          TYPE vbap-matnr          ,
    zmeng          TYPE vbap-zmeng          ,
    zieme          TYPE vbap-zieme          ,
    vbeln_o        TYPE vbak-vbeln          ,
    zmeng_o        TYPE vbap-zmeng          ,
    zieme_o        TYPE vbap-zieme          ,
    bukrs_vf       TYPE vbak-bukrs_vf       ,
    vbegdat        TYPE veda-vbegdat        ,
    venddat        TYPE veda-venddat        ,
    vkorg          TYPE vbak-vkorg          ,
    vtweg          TYPE vbak-vtweg          ,
    spart          TYPE vbak-spart          ,
  END OF ty_outtab,

  BEGIN OF ty_soma_qt,
            matnr     TYPE vbap-matnr                     ,
            zmeng     TYPE vbap-zmeng                     ,
         END OF ty_soma_qt.






DATA:
      it_vbak        TYPE TABLE OF ty_vbak,
      it_vbak_orders TYPE TABLE OF ty_vbak,
      it_vbak_aux    TYPE TABLE OF ty_vbak,
      it_vbap        TYPE TABLE OF ty_vbap,
      it_vbap_orders  TYPE TABLE OF ty_vbap,
      it_vbap_aux    TYPE TABLE OF ty_vbap,
      it_veda        TYPE TABLE OF ty_veda,
      it_fieldcat    TYPE          slis_t_fieldcat_alv,
      it_fieldcat2   TYPE          lvc_t_fcat        WITH HEADER LINE,
      it_index_rows  TYPE lvc_t_row,                                                          " Tabela - Leitura ALV
      it_row_no      TYPE lvc_t_roid,
      it_somaqt      TYPE SORTED TABLE OF ty_soma_qt WITH UNIQUE KEY matnr  ,
      it_qt_vbap     TYPE SORTED TABLE OF ty_soma_qt WITH UNIQUE KEY matnr  ,
      it_saida       TYPE TABLE OF zbrsdt600,
      it_log_cont    TYPE TABLE OF zbrsdt601,
      it_outtab      TYPE TABLE OF ty_outtab,
      it_outtab2     TYPE TABLE OF ty_outtab.

DATA : wa_vbak            LIKE LINE OF it_vbak,
       wa_vbak_orders     LIKE LINE OF it_vbak,
       wa_vbap            LIKE LINE OF it_vbap,
       wa_vbap_aux        LIKE LINE OF it_vbap,
       wa_vbap_orders     LIKE LINE OF it_vbap,
       wa_veda            LIKE LINE OF it_veda,
       wa_somaqt          LIKE LINE OF it_somaqt,
       wa_qt_vbap         LIKE LINE OF it_qt_vbap,
       wa_saida           TYPE zbrsdt600,
       wa_variant         TYPE disvariant,
       wa_layout          TYPE         slis_layout_alv,
       wa_fieldcat        TYPE         slis_fieldcat_alv,
       wa_row_no           LIKE LINE OF it_row_no         ,
       wa_outtab          TYPE         ty_outtab,
       wa_outtab2         TYPE         ty_outtab.

DATA:
      lv_vbeln_check TYPE vbak-vbeln,
      lv_res         TYPE c,
      lv_counter     TYPE string,
      lv_message     TYPE string,
      lv_info_flag   TYPE boolean,
      lv_info_flag2  TYPE boolean,
      lv_orderqt     TYPE vbap-zmeng ,
      v_status                TYPE        string                    ,
      v_msg                   TYPE        string                    ,
      v_grid2                 TYPE REF TO cl_gui_alv_grid           , "Logs do process.
      v_dialogbox_container2  TYPE REF TO cl_gui_dialogbox_container, "Logs de Process.
      v_cockpit               TYPE        scrfname VALUE 'ALVGRID'  ,
      v_cockpit_log           TYPE        scrfname VALUE 'ALVLOG'   ,
      v_custom_container      TYPE REF TO cl_gui_custom_container   ,
      v_custom_container2     TYPE REF TO cl_gui_custom_container   ,
      v_grid1                 TYPE REF TO cl_gui_alv_grid           ,
      v_grid3                 TYPE REF TO cl_gui_alv_grid           ,
      v_layout                TYPE        lvc_s_layo                ,
      v_dialogbox_status      TYPE        c                         ,
      v_filename              TYPE        string                    ,
      v_repid                 LIKE        sy-repid                  ,
      v_layout2               TYPE        lvc_s_layo                , " Logs do Process
      v_layout3               TYPE        lvc_s_layo                , " Logs do Process
      v_empresa               TYPE        bukrs                     ,
      v_tipo_transp           TYPE        versarts                  ,
      v_subrc                 TYPE        i                         ,
      v_gui_timer             TYPE REF TO cl_gui_timer              ,
      v_resp                  TYPE        c                         ,
      v_dif                   TYPE        brgew_15                  ,
      v_dif_peso              TYPE        brgew_15                  ,
      v_perc                  TYPE        toleranz                  ,
      v_med_para              TYPE        meins                     ,
      v_dt_venc_cte           TYPE        sy-datum                  ,
      v_cfop_miro             TYPE        j_1bcfop                  ,
      v_iva                   TYPE        mwskz                     ,
      v_matnr                 TYPE        matnr                     ,
      v_valid                                                       ,
      v_tela                                                        .


CONSTANTS:
  c_x                          TYPE c             VALUE 'X'              ,
  c_a                          TYPE c             VALUE 'A'              ,
  c_s                          TYPE c             VALUE 'S'              ,
  c_p                          TYPE c             VALUE 'P'              ,
  c_e                          TYPE c             VALUE 'E'              ,
  c_i                          TYPE c             VALUE 'I'              ,
  c_col(3)                     TYPE c             VALUE 'COL'            ,
  c_icon_okay(04)              TYPE c             VALUE '@08@'           ,
  c_icon_error(04)             TYPE c             VALUE '@0A@'           ,
  c_icon_alert(04)             TYPE c             VALUE '@09@'           ,
  c_icon_n_exec(04)            TYPE c             VALUE '@00@'           ,
  c_vbeln(05)                  TYPE c             VALUE 'VBELN'          ,
  c_vbeln_o(05)                TYPE c             VALUE 'VBELN_O'        ,
  c_program                    TYPE slis_formname VALUE 'Z_PF_STATUS_001',
  c_tab_plan_mime              TYPE dd02l-tabname VALUE 'ZBRSDT600'      ,
  c_st_monitor                 TYPE dd02l-tabname VALUE 'ZBRSDS600'      ,
  c_tab_log_cte                TYPE dd02l-tabname VALUE 'ZBRSDS601'      ,

  c_program_uc                 TYPE slis_formname VALUE 'Z_USER_COMMAND' .



*&---------------------------------------------------------------------*
*&  Include           ZZBRSDI203_CLASS
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: on_finished FOR EVENT finished OF cl_gui_timer
                                IMPORTING sender.

    METHODS:

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row e_column,
    handle_close
        FOR EVENT close OF cl_gui_dialogbox_container
            IMPORTING sender,

     handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,


  handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.




  PRIVATE SECTION.
    DATA: v_dialogbox_status TYPE c.
    DATA: v_dialogbox_status2 TYPE c.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* §3.At doubleclick(1): The event DOUBLE_CLICK provides
*    parameters of the clicked row and column.
*    Use row parameter to select a line of the
*    corresponding internal table.

  METHOD handle_double_click.

*    IF e_column NE c_simp.
*      IF v_dialogbox_container2 IS INITIAL.
*        PERFORM f_log_execute USING e_column e_row.
*        CHECK NOT it_log[] IS INITIAL.
*        PERFORM f_create_detail_list.
*      ELSE.
*        PERFORM f_log_execute USING e_column e_row.
*        CHECK NOT it_log[] IS INITIAL.
*        CALL METHOD v_dialogbox_container2->set_visible
*          EXPORTING
*            visible = abap_on.
*        CALL METHOD v_grid2->refresh_table_display.
*      ENDIF.
*    ELSE.
*      MESSAGE s024(sd) WITH text-026 DISPLAY LIKE c_e.
*    ENDIF.

  ENDMETHOD.                    "handle_double_click
*--------------------------------------------------------*
  METHOD handle_close.
    CALL METHOD sender->set_visible
      EXPORTING
        visible = space.

  ENDMETHOD.                    "handle_close


  METHOD handle_toolbar.


  ENDMETHOD.                    "handle_toolbar


*-------------------------------------------------------------------
  METHOD handle_user_command.



  ENDMETHOD.                           "handle_user_command
  METHOD on_finished.
    PERFORM f_select_data.
    IF v_grid1 IS INITIAL.
*     Get the current grid control instance
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = v_grid1.
    ENDIF.
*   Refresh
    v_grid1->refresh_table_display( ).
*   Start timer again
    sender->run( ).
  ENDMETHOD.                           " on_finished

METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click
       USING es_row_no-row_id
             e_column_id-fieldname.
  ENDMETHOD.



ENDCLASS.                    "lcl_event_receiv




*&---------------------------------------------------------------------*
*&  Include           ZZBRSDI203_F01
*&
*&---------------------------------------------------------------------*


FORM f_select_data.
  SELECT vbeln erdat erzet ernam angdt bnddt audat vbtyp
         trvog auart augru vkorg vtweg spart vgbel bukrs_vf
    FROM vbak
    INTO TABLE it_vbak
    WHERE  bukrs_vf IN s_bukrs AND
           vbeln    IN s_vbeln AND
           erdat    IN s_erdat AND
           vbtyp    IN s_vbtyp.

  IF NOT it_vbak IS INITIAL.
    FREE: it_vbak_aux[].
    it_vbak_aux[] = it_vbak[].
    SORT it_vbak_aux BY vbeln .
    DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING vbeln.

    SELECT vbeln vbegdat venddat
        FROM veda
        INTO TABLE it_veda
        FOR ALL ENTRIES IN it_vbak_aux
        WHERE vbeln EQ it_vbak_aux-vbeln.

    FREE: it_vbak_aux[].
    it_vbak_aux[] = it_vbak[].
    SORT it_vbak_aux BY vbeln .
    DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING vbeln.

    SELECT vbeln posnr matnr zmeng zieme
        FROM vbap
        INTO TABLE it_vbap
        FOR ALL ENTRIES IN it_vbak_aux
        WHERE vbeln EQ it_vbak_aux-vbeln.


    "Selecionar ordens relacionadas ao contrato.
    FREE: it_vbak_aux[].
    it_vbak_aux[] = it_vbak[].
    SORT it_vbak_aux BY vbeln .
    DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING vbeln.

    SELECT vbeln erdat erzet ernam angdt bnddt audat vbtyp
            trvog auart augru vkorg vtweg spart vgbel bukrs_vf
       FROM vbak
       INTO TABLE it_vbak_orders
       FOR ALL ENTRIES IN it_vbak_aux
       WHERE vgbel = it_vbak_aux-vbeln.


    it_vbak_aux[] = it_vbak_orders[].
    SORT it_vbak_aux BY vbeln .
    DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING vbeln .

    SELECT vbeln posnr matnr zmeng zieme
        FROM vbap
        INTO TABLE it_vbap_orders
        FOR ALL ENTRIES IN it_vbak_aux
        WHERE vbeln EQ it_vbak_aux-vbeln.



  ENDIF.

  SORT: it_vbap BY vbeln,
        it_vbap_orders BY vbeln,
        it_vbak_orders BY vgbel,
        it_veda BY vbeln,
        it_vbak BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_vbak COMPARING vbeln.
  FREE it_vbak_aux[].

  LOOP AT it_vbak INTO wa_vbak.

    READ TABLE it_veda INTO wa_veda WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    PERFORM: f_valida_status.


  ENDLOOP.
  PERFORM f_exibe_msg.

ENDFORM.                    "f_select_data
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*Confere se a quantidade do contrato foi atingida, está parcialmente cumprida ou ainda não foi efetuada.
*----------------------------------------------------------------------*
FORM f_valida_status .
  CLEAR: lv_vbeln_check.
*   Soma toda quantia de cada item do contrato por matnr
  READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_vbeln_check = wa_vbap-vbeln.
  ENDIF.
  LOOP AT it_vbap INTO wa_vbap_aux FROM sy-tabix.
    IF wa_vbap_aux-vbeln NE lv_vbeln_check .
      EXIT.
    ELSE.
      wa_qt_vbap-matnr = wa_vbap_aux-matnr.
      wa_qt_vbap-zmeng = wa_vbap_aux-zmeng.
      COLLECT wa_qt_vbap INTO it_qt_vbap.
    ENDIF.
  ENDLOOP.
*    Soma a quantidade de cada um dos itens para todas as vendas relacionadas ao contrato.
  READ TABLE it_vbak_orders INTO wa_vbak_orders WITH KEY vgbel = wa_vbak-vbeln BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_vbeln_check = wa_vbak-vbeln.

    LOOP AT it_vbak_orders INTO wa_vbak_orders FROM sy-tabix.
      IF wa_vbak_orders-vgbel EQ lv_vbeln_check.
        READ TABLE it_vbap_orders INTO wa_vbap_orders WITH KEY vbeln = wa_vbak_orders-vbeln.
        wa_somaqt-matnr = wa_vbap_orders-matnr.
        wa_somaqt-zmeng = wa_vbap_orders-zmeng.
        COLLECT wa_somaqt INTO it_somaqt.
        "Atualiza campos do ALV.
        wa_outtab-vbeln           =  wa_vbak-vbeln            .
        wa_outtab-posnr           =  wa_vbap-posnr            .
        wa_outtab-matnr           =  wa_vbap-matnr            .
        wa_outtab-zmeng           =  wa_vbap-zmeng            .
        wa_outtab-zieme           =  wa_vbap-zieme            .
        wa_outtab-vbeln_o         =  wa_vbak_orders-vbeln     .
        wa_outtab-zmeng_o         =  wa_vbap_orders-zmeng     .
        wa_outtab-zieme_o         =  wa_vbap_orders-zieme     .
        wa_outtab-bukrs_vf        =  wa_vbak-bukrs_vf         .
        wa_outtab-vbegdat         =  wa_veda-vbegdat          .
        wa_outtab-venddat         =  wa_veda-venddat          .
        wa_outtab-vkorg           =  wa_vbak-vkorg            .
        wa_outtab-vtweg           =  wa_vbak-vtweg            .
        wa_outtab-spart           =  wa_vbak-spart            .
        APPEND wa_outtab TO it_outtab.
        CLEAR: wa_vbak_orders,
               wa_vbap_orders.

      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
*  Caso não haja OV para o contrato, gravar no monitor vazio.
    CLEAR: wa_vbak_orders,
           wa_vbap_orders.

    wa_outtab-vbeln           =  wa_vbak-vbeln            .
    wa_outtab-posnr           =  wa_vbap-posnr            .
    wa_outtab-matnr           =  wa_vbap-matnr            .
    wa_outtab-zmeng           =  wa_vbap-zmeng            .
    wa_outtab-zieme           =  wa_vbap-zieme            .
    wa_outtab-bukrs_vf        =  wa_vbak-bukrs_vf         .
    wa_outtab-vbegdat         =  wa_veda-vbegdat          .
    wa_outtab-venddat         =  wa_veda-venddat          .
    wa_outtab-vkorg           =  wa_vbak-vkorg            .
    wa_outtab-vtweg           =  wa_vbak-vtweg            .
    wa_outtab-spart           =  wa_vbak-spart            .

    APPEND wa_outtab TO it_outtab.

  ENDIF.
  FREE: it_vbap_orders.



  IF NOT it_somaqt IS INITIAL.
    LOOP AT it_somaqt INTO wa_somaqt.
      READ TABLE it_qt_vbap INTO wa_qt_vbap WITH KEY matnr = wa_somaqt-matnr.
      IF sy-subrc IS INITIAL.
        IF wa_somaqt-zmeng EQ wa_qt_vbap-zmeng.        "completo para esse MATNR
          CONCATENATE lv_counter c_s INTO lv_counter.
        ELSEIF wa_somaqt-zmeng NE wa_qt_vbap-zmeng AND "Parcial para esse MATNR
               NOT wa_somaqt-zmeng IS INITIAL.
          CONCATENATE lv_counter c_p INTO lv_counter.
        ELSEIF wa_somaqt-zmeng IS INITIAL.              "Nada para este MATNR
          CONCATENATE lv_counter c_e INTO lv_counter.
        ENDIF.
      ELSE.

        CONCATENATE lv_message c_a INTO lv_message."Para identificar o tipo de mensagem a ser apresentado.
        DELETE it_outtab WHERE vbeln = wa_vbak-vbeln AND matnr = wa_somaqt-matnr OR matnr = wa_qt_vbap-matnr.

      ENDIF.
    ENDLOOP.

    IF     lv_counter  CO  c_s.
      wa_outtab-status =    c_icon_okay.
    ELSEIF lv_counter  CA  c_p .
      wa_outtab-status =    c_icon_alert.
    ELSE.
      wa_outtab-status =    c_icon_error.
    ENDIF.
  ELSE.
    "Para identificar o tipo de mensagem a ser apresentado.
    CONCATENATE lv_message c_e INTO lv_message.
    wa_outtab-status =   c_icon_error.
  ENDIF.

  MODIFY it_outtab FROM wa_outtab TRANSPORTING status WHERE vbeln = wa_vbak-vbeln.

  CLEAR: wa_somaqt,
         wa_vbap,
         wa_vbak,
         lv_counter,
         wa_outtab,
         wa_qt_vbap.
  FREE: it_somaqt ,
        it_qt_vbap .

ENDFORM.                    " F_VALIDA_STATUS

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click  USING    p_row_id
                                    p_fieldname.

  READ TABLE it_outtab INTO wa_outtab INDEX p_row_id.

  CASE p_fieldname.
    WHEN c_vbeln.
      CHECK NOT wa_outtab-vbeln IS INITIAL.
      SET PARAMETER ID : 'KTN' FIELD wa_outtab-vbeln.
      CALL TRANSACTION 'VA43' AND SKIP FIRST SCREEN.

    WHEN c_vbeln_o.

      CHECK NOT wa_outtab-vbeln_o IS INITIAL.
      SET PARAMETER ID : 'KTN' FIELD wa_outtab-vbeln_o.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibe_msg .

  IF lv_message CO c_a.
    MESSAGE text-000 TYPE c_i.

  ELSEIF lv_message CO c_e.
    MESSAGE text-002 TYPE c_i.


  ELSEIF lv_message CA c_a AND
         lv_message CA c_e.
    MESSAGE text-003 TYPE c_i.

  ENDIF.
ENDFORM.                    " F_EXIBE_MSG


*&---------------------------------------------------------------------*
*&  Include           ZZBRSDI203_F01
*&
*&---------------------------------------------------------------------*


FORM f_select_data.
  SELECT vbeln erdat erzet ernam angdt bnddt audat vbtyp
         trvog auart augru vkorg vtweg spart vgbel bukrs_vf
    FROM vbak
    INTO TABLE it_vbak
    WHERE  bukrs_vf IN s_bukrs AND
           vbeln    IN s_vbeln AND
           erdat    IN s_erdat AND
           vbtyp    IN s_vbtyp.

  IF NOT it_vbak IS INITIAL.
    FREE: it_vbak_aux[].
    it_vbak_aux[] = it_vbak[].
    SORT it_vbak_aux BY vbeln .
    DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING vbeln.

    SELECT vbeln vbegdat venddat
        FROM veda
        INTO TABLE it_veda
        FOR ALL ENTRIES IN it_vbak_aux
        WHERE vbeln EQ it_vbak_aux-vbeln.

    FREE: it_vbak_aux[].
    it_vbak_aux[] = it_vbak[].
    SORT it_vbak_aux BY vbeln .
    DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING vbeln.

    SELECT vbeln posnr matnr zmeng zieme
        FROM vbap
        INTO TABLE it_vbap
        FOR ALL ENTRIES IN it_vbak_aux
        WHERE vbeln EQ it_vbak_aux-vbeln.


    "Selecionar ordens relacionadas ao contrato.
    FREE: it_vbak_aux[].
    it_vbak_aux[] = it_vbak[].
    SORT it_vbak_aux BY vbeln .
    DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING vbeln.

    SELECT vbeln erdat erzet ernam angdt bnddt audat vbtyp
            trvog auart augru vkorg vtweg spart vgbel bukrs_vf
       FROM vbak
       INTO TABLE it_vbak_orders
       FOR ALL ENTRIES IN it_vbak_aux
       WHERE vgbel = it_vbak_aux-vbeln.


    it_vbak_aux[] = it_vbak_orders[].
    SORT it_vbak_aux BY vbeln .
    DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING vbeln .

    SELECT vbeln posnr matnr zmeng zieme
        FROM vbap
        INTO TABLE it_vbap_orders
        FOR ALL ENTRIES IN it_vbak_aux
        WHERE vbeln EQ it_vbak_aux-vbeln.



  ENDIF.

  SORT: it_vbap BY vbeln,
        it_vbap_orders BY vbeln,
        it_vbak_orders BY vgbel,
        it_veda BY vbeln,
        it_vbak BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_vbak COMPARING vbeln.
  FREE it_vbak_aux[].

  LOOP AT it_vbak INTO wa_vbak.

    READ TABLE it_veda INTO wa_veda WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    PERFORM: f_valida_status.


  ENDLOOP.
  PERFORM f_exibe_msg.

ENDFORM.                    "f_select_data
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*Confere se a quantidade do contrato foi atingida, está parcialmente cumprida ou ainda não foi efetuada.
*----------------------------------------------------------------------*
FORM f_valida_status .
  CLEAR: lv_vbeln_check.
*   Soma toda quantia de cada item do contrato por matnr
  READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_vbeln_check = wa_vbap-vbeln.
  ENDIF.
  LOOP AT it_vbap INTO wa_vbap_aux FROM sy-tabix.
    IF wa_vbap_aux-vbeln NE lv_vbeln_check .
      EXIT.
    ELSE.
      wa_qt_vbap-matnr = wa_vbap_aux-matnr.
      wa_qt_vbap-zmeng = wa_vbap_aux-zmeng.
      COLLECT wa_qt_vbap INTO it_qt_vbap.
    ENDIF.
  ENDLOOP.
*    Soma a quantidade de cada um dos itens para todas as vendas relacionadas ao contrato.
  READ TABLE it_vbak_orders INTO wa_vbak_orders WITH KEY vgbel = wa_vbak-vbeln BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    lv_vbeln_check = wa_vbak-vbeln.

    LOOP AT it_vbak_orders INTO wa_vbak_orders FROM sy-tabix.
      IF wa_vbak_orders-vgbel EQ lv_vbeln_check.
        READ TABLE it_vbap_orders INTO wa_vbap_orders WITH KEY vbeln = wa_vbak_orders-vbeln.
        wa_somaqt-matnr = wa_vbap_orders-matnr.
        wa_somaqt-zmeng = wa_vbap_orders-zmeng.
        COLLECT wa_somaqt INTO it_somaqt.
        "Atualiza campos do ALV.
        wa_outtab-vbeln           =  wa_vbak-vbeln            .
        wa_outtab-posnr           =  wa_vbap-posnr            .
        wa_outtab-matnr           =  wa_vbap-matnr            .
        wa_outtab-zmeng           =  wa_vbap-zmeng            .
        wa_outtab-zieme           =  wa_vbap-zieme            .
        wa_outtab-vbeln_o         =  wa_vbak_orders-vbeln     .
        wa_outtab-zmeng_o         =  wa_vbap_orders-zmeng     .
        wa_outtab-zieme_o         =  wa_vbap_orders-zieme     .
        wa_outtab-bukrs_vf        =  wa_vbak-bukrs_vf         .
        wa_outtab-vbegdat         =  wa_veda-vbegdat          .
        wa_outtab-venddat         =  wa_veda-venddat          .
        wa_outtab-vkorg           =  wa_vbak-vkorg            .
        wa_outtab-vtweg           =  wa_vbak-vtweg            .
        wa_outtab-spart           =  wa_vbak-spart            .
        APPEND wa_outtab TO it_outtab.
        CLEAR: wa_vbak_orders,
               wa_vbap_orders.

      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
*  Caso não haja OV para o contrato, gravar no monitor vazio.
    CLEAR: wa_vbak_orders,
           wa_vbap_orders.

    wa_outtab-vbeln           =  wa_vbak-vbeln            .
    wa_outtab-posnr           =  wa_vbap-posnr            .
    wa_outtab-matnr           =  wa_vbap-matnr            .
    wa_outtab-zmeng           =  wa_vbap-zmeng            .
    wa_outtab-zieme           =  wa_vbap-zieme            .
    wa_outtab-bukrs_vf        =  wa_vbak-bukrs_vf         .
    wa_outtab-vbegdat         =  wa_veda-vbegdat          .
    wa_outtab-venddat         =  wa_veda-venddat          .
    wa_outtab-vkorg           =  wa_vbak-vkorg            .
    wa_outtab-vtweg           =  wa_vbak-vtweg            .
    wa_outtab-spart           =  wa_vbak-spart            .

    APPEND wa_outtab TO it_outtab.

  ENDIF.
  FREE: it_vbap_orders.



  IF NOT it_somaqt IS INITIAL.
    LOOP AT it_somaqt INTO wa_somaqt.
      READ TABLE it_qt_vbap INTO wa_qt_vbap WITH KEY matnr = wa_somaqt-matnr.
      IF sy-subrc IS INITIAL.
        IF wa_somaqt-zmeng EQ wa_qt_vbap-zmeng.        "completo para esse MATNR
          CONCATENATE lv_counter c_s INTO lv_counter.
        ELSEIF wa_somaqt-zmeng NE wa_qt_vbap-zmeng AND "Parcial para esse MATNR
               NOT wa_somaqt-zmeng IS INITIAL.
          CONCATENATE lv_counter c_p INTO lv_counter.
        ELSEIF wa_somaqt-zmeng IS INITIAL.              "Nada para este MATNR
          CONCATENATE lv_counter c_e INTO lv_counter.
        ENDIF.
      ELSE.

        CONCATENATE lv_message c_a INTO lv_message."Para identificar o tipo de mensagem a ser apresentado.
        DELETE it_outtab WHERE vbeln = wa_vbak-vbeln AND matnr = wa_somaqt-matnr OR matnr = wa_qt_vbap-matnr.

      ENDIF.
    ENDLOOP.

    IF     lv_counter  CO  c_s.
      wa_outtab-status =    c_icon_okay.
    ELSEIF lv_counter  CA  c_p .
      wa_outtab-status =    c_icon_alert.
    ELSE.
      wa_outtab-status =    c_icon_error.
    ENDIF.
  ELSE.
    "Para identificar o tipo de mensagem a ser apresentado.
    CONCATENATE lv_message c_e INTO lv_message.
    wa_outtab-status =   c_icon_error.
  ENDIF.

  MODIFY it_outtab FROM wa_outtab TRANSPORTING status WHERE vbeln = wa_vbak-vbeln.

  CLEAR: wa_somaqt,
         wa_vbap,
         wa_vbak,
         lv_counter,
         wa_outtab,
         wa_qt_vbap.
  FREE: it_somaqt ,
        it_qt_vbap .

ENDFORM.                    " F_VALIDA_STATUS

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click  USING    p_row_id
                                    p_fieldname.

  READ TABLE it_outtab INTO wa_outtab INDEX p_row_id.

  CASE p_fieldname.
    WHEN c_vbeln.
      CHECK NOT wa_outtab-vbeln IS INITIAL.
      SET PARAMETER ID : 'KTN' FIELD wa_outtab-vbeln.
      CALL TRANSACTION 'VA43' AND SKIP FIRST SCREEN.

    WHEN c_vbeln_o.

      CHECK NOT wa_outtab-vbeln_o IS INITIAL.
      SET PARAMETER ID : 'KTN' FIELD wa_outtab-vbeln_o.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.


    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibe_msg .

  IF lv_message CO c_a.
    MESSAGE text-000 TYPE c_i.

  ELSEIF lv_message CO c_e.
    MESSAGE text-002 TYPE c_i.


  ELSEIF lv_message CA c_a AND
         lv_message CA c_e.
    MESSAGE text-003 TYPE c_i.

  ENDIF.
ENDFORM.                    " F_EXIBE_MSG

*&---------------------------------------------------------------------*
*&  Include           ZZBRSDI203_I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 1000.
    WHEN 'REPROCESSA'.

      PERFORM f_select_data.
      PERFORM f_exibit_alv.

    WHEN 'PDFGEN'.
*      CALL FUNCTION
       CHECK it_vbak[] IS NOT INITIAL.


    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

MODULE user_command_1001 INPUT.

  DATA lv_extension TYPE c LENGTH 10.
  CASE sy-ucomm.
    WHEN 'REFRESH'.
      PERFORM f_select_data.
      CALL METHOD v_grid1->refresh_table_display.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           ZZBRSDI203_O01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  Z_ALL_ALV
*&---------------------------------------------------------------------*
FORM f_exibit_alv .

  PERFORM : z_fieldcat,  " Cria o fieldcat para o ALV
            z_layout,    " Cria o Layout do ALV.
            z_alv        " Cria o ALV
           .

ENDFORM.                    " Z_ALL_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_FIELDCAT
*&---------------------------------------------------------------------*
FORM z_fieldcat .
  PERFORM z_feed_fieldcat USING :
      "Fname       Seltext                         just|Col_pos|fix_colum|key_sel|hotspot|table_name
      'STATUS'     ' '                             'C'   1       ' '      ' '     ' '     'IT_OUTTAB'             ,
      'VBELN'      ' '                             'C'   2       ' '      ' '     'X'     'IT_OUTTAB'             ,
      'POSNR'      ' '                             'C'   3       ' '      ' '     ' '     'IT_OUTTAB'             ,
      'MATNR'      ' '                             'C'   4       ' '      ' '     ' '     'IT_OUTTAB'             ,
      'ZMENG'      ' '                             'C'   5       ' '      ' '     ' '     'IT_OUTTAB'             ,
      'ZIEME'      ' '                             'C'   6       ' '      ' '     ' '     'IT_OUTTAB'             ,
      'VBELN_O'    'Documento ordem venda '        'C'   7       ' '      ' '     'X'     'IT_OUTTAB'             ,
      'ZMENG_O'    ' '                             'C'   8       ' '      ' '     ' '     'IT_OUTTAB'             ,
      'ZIEME_O'    ' '                             'C'   9       ' '      ' '     ' '     'IT_OUTTAB'             ,
      'BUKRS_VF'   ' '                             'C'   10      ' '      ' '     ' '     'IT_OUTTAB'             ,
      'VBEGDAT'    ' '                             'C'   11      ' '      ' '     ' '     'IT_OUTTAB'             ,
      'VENDDAT'    ' '                             'C'   12      ' '      ' '     ' '     'IT_OUTTAB'             ,
      'VKORG'      ' '                             'C'   13      ' '      ' '     ' '     'IT_OUTTAB'             ,
      'VTWEG'      ' '                             'C'   14      ' '      ' '     ' '     'IT_OUTTAB'             ,
      'SPART'      ' '                             'C'   15      ' '      ' '     ' '     'IT_OUTTAB'
              .


*
*  'Status'
*  'Documento'
*  'Item'
*  'Material'
*  'Quantidade prevista'
*  'Unidade'
*  'Documento ordem venda'
*  'Quantidade Prevista'
*  'Unidade'
*  'Empresa faturadora'
*  'Início do contrato'
*  'Fim do contrato'
*  'Organização vendas'
*  'Canal distribuição'
*  'Setor de atividade'
ENDFORM.                    " Z_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_FEED_FIELDCAT
*&---------------------------------------------------------------------*
FORM z_feed_fieldcat  USING    p_fname
                               p_seltext
                               p_just
                               p_col_pos
                               p_fix_column
                               p_key_sel
                               p_hotspot
                               p_tabname

                                         .



  it_fieldcat2-fieldname     = p_fname.
  it_fieldcat2-cfieldname    = p_seltext.
  it_fieldcat2-just          = p_just.
  it_fieldcat2-col_pos       = p_col_pos.
  it_fieldcat2-fix_column    = p_fix_column.
  it_fieldcat2-key_sel       = p_key_sel.
  it_fieldcat2-hotspot       = p_hotspot .
  it_fieldcat2-tabname       = p_tabname .


*  wa_fieldcat-fieldname   = p_fname.
*  wa_fieldcat-seltext_m   = p_seltext.
*  wa_fieldcat-checkbox    = p_check.
*  wa_fieldcat-edit        = p_edit.
*  wa_fieldcat-col_pos     = p_col_pos.
*  wa_fieldcat-hotspot     = p_hotspot.

  APPEND it_fieldcat2.
  CLEAR  it_fieldcat2.


ENDFORM.                    " Z_FEED_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
FORM z_layout .

  wa_layout-colwidth_optimize = c_x.
  wa_layout-zebra             = c_x.


ENDFORM.                    " Z_LAYOUT

*----------------------------------------------------------------------*
*  MODULE status_1001 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  SET PF-STATUS 'STATUS_1001'.
  SET TITLEBAR 'TITULO_1001'.

ENDMODULE.                 " STATUS_1001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITULO_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1002 OUTPUT.
  SET PF-STATUS 'STATUS_1002'.
  SET TITLEBAR 'TITLE_1002'.

ENDMODULE.                 " STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1003 OUTPUT.
  SET PF-STATUS 'STATUS_1003'.
  SET TITLEBAR 'TITLE_1003'.

ENDMODULE.                 " STATUS_1003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1004 OUTPUT.
  SET PF-STATUS 'STATUS_1004'.
  SET TITLEBAR 'TITLE_1004'.

ENDMODULE.                 " STATUS_1004  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  Z_ALV
*&---------------------------------------------------------------------*
FORM z_alv .
  v_tela = 1.
  CALL SCREEN '1001'.


ENDFORM.                    " Z_ALV
*----------------------------------------------------------------------*
*  MODULE container_alv_1001 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE container_alv_1001 OUTPUT.

  DATA: lv_event_receiver    TYPE REF TO lcl_event_receiver,
        lv_timer  TYPE tvarvc-low.
  v_repid = sy-repid.
  IF v_custom_container IS INITIAL.

    CREATE OBJECT v_custom_container
      EXPORTING
        container_name              = v_cockpit
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = v_repid
          txt2  = sy-subrc
          txt1  = text-001.
    ENDIF.

    IF  v_grid1 IS INITIAL.
      CREATE OBJECT v_grid1
        EXPORTING
          i_parent = v_custom_container.
    ENDIF.

*..............................................
* set some layout-values (Structure LVC_S_LAYO)
*..............................................
    v_layout-zebra          =     abap_on.
    v_layout-box_fname      =     abap_on.
    v_layout-sel_mode       =     c_a.
    v_layout-info_fname      =    c_col. "'COL'.

*.............................
* show table on ALV Control
*.............................
    wa_variant-report = sy-repid.

    CALL METHOD v_grid1->set_table_for_first_display
      EXPORTING
        i_structure_name = c_st_monitor
        is_layout        = v_layout2 "2
        i_save           = c_a
        is_variant       = wa_variant
      CHANGING
        it_fieldcatalog  = it_fieldcat2[]
        it_outtab        = it_outtab[].
  ENDIF.

  CREATE OBJECT lv_event_receiver.
  CREATE OBJECT v_gui_timer.

  SET HANDLER   lv_event_receiver->handle_double_click FOR v_grid1.
* Instancia evento timer
  SET HANDLER lv_event_receiver->handle_hotspot_click for v_grid1.

  IF lv_timer IS NOT INITIAL.
    v_gui_timer->interval = lv_timer.
    v_gui_timer->run( ).
  ENDIF.

  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = v_grid1.

ENDMODULE.                 " CONTAINER_ALV_1001  OUTPUT

*----------------------------------------------------------------------*
*  MODULE container_alv_0100 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE container_alv_0100 OUTPUT.

  DATA: r_doc TYPE RANGE OF j_1bdocnum,
        wa_doc LIKE LINE OF r_doc.
*        it_t130 TYPE TABLE OF zbrsdt130.

  CLEAR: it_index_rows[], it_row_no[], wa_row_no, r_doc[],wa_saida.

  CALL METHOD v_grid1->get_selected_rows
    IMPORTING
      et_index_rows = it_index_rows
      et_row_no     = it_row_no.

  IF lines( it_row_no ) GT 1.
    MESSAGE s089(pd) DISPLAY LIKE c_e.
    RETURN.
  ELSEIF lines( it_row_no ) LT 1.
    MESSAGE s205(fagl_mm_recon) DISPLAY LIKE c_e.
    LEAVE TO SCREEN 0.
  ENDIF.

  READ TABLE it_row_no INTO wa_row_no INDEX 1.
  READ TABLE it_saida INTO wa_saida INDEX wa_row_no-row_id.

  SELECT *
      FROM zbrsdt601
      INTO TABLE it_log_cont
      WHERE vbeln EQ wa_saida-vbeln.

*  IF it_log_cont[] IS INITIAL.
*    SELECT *
*    FROM zbrsdt997 INTO TABLE it_log_cte
*    WHERE chave_cte EQ wa_saida-cte_chave.
*  ENDIF.

  IF it_log_cont[] IS NOT INITIAL.

    IF v_custom_container2 IS INITIAL.

      CREATE OBJECT v_custom_container2
        EXPORTING
          container_name              = v_cockpit_log
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

*      IF sy-subrc NE 0.
*        CALL FUNCTION 'POPUP_TO_INFORM'
*          EXPORTING
*            titel = v_repid
*            txt2  = sy-subrc
*            txt1  = text-021.
*      ENDIF.

      IF  v_grid3 IS INITIAL.
        CREATE OBJECT v_grid3
          EXPORTING
            i_parent = v_custom_container2.
      ENDIF.

*..............................................
* set some layout-values (Structure LVC_S_LAYO)
*..............................................
      v_layout3-zebra          =     abap_on.
      v_layout3-box_fname      =     abap_on.
      v_layout3-sel_mode       =     c_a.
      v_layout3-info_fname      =    c_col. "'COL'.

*.............................
* show table on ALV Control
*.............................
      wa_variant-report = sy-repid.

      CALL METHOD v_grid3->set_table_for_first_display
        EXPORTING
          i_structure_name = c_tab_log_cte
          is_layout        = v_layout3
          i_save           = c_a
          is_variant       = wa_variant
        CHANGING
          it_outtab        = it_log_cont[].
    ENDIF.

    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = v_grid3.

    CALL METHOD v_grid3->refresh_table_display
      EXPORTING
        i_soft_refresh = abap_true.
  ELSE.
    MESSAGE s111(wty) DISPLAY LIKE c_e.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " CONTAINER_ALV_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CONTAINER_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE container_alv_1003 OUTPUT.

  DATA: v_cockpit_sheet               TYPE        scrfname VALUE 'CC_PLANILHA_MIME',
        v_custom_container_sheet      TYPE REF TO cl_gui_custom_container,
        v_grid_sheet                  TYPE REF TO cl_gui_alv_grid.

  IF v_custom_container_sheet IS INITIAL.

    CREATE OBJECT v_custom_container_sheet
      EXPORTING
        container_name              = v_cockpit_sheet
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = v_repid
          txt2  = sy-subrc
          txt1  = text-021.
    ENDIF.

    IF  v_grid_sheet IS INITIAL.
      CREATE OBJECT v_grid_sheet
        EXPORTING
          i_parent = v_custom_container_sheet.
    ENDIF.

*..............................................
* set some layout-values (Structure LVC_S_LAYO)
*..............................................
    v_layout3-zebra          =     abap_on.
    v_layout3-box_fname      =     abap_on.
    v_layout3-sel_mode       =     c_a.
    v_layout3-cwidth_opt     =     abap_on.
    v_layout3-info_fname     =     c_col.  "'COL'.

*.............................
* show table on ALV Control
*.............................
    wa_variant-report = sy-repid.

    FREE it_fieldcat2.
*    PERFORM f_fill_fieldcat3 USING:
*        'STATUS_MIME'  'IT_LOG_CHECK'   'Status ME' '13' ,
*        'MSG'          'IT_LOG_CHECK'   'Mensagem'  '14' ,
*        'PLACA'        'IT_LOG_CHECK'   'Placa'     '06' .

    CALL METHOD v_grid_sheet->set_table_for_first_display
      EXPORTING
        i_structure_name = c_tab_plan_mime
        is_layout        = v_layout3
        i_save           = c_a
        is_variant       = wa_variant
      CHANGING
        it_fieldcatalog  = it_fieldcat2[]
        it_outtab        = it_log_cont[].
  ENDIF.

  CALL METHOD v_grid_sheet->refresh_table_display.

  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = v_grid_sheet.

ENDMODULE.                 " CONTAINER_ALV_1003  OUTPUT




