*&-------------------------------------------------------------------------*
*& Report  ZNFSE_CARGAXML
*&
*&-------------------------------------------------------------------------*
*& Eduardo F/Aires         Data: 15/06/2018    Chamado:XXXXX
*&Programa para executar carga manual XML de NFSE, na forma de contingência.
*&-------------------------------------------------------------------------*
* Notas: Este programa é feito para upload de notas NFSE no formato xml. É
* necessário que o nome do arquivo tenha 4 dígitos do número da prenota e
* _e ou _r exemplo 0001_e 0001_r. Todos os arquivos devem estar no mesmo di-
* retório.
REPORT znfse_cargaxml.

TYPES:
       BEGIN OF ty_xml                    ,
        xml            TYPE xstring       ,
       END OF ty_xml                      ,

*   Tabela com todos os campos do XML

       BEGIN OF ty_xmlfields,
        field01        TYPE string        ,
        field02        TYPE string        ,
        field03        TYPE string        ,
        field04        TYPE string        ,
        field05        TYPE string        ,
        field06        TYPE string        ,
        field07        TYPE string        ,
        field08        TYPE string        ,
        field09        TYPE string        ,
        field10        TYPE string        ,
        field11        TYPE string        ,
        field12        TYPE string        ,
       END OF ty_xmlfields.

DATA:
*      tabelas internas
      it_zsdnfsetws001in      TYPE TABLE OF     zsdnfsetws001in                         ,
      it_zsdnfset_logproc     TYPE TABLE OF     zsdnfset_logproc                        ,
      it_zsdnfset_monitor     TYPE TABLE OF     zsdnfset_monitor                        ,
      it_zsdnfsetprenotad     TYPE TABLE OF     zsdnfsetprenotad                        ,
      it_zsdnfsetws001out     TYPE TABLE OF     zsdnfsetws001out                        ,
      it_j_1bnfdoc            TYPE TABLE OF     j_1bnfdoc                               ,
      it_xml                  TYPE TABLE OF     ty_xml               WITH HEADER LINE   ,
      it_xml_e                TYPE TABLE OF     ty_xml               WITH HEADER LINE   ,
      it_xml_r                TYPE TABLE OF     ty_xml               WITH HEADER LINE   ,
      it_xml_data             TYPE TABLE OF     smum_xmltb                              ,
      it_xml_data2            TYPE TABLE OF     smum_xmltb                              ,
      it_xml_data_env         TYPE TABLE OF     smum_xmltb                              ,
      it_xml_data_ret         TYPE TABLE OF     smum_xmltb                              ,
      it_envio_all            TYPE TABLE OF     zsdnfsetws001in                         ,
      it_retorno_all          TYPE TABLE OF     ty_xmlfields                            ,
      it_field_table          TYPE TABLE OF     sdokpath                                ,
      it_dir_table            TYPE TABLE OF     sdokpath                                ,
      it_return               TYPE TABLE OF     bapiret2                                ,
      it_param                TYPE TABLE OF     znfset_tag_2_fld                        ,

*      estruturas
      wa_param                TYPE              znfset_tag_2_fld                        ,
      wa_envio_retorno        TYPE              zsdnfsetws001in                         ,
      wa_zsdnfsetws001in      TYPE              zsdnfsetws001in                         ,
      wa_zsdnfset_logproc     TYPE              zsdnfset_logproc                        ,
      wa_zsdnfset_monitor     TYPE              zsdnfset_monitor                        ,
      wa_zsdnfsetprenotad     TYPE              zsdnfsetprenotad                        ,
      wa_zsdnfsetws001out     TYPE              zsdnfsetws001out                        ,
      wa_j_1bnfdoc            TYPE              j_1bnfdoc                               ,
      wa_xml_data             TYPE              smum_xmltb                              ,
      wa_xml_data_srt         TYPE              smum_xmltb                              ,
      wa_field_table          TYPE              sdokpath                                ,
      wa_field_table_e        TYPE              sdokpath                                ,
*      variáveis
      v_n                     TYPE  i,
      v_xml                   TYPE REF TO       cl_xml_document                         ,
      v_subrc                 TYPE              sy-subrc                                ,
      v_xml_string            TYPE              xstring                                 ,
      v_size                  TYPE              sytabix                                 ,
      v_tabix                 TYPE              sytabix                                 ,
      v_envio_nome            TYPE              string                                  ,
      v_filename              TYPE              string                                  ,
      v_result                TYPE              file_table                              ,
      v_rc                    TYPE              i                                       ,
      v_filetable             TYPE              filetable                               ,

      v_carga                 TYPE              string                                  .

*      objetos referenciados

CONSTANTS:
*    Single Char
      c_i                     TYPE c            VALUE 'I'                                        ,
      c_e                                       VALUE 'E'                                        ,
*    Numéricas
      c_0                                       VALUE '0'                                        ,
      c_1                                       VALUE '1'                                        ,
      c_2                                       VALUE '2'                                        ,
      c_3                                       VALUE '3'                                        ,
      c_5                                       VALUE '5'                                        ,
      c_6                                       VALUE '6'                                        ,
      c_7                                       VALUE '7'                                        ,
      c_8                                       VALUE '8'                                        ,
*    Nomes tvarv + transações + outros
      c_env                  TYPE char3         VALUE '_e'                                       ,
      c_ret                  TYPE char3         VALUE '_r'                                       ,
      c_xml(5)                                  VALUE '*.xml'                                    ,
      c_barra                                   VALUE '\'                                        ,
      c_ponto_final                             VALUE '.'                                        ,
      c_semicolon                               VALUE ';'                                        ,
      c_virgula                                 VALUE ','                                        ,
      c_vb                   TYPE string        VALUE 'wa_zsdnfsetws001out-nfse_valor_bruto'     ,
      c_cnpj                 TYPE string        VALUE 'ws_001_in_tom_cpf_cnpj'                   ,
      c_cod_v                TYPE string        VALUE 'ws_001_in_pnfse_det_cod_val'              ,
      c_bloco                TYPE string        VALUE 'ws_001_in_pnfse_bloco_seq'                ,
      c_preno                TYPE string        VALUE 'ws_001_in_pnfse_det_seq'                  .



FIELD-SYMBOLS:
*              Tabelas
    <table>            TYPE           ANY TABLE   ,
*              WorkAreas
    <wa>               TYPE           data        ,
*              Campos
    <field>            TYPE           data        .


SELECTION-SCREEN: BEGIN OF BLOCK b04 WITH FRAME TITLE text-001.
PARAMETER: p_xml_tl TYPE rlgrap-filename MODIF ID r2 ,
           p_xml    TYPE rlgrap-filename MODIF ID r2 NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b04.


*-------------AT SELECTION SCREEN--------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_xml_tl.
  PERFORM f_get_diretorio USING p_xml_tl.

*-----------------------------*
*            Start of selection
*-----------------------------*
START-OF-SELECTION.

  IF NOT p_xml_tl IS INITIAL.
    PERFORM f_lote    USING p_xml_tl space        .
    PERFORM f_salv_tab                            .
  ELSE.
    MESSAGE text-002 TYPE c_i.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  f_get_diretorio
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_XML_LT text
*----------------------------------------------------------------------*
FORM f_get_diretorio USING p_p_xml_lt.
  DATA:v_window_title          TYPE              string               ,
       v_selected_folder       TYPE              string               .

  v_window_title = text-001.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = v_window_title
    CHANGING
      selected_folder      = v_selected_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <>  0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  p_p_xml_lt = v_selected_folder.

ENDFORM.                    " F_GET_DIRETORIO


*&---------------------------------------------------------------------*
*&      Form  f_lote
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_lote USING p_p_xml_lt
                  p_e       .

  CREATE OBJECT v_xml.

  IF NOT p_p_xml_lt IS INITIAL.

    CONCATENATE p_p_xml_lt c_barra INTO p_xml_tl.

    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        directory  = p_p_xml_lt
        filter     = c_xml
      TABLES
        file_table = it_field_table
        dir_table  = it_dir_table
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2.

    IF sy-subrc <> 0.
*   Implement suitable error handling here
      MESSAGE text-002 TYPE c_e.
    ENDIF.
  ENDIF.
  SORT it_field_table BY pathname.
*   Correct uppercase in file names.
  LOOP AT it_field_table INTO wa_field_table.
    TRANSLATE wa_field_table-pathname TO LOWER CASE.
    MODIFY it_field_table FROM wa_field_table.
  ENDLOOP.
******************************************
*   Start xlm upload and internal table modification
******************************************
  LOOP AT it_field_table INTO wa_field_table.

    CLEAR p_xml.
    CONCATENATE p_p_xml_lt c_barra wa_field_table-pathname INTO p_xml.
    IF wa_field_table-pathname+4(2) = c_ret.

      CLEAR: wa_xml_data, v_xml_string, v_size.
      FREE it_xml_data.
      CALL METHOD v_xml->free.
      CREATE OBJECT v_xml.

*Upload XML File DE RETORNO.
      CALL METHOD v_xml->import_from_file
        EXPORTING
          filename = p_xml
        RECEIVING
          retcode  = v_subrc.

      IF v_subrc = 0 OR v_subrc = 1.
        CALL METHOD v_xml->render_2_xstring
          IMPORTING
            retcode = v_subrc
            stream  = v_xml_string
            size    = v_size.
        IF v_subrc = 0.
          MOVE v_xml_string TO it_xml-xml.
          APPEND it_xml.
          it_xml_r[] = it_xml[] .
          CLEAR: it_xml,
                 p_xml.
          FREE it_xml[].
        ENDIF.
      ENDIF.

*   Upload XML DE ENVIO.
      CONCATENATE wa_field_table-pathname(4) '_e.xml' INTO v_envio_nome.
      READ TABLE it_field_table INTO wa_field_table_e WITH KEY pathname = v_envio_nome BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        CONCATENATE p_p_xml_lt c_barra wa_field_table_e-pathname INTO p_xml.
        CLEAR: wa_xml_data, v_xml_string, v_size.
        FREE it_xml_data.
        CALL METHOD v_xml->free.
        CREATE OBJECT v_xml.

*Upload XML File for return file.
        CALL METHOD v_xml->import_from_file
          EXPORTING
            filename = p_xml
          RECEIVING
            retcode  = v_subrc.

        IF v_subrc = 0 OR v_subrc = 1.
          CALL METHOD v_xml->render_2_xstring
            IMPORTING
              retcode = v_subrc
              stream  = v_xml_string
              size    = v_size.
          IF v_subrc = 0.
            MOVE v_xml_string TO it_xml-xml.
            APPEND it_xml.
            it_xml_e[] = it_xml[] .
            CLEAR: it_xml,
                   p_xml.
            FREE it_xml[].
          ENDIF.
        ENDIF.
      ENDIF.
************************************************************
*  Iniciar preenchimento das estruturas.
************************************************************
      PERFORM f_parse.

    ELSE.
      CONTINUE.
    ENDIF.

  ENDLOOP.
ENDFORM.                    "f_lote

**&---------------------------------------------------------------------*
**&      Form  f_parse
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
FORM f_parse.
  DATA: lv_ztag1          TYPE t052-ztag1     ,
        lv_vencimento_min TYPE datum          ,
        v_table           TYPE string         ,
        v_field           TYPE string         ,
        v_cname           TYPE string         ,
        v_tbdat           TYPE REF TO data    ,
        v_1               TYPE i              ,
        v_strt            TYPE string         ,
        v_strt2           TYPE string         ,
        lv_cte_imp        TYPE c.

  CONSTANTS: c_tx   TYPE c LENGTH 2 VALUE 'TX',
             c_s    TYPE c          VALUE 'S'.

  SORT it_xml_data_env BY hier
                          cname.
*   Tabela com parametros para preenchimento das tabelas.
  SELECT * FROM  znfset_tag_2_fld UP TO 100  ROWS INTO TABLE it_param.
  SORT it_param BY cname.


  LOOP AT it_xml_e.
    CLEAR lv_cte_imp.
    CLEAR wa_xml_data.
    FREE: it_xml_data, it_return.

* Convert XML to internal table
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = it_xml_e-xml
      TABLES
        xml_table = it_xml_data
        return    = it_return.

    MOVE it_xml_data[] TO it_xml_data_env[].
    CLEAR:  wa_xml_data.
  ENDLOOP.

  LOOP AT it_xml_r.
    CLEAR lv_cte_imp.
    CLEAR wa_xml_data.
    FREE: it_xml_data, it_return.

* Convert XML to internal table
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = it_xml_r-xml
      TABLES
        xml_table = it_xml_data2
        return    = it_return.

    MOVE it_xml_data2[] TO it_xml_data_ret[].
  ENDLOOP.


* Preenchendo com parametros para fazer a seleção na tabela zsdnfsetws001in.
  READ TABLE it_xml_data_env INTO  wa_xml_data WITH KEY cname = c_cnpj.
  IF sy-subrc IS INITIAL.
    wa_envio_retorno-prest_cnpj         =  wa_xml_data-cvalue.
  ENDIF.

  READ TABLE it_xml_data_env  INTO  wa_xml_data WITH KEY cname =  c_cod_v.
  IF sy-subrc IS INITIAL.
    wa_envio_retorno-pnfse_det_cod_val  =  wa_xml_data-cvalue.
  ENDIF.

  READ TABLE it_xml_data_env  INTO  wa_xml_data WITH KEY cname = c_bloco.
  IF sy-subrc IS INITIAL.
    wa_envio_retorno-pnfse_bloco_seq    =  wa_xml_data-cvalue.
  ENDIF.

  READ TABLE it_xml_data_env  INTO  wa_xml_data WITH KEY cname =  c_preno.
  IF sy-subrc IS INITIAL.
    wa_envio_retorno-pnfse_det_seq      =  wa_xml_data-cvalue.
  ENDIF.

  SELECT *
   FROM  zsdnfsetws001in
   INTO  wa_zsdnfsetws001in
   WHERE
         prest_cnpj           EQ     wa_envio_retorno-prest_cnpj         AND
         pnfse_det_cod_val    EQ     wa_envio_retorno-pnfse_det_cod_val  AND
         pnfse_bloco_seq      EQ     wa_envio_retorno-pnfse_bloco_seq    AND
         pnfse_det_seq        EQ     wa_envio_retorno-pnfse_det_seq        .
  ENDSELECT.
*    Atualização Log
    SELECT SINGLE docnum MAX( logver )
      INTO (wa_zsdnfset_logproc-docnum, wa_zsdnfset_logproc-logver)
      FROM zsdnfset_logproc
     WHERE docnum = wa_zsdnfsetws001in-docnum
     GROUP BY docnum.

    IF sy-subrc IS NOT INITIAL.
      wa_zsdnfset_logproc-docnum       = wa_zsdnfsetws001in-docnum.
      wa_zsdnfset_logproc-logver          = 1.
    ELSE.
      wa_zsdnfset_logproc-logver = wa_zsdnfset_logproc-logver + 1.
    ENDIF.

  SELECT * FROM zsdnfset_monitor INTO wa_zsdnfset_monitor WHERE docnum = wa_zsdnfsetws001in-docnum.
  ENDSELECT.
  SELECT * FROM zsdnfsetprenotad INTO wa_zsdnfset_monitor WHERE docnum = wa_zsdnfsetws001in-docnum.
  ENDSELECT.
  SELECT * FROM zsdnfsetws001out INTO wa_zsdnfsetws001out WHERE docnum = wa_zsdnfsetws001in-docnum.
  ENDSELECT.
  SELECT * FROM j_1bnfdoc        INTO wa_j_1bnfdoc        WHERE docnum = wa_zsdnfsetws001in-docnum.
  ENDSELECT.

  LOOP AT  it_xml_data_env[] INTO wa_xml_data.
    TRANSLATE wa_xml_data-cname TO UPPER CASE.
    READ TABLE it_param INTO wa_param WITH KEY cname = wa_xml_data-cname BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      v_cname = wa_param-cname.
      LOOP AT it_param INTO wa_param FROM sy-tabix.
        IF wa_param-cname EQ v_cname.
          TRANSLATE wa_param-field TO LOWER CASE.
          v_1 = strlen( wa_param-field ).
          IF v_1 > 6.
            ASSIGN (wa_param-field) TO <field>.
            SPLIT wa_param-field AT '-' INTO v_strt v_strt2.
            ASSIGN (v_strt) TO <wa>.
          ELSE.
            CONTINUE.
          ENDIF.
          SPLIT wa_param-field AT '-' INTO v_table v_field.
          TRANSLATE v_table TO LOWER CASE.
          CONCATENATE 'it' v_table+2 INTO v_table.
          ASSIGN (v_table) TO <table>.
          IF <field> IS  ASSIGNED.
            IF wa_param-field EQ c_vb.
              MOVE wa_xml_data-cvalue(18) TO <field>.
            ELSE.
              MOVE wa_xml_data-cvalue TO <field>.
            ENDIF.

*            <field> = wa_xml_data-cvalue.
            IF <table> IS ASSIGNED AND <wa> IS ASSIGNED.
*              UPDATE <table> FROM <wa>.
            ENDIF.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  LOOP AT  it_xml_data_ret[] INTO wa_xml_data.
    TRANSLATE wa_xml_data-cname TO UPPER CASE.
    READ TABLE it_param INTO wa_param WITH KEY cname = wa_xml_data-cname BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      v_cname = wa_param-cname.
      LOOP AT it_param INTO wa_param FROM sy-tabix.
        IF wa_param-cname EQ v_cname.
          TRANSLATE wa_param-field TO LOWER CASE.
          v_1 = strlen( wa_param-field ).
          IF v_1 > 6.
            ASSIGN (wa_param-field) TO <field>.
            SPLIT wa_param-field AT '-' INTO v_strt v_strt2.
            ASSIGN (v_strt) TO <wa>.
          ELSE.
            CONTINUE.
          ENDIF.
          SPLIT wa_param-field AT '-' INTO v_table v_field.
          TRANSLATE v_table TO LOWER CASE.
          CONCATENATE 'it' v_table+2 INTO v_table.
          ASSIGN (v_table) TO <table>.
          IF <field> IS  ASSIGNED.
            MOVE wa_xml_data-cvalue TO <field>.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

*  Append all structures to internal tables.
  IF NOT wa_zsdnfsetws001in  IS INITIAL.
    APPEND wa_zsdnfsetws001in  TO it_zsdnfsetws001in  .
  ENDIF.
  IF NOT  wa_zsdnfset_logproc IS INITIAL.
    APPEND wa_zsdnfset_logproc TO it_zsdnfset_logproc .
  ENDIF.
  IF NOT wa_zsdnfset_monitor IS INITIAL.
    APPEND wa_zsdnfset_monitor TO it_zsdnfset_monitor .
  ENDIF.
  IF NOT wa_zsdnfsetprenotad IS INITIAL.
    APPEND wa_zsdnfsetprenotad TO it_zsdnfsetprenotad .
  ENDIF.
  IF NOT wa_zsdnfsetws001out IS INITIAL.
    APPEND wa_zsdnfsetws001out TO it_zsdnfsetws001out .
  ENDIF.
  IF NOT wa_j_1bnfdoc IS INITIAL.
    APPEND wa_j_1bnfdoc        TO it_j_1bnfdoc        .
  ENDIF.

  CLEAR:
  wa_envio_retorno   ,
  wa_zsdnfsetws001in ,
  wa_zsdnfset_logproc,
  wa_zsdnfset_monitor,
  wa_zsdnfsetprenotad,
  wa_zsdnfsetws001out,
  wa_j_1bnfdoc       .

  SORT it_envio_all BY prest_cnpj pnfse_det_cod_val pnfse_bloco_seq pnfse_det_seq .
  DELETE ADJACENT DUPLICATES FROM it_envio_all COMPARING prest_cnpj pnfse_det_cod_val pnfse_bloco_seq pnfse_det_seq .

ENDFORM.                    "f_parse


*&---------------------------------------------------------------------*
*&      Form  f_salv_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_salv_tab.

**Commit work somente se todas as tabelas forem escritas com sucesso.
*  MODIFY zsdnfset_logproc    FROM TABLE it_zsdnfset_logproc.
*  IF sy-subrc IS INITIAL.
*    MODIFY zsdnfset_monitor    FROM TABLE it_zsdnfset_monitor.
*    IF sy-subrc IS INITIAL.
*      MODIFY j_1bnfdoc           FROM TABLE it_j_1bnfdoc.
*      IF sy-subrc IS INITIAL.
*        MODIFY zsdnfsetws001in     FROM TABLE it_zsdnfsetws001in.
*        IF sy-subrc IS INITIAL.
*          MODIFY zsdnfsetprenotad    FROM TABLE it_zsdnfsetprenotad.
*          IF sy-subrc IS INITIAL.
*            MODIFY zsdnfsetws001out   FROM TABLE it_zsdnfsetws001out.
*            IF sy-subrc IS INITIAL.
*              COMMIT WORK.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.



ENDFORM.                    "f_salv_tab
