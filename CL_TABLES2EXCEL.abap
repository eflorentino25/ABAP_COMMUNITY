*&---------------------------------------------------------------------*
*& Report  ZEDU_EXCEL01_CLASS
*&
*&---------------------------------------------------------------------*
*&  Author: Eduardo D. Florentino
*&  Date:23/01/2019
*&---------------------------------------------------------------------*

REPORT  ZEDU_EXCEL01_CLASS.


class export_excel DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
        Init_xl       IMPORTING ip_sheetname    TYPE string
                                ip_contentstb   TYPE ANY TABLE,

        create_sheet  IMPORTING ip_sheetname    TYPE string
                                ip_contentstb   TYPE ANY TABLE,

        pass_table    IMPORTING ip_sheetname    TYPE string
                                ip_table        TYPE ANY TABLE.

  PRIVATE SECTION.


    TYPES: data1(1500) TYPE c,
           ty_data     TYPE TABLE OF data1.

    CLASS-DATA:
                it_contents       TYPE TABLE OF data1        ,
                wa_dadosxl        like LINE OF  it_contents  ,
                gv_stg            TYPE          string       ,
                l_rc              TYPE          i            ,
                gt_1              TYPE          ty_data      ,
                gt_2              TYPE          ty_data      ,
                gt_3              TYPE          ty_data      ,
                gt_4              TYPE          ty_data      ,
                gv_used           TYPE          i            ,
                rec               TYPE          sy-tfill     ,
                deli(1)           TYPE          c            ,
                l_amt(18)         TYPE          c            ,
                gv_sheet_name(20) TYPE          c            ,
                h_excel           TYPE          ole2_object  , " Excel object
                h_mapl            TYPE          ole2_object  , " list of workbooks
                h_map             TYPE          ole2_object  , " workbook
                h_zl              TYPE          ole2_object  , " cell
                h_f               TYPE          ole2_object  , " font
                gs_interior       TYPE          ole2_object  , " Pattern
                worksheet         TYPE          ole2_object  ,
                h_cell            TYPE          ole2_object  ,
                h_cell1           TYPE          ole2_object  ,
                range             TYPE          ole2_object  ,
                h_sheet2          TYPE          ole2_object  ,
                h_sheet3          TYPE          ole2_object  ,
                h_sheet4          TYPE          ole2_object  ,
                gs_font           TYPE          ole2_object  ,
                wa_hstruct        TYPE          ole2_object  ,
                flg_stop(1)       TYPE          c            ,
                w_cell1           TYPE          ole2_object  ,
                w_cell2           TYPE          ole2_object  .


ENDCLASS.



*******************************************************
** Class Implementation
** Classe criada sem variável de referencia.
*******************************************************

CLASS export_excel IMPLEMENTATION.

  method Init_xl.



    IF h_excel-header = space OR h_excel-handle = -1.
      CREATE OBJECT h_excel 'EXCEL.APPLICATION'.
    ENDIF.

*--- get list of workbooks, initially empty
    CALL METHOD OF h_excel 'Workbooks' = h_mapl.
    SET PROPERTY OF h_excel 'Visible'   = 1  .
    CALL METHOD OF h_mapl 'Add'  = h_map.



*&---------------------------------------------------------------------*
*&       SET_FIRST_SHEET
*&---------------------------------------------------------------------*
    gv_sheet_name = ip_sheetname.
    it_contents[] = IP_CONTENTSTB[].
    GET PROPERTY OF h_excel   'ACTIVESHEET' = worksheet     .
    SET PROPERTY OF worksheet 'Name'        = gv_sheet_name .


    CALL METHOD cl_gui_frontend_services=>clipboard_export
      IMPORTING
        data                 = it_contents[]
      CHANGING
        rc                   = l_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

*&---------------------------------------------------------------------*
*&   PASTE_EXCEL
*&---------------------------------------------------------------------*

    CALL METHOD OF h_excel 'Cells' = w_cell1
      EXPORTING
        #1      = 1
        #2      = 1.
    CALL METHOD OF h_excel 'Cells' = w_cell2
      EXPORTING
        #1      = 1
        #2      = 1.
    CALL METHOD OF h_excel 'Range' = range
      EXPORTING
        #1      = w_cell1
        #2      = w_cell2.
    CALL METHOD OF range 'Select'.
    CALL METHOD OF worksheet 'Paste'.



  ENDMETHOD.


*&---------------------------------------------------------------------*
*&     Create/Add sheet to initialized excel
*&---------------------------------------------------------------------*


  METHOD create_sheet.


    it_contents[] = ip_contentstb[].


    gv_sheet_name = ip_sheetname.
    GET PROPERTY OF h_excel 'Sheets' = wa_hstruct .
    CALL METHOD OF
        wa_hstruct
        'Add'      = h_map.
    SET PROPERTY OF h_map 'Name' = gv_sheet_name .
    GET PROPERTY OF h_excel 'ACTIVESHEET' = worksheet.


    CALL METHOD cl_gui_frontend_services=>clipboard_export
      IMPORTING
        data                 = it_contents[]
      CHANGING
        rc                   = l_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

*&---------------------------------------------------------------------*
*&   PASTE_EXCEL
*&---------------------------------------------------------------------*

    CALL METHOD OF h_excel 'Cells' = w_cell1
        EXPORTING
        #1      = 1
        #2      = 1.
    CALL METHOD OF h_excel 'Cells' = w_cell2
      EXPORTING
        #1      = 1
        #2      = 1.
    CALL METHOD OF h_excel 'Range' = range
      EXPORTING
        #1      = w_cell1
        #2      = w_cell2.
    CALL METHOD OF range 'Select'.
    CALL METHOD OF worksheet 'Paste'.

  ENDMETHOD.


*&---------------------------------------------------------------------*
*&   Pass Table converter
*&  Converte table qq para uma tabela do tipo data1 para excel.
*&---------------------------------------------------------------------*

  METHOD pass_table.



    deli = cl_abap_char_utilities=>horizontal_tab.


    FIELD-SYMBOLS: <itab> TYPE ANY TABLE,
                 <wa> TYPE ANY,
                 <fs_field> TYPE ANY.

    DATA: lro_structdescr TYPE REF TO cl_abap_structdescr,
          lt_components   TYPE cl_abap_structdescr=>component_table.
    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF lt_components.

    LOOP AT ip_table ASSIGNING <wa>.
      IF lt_components IS INITIAL.  "get columns' names only once.
        lro_structdescr ?= cl_abap_typedescr=>describe_by_data( <wa> ).
        lt_components = lro_structdescr->get_components( ).
      ENDIF.

      DO. "iterate all columns in the row
        ASSIGN COMPONENT sy-index OF STRUCTURE <wa> TO <fs_field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        READ TABLE lt_components ASSIGNING <ls_comp> INDEX sy-index.


        IF <fs_field> IS ASSIGNED AND <ls_comp>  IS ASSIGNED.
          MOVE <fs_field> to gv_stg.                                         "field name: <ls_comp>-name.
          "field value: <fs_field>.
          CONCATENATE gv_stg  wa_dadosxl  into wa_dadosxl SEPARATED BY deli.
        endif.
      ENDDO.
      APPEND wa_dadosxl to it_contents.
    ENDLOOP.



    if gv_used IS INITIAL.



      export_excel=>INIT_XL(
        exporting
          IP_SHEETNAME  = ip_sheetname
          IP_CONTENTSTB = it_contents[]

      ).

      gv_used = 1.
      FREE it_contents[].
      CLEAR wa_dadosxl.
    ELSE.


      export_excel=>CREATE_SHEET(
        exporting
          IP_SHEETNAME  = ip_sheetname
          IP_CONTENTSTB = it_contents[]

      ).

      FREE it_contents[].
      CLEAR wa_dadosxl.

    ENDIF.



  ENDMETHOD.
ENDCLASS.

TYPES: data1(1500) TYPE c                                 ,
       ty_data     TYPE TABLE OF data1                    .

DATA: it_vbak     TYPE TABLE OF vbak                       ,
      it_vbap     TYPE TABLE OF vbap                       ,
      it_mara     TYPE TABLE OF mara                       ,
      it_rcalc    TYPE          ZBRSDTT005                 ,
      it_rnome    TYPE          ZBRSDTT005                 ,
      wa_string   type          string                     ,
      wa_string2  type          string                     ,
      wa_vbak     TYPE          vbak                       ,
      wa_vbap     TYPE          vbap                       ,
      wa_mara     TYPE          mara                       ,
      it_vbakxl   TYPE          ty_data  WITH HEADER LINE  ,
      it_vbapxl   TYPE          ty_data  WITH HEADER LINE  ,
      it_maraxl   TYPE          ty_data  WITH HEADER LINE  ,
      it_calcxl   TYPE          ty_data  WITH HEADER LINE  ,
      deli(1)     TYPE          c                          ,
      hstruct     TYPE          ole2_object                ,
      LR_CALC     TYPE REF TO   ZCL_BRCORE_DEV             .




START-OF-SELECTION.


  CREATE OBJECT LR_CALC.

  lr_calc->GET_TVARV_RANGE(
    exporting
      I_PARAMETER = 'ZJ1BPIS_CALC'
    changing
      E_RANGE     = it_rcalc
  ).


  lr_calc->GET_TVARV_RANGE(
    exporting
      I_PARAMETER = 'ZJ1BPIS_NOMES'
    changing
      E_RANGE     = it_rnome
  ).



  deli = cl_abap_char_utilities=>horizontal_tab.


  select * FROM vbak into TABLE it_vbak.
  select * from vbap into table it_vbap.
  select * FROM mara INTO TABLE it_mara.

  if NOT it_vbak IS INITIAL.
    LOOP at it_vbak into wa_vbak.
      CONCATENATE wa_vbak-VBELN wa_vbak-AEDAT INTO IT_VBAKXL SEPARATED BY deli.
      APPEND it_vbakxl.
    ENDLOOP.
  ENDIF.

  if NOT it_vbap IS INITIAL.
    LOOP at it_vbap into wa_vbap.
      CONCATENATE wa_vbap-VBELN wa_vbap-POSNR INTO IT_VBApXL SEPARATED BY deli.
      APPEND it_vbapxl.
    ENDLOOP.
  ENDIF.

  IF  NOT it_mara IS INITIAL.
    LOOP at   it_mara INTO  wa_mara.
      CONCATENATE wa_mara-KUNNR wa_mara-MATNR INTO it_maraxl SEPARATED BY deli.
      APPEND it_maraxl.
    ENDLOOP.
  ENDIF.

  IF NOT it_rcalc IS INITIAL.
    LOOP at it_rcalc into wa_string.
      READ TABLE IT_RNOME into wa_string2 INDEX sy-tabix.
      IF sy-subrc IS INITIAL.
        CONCATENATE wa_string+3 wa_string2+3 INTO it_calcxl SEPARATED BY deli.
      ENDIF.
      APPEND it_calcxl.
    ENDLOOP.
  ENDIF.






  export_excel=>PASS_TABLE(
    exporting
      IP_SHEETNAME = 'T1'
      IP_TABLE     = it_vbap
  ).

  export_excel=>PASS_TABLE(
     exporting
       IP_SHEETNAME = 'T2'
       IP_TABLE     = it_mara
   ).


  export_excel=>PASS_TABLE(
     exporting
       IP_SHEETNAME = 'T3'
       IP_TABLE     = it_vbak
   ).


*
*  export_excel=>INIT_XL(
*    exporting
*      IP_SHEETNAME  = 'VBAK'
*      IP_CONTENTSTB = it_vbakxl[]
*
*  ).
*
*  export_excel=>CREATE_SHEET(
*    exporting
*      IP_SHEETNAME  = 'VBAP'
*      IP_CONTENTSTB =  it_vbapxl[]
*
*  ).
*
*   export_excel=>CREATE_SHEET(
*    exporting
*      IP_SHEETNAME  = 'MARA'
*      IP_CONTENTSTB =  it_maraxl[]
*
*  ).

*     EXPORT_EXCEL=>CREATE_SHEET(
*    exporting
*      IP_SHEETNAME  = 'APURAÇÃO'
*      IP_CONTENTSTB =  it_calcxl[]
*
*  ).
