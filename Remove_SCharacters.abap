FORM f_removespec .
  CONSTANTS:
       lv_spec(41)    TYPE c  VALUE '-+€»¤ƒ³¯ï¼ˆæœ¬ï¼‰’ä½¿”¨™!@#$%¨&*()^´~<>',
       lv_spec2(18)    TYPE c  VALUE 'êéáãõóçúíÊÉÁÃÕÓÇÚÍ',
       lv_spec3(18)    TYPE c  VALUE 'eeaaoocuiEEAAOOCUI'.

  DATA:
        lv_i    TYPE i,
        lv_str  TYPE i.
  lv_str = strlen( lv_spec ).


  DO lv_str TIMES.
    REPLACE ALL OCCURRENCES OF lv_spec+lv_i(1) IN wa_exit-txt WITH ''. " wa_exit-txt = Texto com caracteres a serem retirados. 
    lv_i = lv_i + 1.
  ENDDO.
  lv_i = 0.

  DO 18 TIMES.
    REPLACE ALL OCCURRENCES OF lv_spec2+lv_i(1) IN wa_exit-txt WITH lv_spec3+lv_i(1).
      lv_i = lv_i + 1.
  ENDDO.
   lv_i = 0.

ENDFORM. 
