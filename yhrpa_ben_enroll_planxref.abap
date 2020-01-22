*&---------------------------------------------------------------------*
*& Report ZHRPA_BEN_ENROLL_PLANXREF
*& Report ZHRPA_BEN_VENDOR_PLANXREF
*&---------------------------------------------------------------------*
*& CLass to load the vendor plan xref table into program
*&---------------------------------------------------------------------*
REPORT yhrpa_ben_enroll_planxref.


****
**<<<< lcl_plan_xref >>>>
****
CLASS lcl_plan_xref DEFINITION.

  PUBLIC SECTION.

    TYPES:  BEGIN OF plan_xref_s
              ,begda      TYPE  dats    "record active from
              ,endda      TYPE  dats    "record active until
              ,nkuplan    TYPE  char32  "NKU plan code
              ,xref_vendor TYPE  char32  "xref vendor key - identifies specific vendor
              ,xref_value  TYPE  char32  "xref value - code to pass to vendor
            ,END OF plan_xref_s.

    TYPES plan_xref_t   TYPE  STANDARD TABLE OF plan_xref_s WITH DEFAULT KEY.

    METHODS constructor IMPORTING iv_begda TYPE dats OPTIONAL iv_endda TYPE dats OPTIONAL  iv_xref_vendor TYPE char32.
*<TASK> add recordsloaded count for error checking
    METHODS get_xref_value IMPORTING iv_nkuplan TYPE char32 EXPORTING ev_value TYPE char32.

    DATA  mv_begda    TYPE  dats.
    DATA  mv_endda    TYPE  dats.
    DATA  mv_vendor   TYPE char32.

    DATA  mt_xref_value TYPE  plan_xref_t.

ENDCLASS.



*----------------------------------------------------------------------*
* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> *
*----------------------------------------------------------------------*

DATA  ss_hide             TYPE    i VALUE 0.
DATA  ss_show             TYPE    i VALUE 1.
DATA  ss_enter            TYPE    i VALUE 0.
DATA  ss_noenter          TYPE    i VALUE 1.
DATA  ss_default_dd       TYPE    localfile.

* Screen block for data selection options
SELECTION-SCREEN BEGIN OF BLOCK dsel_opt WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(4) TEXT-004.
PARAMETERS p_begda  TYPE  dats.
SELECTION-SCREEN COMMENT 20(2) TEXT-005.
PARAMETERS p_endda  TYPE  dats.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(4) TEXT-006.
PARAMETERS p_vend  TYPE  char32.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK dsel_opt.


*----------------------------------------------------------------------*
* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> *
*----------------------------------------------------------------------*

INITIALIZATION.

*----------------------------------------------------------------------*
* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  IF p_begda(4) NE p_endda(4).
    "...only select data from a single year
    p_begda = p_endda(4) && '0101'.
  ENDIF.

  DATA(o_plan_xref) = NEW lcl_plan_xref( iv_begda = p_begda iv_endda = p_endda iv_xref_vendor = p_vend ).

  o_plan_xref->get_xref_value( EXPORTING iv_nkuplan = 'NKU1' IMPORTING ev_value = DATA(lt_xref_value) ).

*----------------------------------------------------------------------*
* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> *
*----------------------------------------------------------------------*

****
**<<<< lcl_alv IMPLEMENTATION >>>>
****
*CLASS lcl_alv IMPLEMENTATION.
*
*  METHOD constructor.
*    me->o_data = io_data.
*  ENDMETHOD.
*
*  METHOD generate_alv.
*
*
*    FIELD-SYMBOLS   <data>  TYPE  lcl_data=>report_data_s.
*    FIELD-SYMBOLS   <alv>   TYPE  alv_s.
*
*    LOOP AT me->o_data->mt_data ASSIGNING <data>.
*      APPEND INITIAL LINE TO mt_alv ASSIGNING <alv>.
*      MOVE-CORRESPONDING <data> TO <alv>.
*    ENDLOOP.
*
*    TRY.
*        CALL METHOD cl_salv_table=>factory
*          IMPORTING
*            r_salv_table = me->o_salv
*          CHANGING
*            t_table      = mt_alv.
*      CATCH   cx_salv_msg.
*    ENDTRY.
*
*    me->set_alv_columns( EXPORTING colhdr = colhdr ).
*    me->o_salv->display(  ).
*  ENDMETHOD.
*
*
*  METHOD  set_alv_columns.
*    DATA: lr_column  TYPE REF TO cl_salv_column_table,
*          lr_columns TYPE REF TO cl_salv_columns_table.
*
*    lr_columns = o_salv->get_columns( ).
*    lr_columns->set_optimize( abap_true ).
*
*    TRY.
*        lr_column ?= lr_columns->get_column( 'PII_PERNR' ).
*        lr_column->set_short_text( 'PERNR' ).
*        lr_column->set_medium_text( 'PERNR' ).
*        lr_column->set_long_text( 'PERNR' ).
**        lr_column->set_key( if_salv_c_bool_sap=>true ).
**        lr_column->set_visible( false ).
*      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
*    ENDTRY.
*  ENDMETHOD.

*ENDCLASS.



CLASS lcl_plan_xref IMPLEMENTATION.

  METHOD constructor.

    DATA  lv_begda  TYPE  dats.
    DATA  lv_endda  TYPE  dats.


    IF iv_begda IS NOT SUPPLIED OR iv_begda IS INITIAL.
      lv_begda = sy-datum.
    ELSE.
      lv_begda = iv_begda.
    ENDIF.

    IF iv_endda IS NOT SUPPLIED OR iv_endda IS INITIAL.
      lv_endda = sy-datum.
    ELSE.
      lv_endda = iv_endda.
    ENDIF.

    mv_begda = lv_begda.
    mv_endda = lv_endda.
    mv_vendor = iv_xref_vendor.

*    SELECT * FROM zhr_benroll_vend INTO CORRESPONDING FIELDS OF TABLE lt_xref_value
*      where xref_vendor EQ mv_vendor
*        and begda LE mv_endda
*        AND endda GE mv_begda
*      ORDER BY nkuplan endda DESCENDING begda DESCENDING .

    DATA(lt_xref_value) = VALUE plan_xref_t(
    ( begda = '19000101' endda = '99991231' nkuplan = 'NKU3' xref_vendor = '0168-HART' xref_value = 'value31' )
    ( begda = '20190101' endda = '20191231' nkuplan = 'NKU1' xref_vendor = '0168-HART' xref_value = 'value11' )
    ( begda = '20200101' endda = '99991231' nkuplan = 'NKU1' xref_vendor = '0168-HART' xref_value = 'value12' )
    ( begda = '20200101' endda = '20200107' nkuplan = 'NKU2' xref_vendor = '0168-HART' xref_value = 'value21' )
    ( begda = '20200108' endda = '99991231' nkuplan = 'NKU2' xref_vendor = '0168-HART' xref_value = 'value22' )
    ).

    SORT lt_xref_value BY nkuplan endda DESCENDING begda DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_xref_value COMPARING nkuplan endda.
    mt_xref_value = lt_xref_value.

  ENDMETHOD.



  METHOD get_xref_value.

*DELETE    FIELD-SYMBOLS   <xref>   LIKE  LINE OF mt_xref_value.

    CLEAR    ev_value.
    LOOP AT mt_xref_value ASSIGNING FIELD-SYMBOL(<xref>)
      WHERE nkuplan EQ iv_nkuplan
        AND begda LE mv_endda
        AND endda GE mv_begda.

      IF sy-subrc NE 0.
        CLEAR ev_value.
      ELSE.
        ev_value = <xref>-xref_value.
      ENDIF.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.