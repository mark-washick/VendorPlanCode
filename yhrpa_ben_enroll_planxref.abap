*&---------------------------------------------------------------------*
*& Report ZHRPA_BEN_ENROLL_PLANXREF
*&---------------------------------------------------------------------*
*& CLass to load the vendor plan xref table into program
*&---------------------------------------------------------------------*
REPORT yhrpa_ben_enroll_planxref.


CONSTANTS   lowdate   TYPE  dats VALUE '19680101'.
CONSTANTS   highdate  TYPE  dats VALUE '99991231'.
CONSTANTS   zerodate  TYPE  c    LENGTH 8 VALUE '00000000'.

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

    METHODS constructor IMPORTING iv_begda TYPE dats OPTIONAL iv_endda TYPE dats OPTIONAL  iv_xref_vendor TYPE char32.
*<TASK> add recordsloaded count for error checking
    METHODS get_xref_value IMPORTING iv_benyear TYPE pabrj iv_nplan TYPE char32 iv_vplan TYPE char32 EXPORTING ev_xref TYPE char32.

    DATA  mt_xref_value TYPE  STANDARD TABLE OF plan_xref_s.
    DATA  vendor_id   TYPE char32.

ENDCLASS.



DATA  o_plan_xref  TYPE  REF TO lcl_plan_xref.



*----------------------------------------------------------------------*
* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> *
*----------------------------------------------------------------------*

DATA  ss_hide             TYPE    i VALUE 0.
DATA  ss_show             TYPE    i VALUE 1.
DATA  ss_enter            TYPE    i VALUE 0.
DATA  ss_noenter          TYPE    i VALUE 1.
DATA  ss_default_dd       TYPE    localfile.
*DATA  ls_sscreen_values   TYPE  lcl_sel=>sscreen_values_s.

* Screen block for data selection options
SELECTION-SCREEN BEGIN OF BLOCK dsel_opt WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(4) TEXT-004.
PARAMETERS p_begda  TYPE  dats.
SELECTION-SCREEN COMMENT 20(2) TEXT-005.
PARAMETERS p_endda  TYPE  dats.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK dsel_opt.

* Screen block for Output file/directory options
SELECTION-SCREEN BEGIN OF BLOCK file_opt WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) TEXT-013.
PARAMETERS : p_outb_y RADIOBUTTON GROUP rb1 MODIF ID rb1.
SELECTION-SCREEN COMMENT 26(4) TEXT-014.
PARAMETERS : p_outb_n RADIOBUTTON GROUP rb1 MODIF ID rb1.
SELECTION-SCREEN COMMENT 33(2) TEXT-015.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
PARAMETERS  p_ofile  TYPE localfile     MODIF ID ofo.

* client/desktop data directory
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(23) TEXT-010 MODIF ID ofo.
PARAMETERS:  p_client  RADIOBUTTON GROUP orbg USER-COMMAND disp MODIF ID ofo.
SELECTION-SCREEN COMMENT 28(7) TEXT-008 MODIF ID ofo.
PARAMETERS:       p_cl_dd      TYPE   localfile MODIF ID ofo.
SELECTION-SCREEN END OF LINE.

* server data directory
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(23) TEXT-000.    "just position cursor
PARAMETERS:  p_server  RADIOBUTTON GROUP orbg DEFAULT 'X' MODIF ID ofo.
SELECTION-SCREEN COMMENT 28(7) TEXT-009 MODIF ID ofo.
PARAMETERS:       p_srv_dd    TYPE  localfile MODIF ID ofo.
SELECTION-SCREEN END OF LINE.

* server archive dir
SELECTION-SCREEN SKIP.
PARAMETERS:       p_srv_ad    TYPE  localfile MODIF ID ofo.

SELECTION-SCREEN END OF BLOCK file_opt.

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

  CREATE OBJECT o_plan_xref EXPORTING iv_begda = p_begda iv_endda = p_endda iv_xref_vendor = '0168-HART'.

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
    DATA  lt_xref_value LIKE mt_xref_value.

    IF iv_begda IS NOT SUPPLIED.
      lv_begda = sy-datum.
    ELSE.
      lv_begda = iv_begda.
    ENDIF.

    IF iv_endda IS NOT SUPPLIED.
      lv_endda = sy-datum.
    ELSE.
      lv_endda = iv_endda.
    ENDIF.

    vendor_id = iv_xref_vendor.

*    SELECT * FROM zhr_benroll_vend INTO CORRESPONDING FIELDS OF TABLE lt_xref_value
*        and begda LE iv_endda
*        AND endda GE iv_begda
*      where xref_vendor EQ iv_xref_vendor
*      ORDER BY nkuplan endda DESCENDING begda DESCENDING .

    DELETE ADJACENT DUPLICATES FROM lt_xref_value COMPARING nkuplan .
    mt_xref_value = lt_xref_value.

  ENDMETHOD.



  METHOD get_xref_value.

    FIELD-SYMBOLS   <xref>   LIKE  LINE OF mt_xref_value.

    CLEAR    ev_xref.
    LOOP AT mt_xref_value ASSIGNING <xref>
      WHERE nkuplan EQ iv_nplan.

      IF sy-subrc NE 0.
        CLEAR ev_xref.
      ELSE.
        ev_xref = <xref>-xref_value.
      ENDIF.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.