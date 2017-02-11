REPORT z_simulation_workbench_bw_3x.

************************************************************************
* Data Declaration                                                     *
************************************************************************
TYPES: BEGIN OF s_logsys,
         logsys TYPE logsys,
         txtmd  TYPE rstxtmd,
         icon   TYPE rsicon,
       END OF s_logsys,
       t_logsys TYPE TABLE OF s_logsys,
       BEGIN OF s_oltpsr,
         oltpsr TYPE roosourcer,
         txtsh TYPE rstxtsh,
         icon  TYPE rsicon,
       END OF s_oltpsr,
       t_oltpsr TYPE TABLE OF s_oltpsr,
       BEGIN OF s_target,
         target TYPE rsinfocube,
         txtlg  TYPE rstxtlg,
         icon   TYPE rsicon,
       END OF s_target,
       t_target TYPE TABLE OF s_target,
       BEGIN OF s_rnr,
         request_updated TYPE rsstatus,
         records         TYPE i,
         datum           TYPE sydatum,
         monitor_status  TYPE rsstatus,
         rnr_ext         TYPE rsbook20,
         rnr             TYPE rsrequnr,
         text            TYPE rstxtlg,
       END OF s_rnr,
       t_rnr TYPE TABLE OF s_rnr,
       BEGIN OF s_packid,
         packid  TYPE rsdatapid,
         records TYPE i,
       END OF s_packid,
       t_packid TYPE TABLE OF s_packid.
TYPE-POOLS: slis.
TYPE-POOLS: rs, rsa, rssm, rsods.
FIELD-SYMBOLS: <gt_psa>    TYPE STANDARD TABLE,
               <gt_outtab> TYPE STANDARD TABLE.
DATA: gt_fieldcat TYPE lvc_t_fcat.
TABLES: tbdls.
TYPES: BEGIN OF selection,
        fieldname TYPE fieldname,
        value     TYPE spo_value,
       END OF selection,
       t_selection TYPE TABLE OF selection.
DATA: gt_selection TYPE t_selection.
DATA: g_returncode TYPE char01.
DATA: g_odsname_tech TYPE rsa_odsdbname.
DATA: gt_events     TYPE slis_t_event.
DATA: g_repid LIKE sy-repid,
      g_dynnr TYPE sydynnr.
DATA: w_rnr TYPE /bi0/oirequid.
DATA: ws_functxt TYPE smp_dyntxt.
CLASS lcl_local_exception DEFINITION DEFERRED.
DATA: local_exception TYPE REF TO lcl_local_exception.
TABLES sscrfields.


************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN: FUNCTION KEY 1,
                  FUNCTION KEY 2.

SELECTION-SCREEN BEGIN OF BLOCK slstaging WITH FRAME TITLE slctxt19.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt20 FOR FIELD p_bi.
PARAMETERS: p_bi RADIOBUTTON GROUP stgt
                 USER-COMMAND STAGING_TYPE_CHANGE.
SELECTION-SCREEN COMMENT 26(27) c_bitran VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 43(27) c_bi.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt21 FOR FIELD p_bw_3x.
PARAMETERS: p_bw_3x DEFAULT 'X'
                    RADIOBUTTON GROUP stgt.
SELECTION-SCREEN COMMENT 26(4) c_bwismp VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 29(4) c_sqr_1 VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 32(4) c_arrow VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 36(4) c_bwupdr VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 39(4) c_sqr_2 VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 43(27) c_bw_3x.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK slstaging.
SELECTION-SCREEN BEGIN OF BLOCK slpsa WITH FRAME TITLE slctxt16.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(21) slctxt1  FOR FIELD p_logsys.
PARAMETERS: p_logsys TYPE logsys.
SELECTION-SCREEN COMMENT 55(4) c_logsys FOR FIELD p_logsys.
SELECTION-SCREEN POSITION 59.
PARAMETERS: t_logsys TYPE rstxtmd MODIF ID txt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(21) slctxt2 FOR FIELD p_oltpsr.
PARAMETERS: p_oltpsr TYPE roosourcer.
SELECTION-SCREEN COMMENT 55(4) c_oltpsr FOR FIELD p_oltpsr.
SELECTION-SCREEN POSITION 59.
PARAMETERS: t_oltpsr TYPE rstxtmd MODIF ID txt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(21) slctxt3 FOR FIELD p_target.
PARAMETERS: p_target TYPE rsinfocube.
SELECTION-SCREEN COMMENT 55(4) c_target FOR FIELD p_target.
SELECTION-SCREEN POSITION 59.
PARAMETERS: t_target TYPE rstxtmd MODIF ID txt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) slctxt4 FOR FIELD s_rnr.
SELECT-OPTIONS: s_rnr FOR w_rnr NO INTERVALS.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(21) slctxt18 FOR FIELD p_ndtrnr MODIF ID cnt.
PARAMETERS: p_ndtrnr AS CHECKBOX MODIF ID cnt DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK slpsa.

SELECTION-SCREEN BEGIN OF BLOCK slctntp WITH FRAME TITLE slctxt7.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt8 FOR FIELD p_cntxt.
SELECTION-SCREEN POSITION 27.
PARAMETERS: p_cntxt  RADIOBUTTON GROUP sltp DEFAULT 'X'
                     USER-COMMAND PSA_SELECT_TYPE_CHG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt9 FOR FIELD p_tchncl.
SELECTION-SCREEN POSITION 27.
PARAMETERS: p_tchncl RADIOBUTTON GROUP sltp.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK slctntp.

SELECTION-SCREEN BEGIN OF BLOCK slctn WITH FRAME TITLE slctxt11.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(21) slctxt10 FOR FIELD p_packid
                                        MODIF ID tch.
PARAMETERS: p_packid TYPE rsdatapid MODIF ID tch.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK slctnsl WITH FRAME TITLE slctxt15.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) slctxt12 FOR FIELD p_sl_all
                                        MODIF ID tch.
PARAMETERS: p_sl_all RADIOBUTTON GROUP sl
                     MODIF ID tch.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) slctxt14 FOR FIELD p_sl_sl
                                        MODIF ID tch.
PARAMETERS: p_sl_sl RADIOBUTTON GROUP sl DEFAULT 'X'
                                        MODIF ID tch.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK slctnsl.

SELECTION-SCREEN END OF BLOCK slctn.

SELECTION-SCREEN BEGIN OF BLOCK bdgopt WITH FRAME TITLE slctxt17.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt5 FOR FIELD p_dbgtrn.
PARAMETERS: p_dbgtrn AS CHECKBOX.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt6 FOR FIELD p_dbgupd.
PARAMETERS: p_dbgupd AS CHECKBOX.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bdgopt.


*---------------------------------------------------------------------*
*       CLASS lcl_local_exception  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_local_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA:
      local_text TYPE string.
    METHODS:
      constructor          IMPORTING text             TYPE string.
ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_bw_metadata DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_bw_metadata DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      is_cube              IMPORTING iv_cube           TYPE rsinfocube
                           RETURNING VALUE(rv_is_cube) TYPE char01,
      is_ods               IMPORTING iv_ods            TYPE rsdodsobject
                           RETURNING VALUE(rv_is_ods)  TYPE char01,
      is_infoobject        IMPORTING iv_infoobject     TYPE rsiobjnm
                           RETURNING VALUE(rv_is_infoobject)
                                                       TYPE char01.
ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_logsys DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_logsys DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_if_exists      IMPORTING iv_logsys         TYPE logsys
                           RAISING   lcl_local_exception,
      get_list             EXPORTING et_logsys         TYPE t_logsys,
      get_details          IMPORTING iv_logsys         TYPE logsys
                           RETURNING VALUE(rs_logsys)  TYPE s_logsys.
ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_oltpsr DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_oltpsr DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_if_exists      IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                           RAISING   lcl_local_exception,
      get_list             IMPORTING iv_logsys         TYPE logsys
                           EXPORTING et_oltpsr         TYPE t_oltpsr,
      get_details          IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                           RETURNING VALUE(rs_oltpsr)  TYPE s_oltpsr,
      get_infosource       IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                           CHANGING  cv_isource        TYPE rsisource
                                     cv_transtru       TYPE rstranstru
                                     OPTIONAL
                                     cv_isostype       TYPE rsisostype
                                     OPTIONAL
                                     cv_istype         TYPE rsrequtype
                                     OPTIONAL.
ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_target DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_target DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_if_exists      IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                                     iv_target         TYPE rsinfocube
                           RAISING   lcl_local_exception,
      is_flexible_update   IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                                     iv_target         TYPE rsinfocube
                           RAISING   lcl_local_exception,
      is_direct_update     IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                                     iv_target         TYPE rsinfocube
                           RETURNING VALUE(rv_direct_update)
                                                       TYPE char01,
      get_list             IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                           EXPORTING et_target         TYPE t_target,
      get_details          IMPORTING iv_target         TYPE rsinfocube
                           RETURNING VALUE(rs_target)  TYPE s_target.
ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_rnr DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_rnr DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_if_exists      IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                                     iv_rnr            TYPE rsrequnr
                           RAISING   lcl_local_exception,
      get_list             IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                                     iv_target         TYPE rsinfocube
                           EXPORTING et_rnr            TYPE t_rnr.
  PRIVATE SECTION.
    TYPES: BEGIN OF s_rnr_4sort.
      INCLUDE TYPE s_rnr.
    TYPES: rnr_ext_4sort TYPE rsbook20,
           END OF s_rnr_4sort.
    TYPES: t_rnr_4sort TYPE TABLE OF s_rnr_4sort.
ENDCLASS.


*---------------------------------------------------------------------*
* CLASS lcl_packid DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_packid DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_if_exists      IMPORTING iv_packid         TYPE rsdatapid
                                     iv_rnr            TYPE rsrequnr
                           RAISING   lcl_local_exception,
      get_list             IMPORTING iv_rnr            TYPE rsrequnr
                           EXPORTING et_packid         TYPE t_packid.
ENDCLASS.


*---------------------------------------------------------------------*
* CLASS lcl_psa DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_psa DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      check_if_data_exists IMPORTING iv_logsys         TYPE logsys
                                     iv_oltpsr         TYPE roosourcer
                           RAISING   lcl_local_exception.
ENDCLASS.


************************************************************************
AT SELECTION-SCREEN ON p_logsys.
************************************************************************
CHECK ( sy-ucomm <> 'STAGING_TYPE_CHANGE' ) AND
      ( sy-ucomm <> 'PSA_SELECT_TYPE_CHG' ).
TRY.
  lcl_logsys=>check_if_exists( p_logsys ).
  CATCH lcl_local_exception INTO local_exception.
    MESSAGE local_exception->local_text TYPE 'E'.
ENDTRY.

************************************************************************
AT SELECTION-SCREEN ON p_oltpsr.
************************************************************************
CHECK ( sy-ucomm <> 'STAGING_TYPE_CHANGE' ) AND
      ( sy-ucomm <> 'PSA_SELECT_TYPE_CHG' ).
TRY.
  lcl_oltpsr=>check_if_exists( iv_logsys = p_logsys
                               iv_oltpsr = p_oltpsr ).
  CATCH lcl_local_exception INTO local_exception.
    MESSAGE local_exception->local_text TYPE 'E'.
ENDTRY.

************************************************************************
AT SELECTION-SCREEN ON p_target.
************************************************************************
CHECK ( sy-ucomm <> 'STAGING_TYPE_CHANGE' ) AND
      ( sy-ucomm <> 'PSA_SELECT_TYPE_CHG' ) AND
      ( sy-ucomm <> 'FC02'                ).
TRY.
  lcl_target=>check_if_exists( iv_logsys = p_logsys
                               iv_oltpsr = p_oltpsr
                               iv_target = p_target ).
  CATCH lcl_local_exception INTO local_exception.
    MESSAGE local_exception->local_text TYPE 'E'.
ENDTRY.

************************************************************************
AT SELECTION-SCREEN ON s_rnr.
************************************************************************
DATA: ws_rnr LIKE LINE OF s_rnr.
CHECK ( sy-ucomm <> 'STAGING_TYPE_CHANGE' ) AND
      ( sy-ucomm <> 'PSA_SELECT_TYPE_CHG' ) AND
      ( sy-ucomm <> 'FC01'                ) AND
      ( sy-ucomm <> 'FC02'                ).

IF p_tchncl = 'X'.
  CASE LINES( s_rnr ).
  WHEN 0.
    MESSAGE e000(oo) WITH 'Specify Request Number for'
                          'Technical PSA selection criteria'.
  WHEN 1.
  WHEN OTHERS.
    MESSAGE e000(oo) WITH 'Select only one Request Number for'
                          'Technical PSA selection criteria'.
  ENDCASE.
ENDIF.
*
LOOP AT s_rnr INTO ws_rnr.
  TRY.
    lcl_rnr=>check_if_exists( iv_logsys = p_logsys
                              iv_oltpsr = p_oltpsr
                              iv_rnr    = ws_rnr-low ).
    CATCH lcl_local_exception INTO local_exception.
      MESSAGE local_exception->local_text TYPE 'E'.
  ENDTRY.
ENDLOOP.

************************************************************************
AT SELECTION-SCREEN ON p_packid.
************************************************************************
CHECK ( sy-ucomm <> 'STAGING_TYPE_CHANGE' ) AND
      ( sy-ucomm <> 'PSA_SELECT_TYPE_CHG' ) AND
      ( sy-ucomm <> 'FC01'                ) AND
      ( sy-ucomm <> 'FC02'                ) AND
      ( p_tchncl = 'X' ).

TRY.
  lcl_packid=>check_if_exists( iv_packid = p_packid
                               iv_rnr    = s_rnr-low ).
  CATCH lcl_local_exception INTO local_exception.
  MESSAGE local_exception->local_text TYPE 'E'.
ENDTRY.

************************************************************************
AT SELECTION-SCREEN.
************************************************************************
DATA: w_logsys TYPE logsys,
      w_oltpsr TYPE roosourcer.

CASE sy-ucomm.
WHEN 'STAGING_TYPE_CHANGE'.
  PERFORM swap_staging_type.
WHEN 'PSA_SELECT_TYPE_CHG'.
WHEN 'FC01'.
  PERFORM display_update_rules.
WHEN 'FC02'.
  PERFORM display_transfer_rules.
WHEN OTHERS.
  PERFORM get_selection_field_value: USING 'P_LOGSYS'
                                     CHANGING w_logsys,
                                     USING 'P_OLTPSR'
                                     CHANGING w_oltpsr.
  TRY.
    lcl_psa=>check_if_data_exists( iv_logsys = p_logsys
                                   iv_oltpsr = p_oltpsr ).
    CATCH lcl_local_exception INTO local_exception.
    MESSAGE local_exception->local_text TYPE 'I'.
  ENDTRY.
  PERFORM get_logsys_descr.
  PERFORM get_oltpsr_descr.
  PERFORM get_target_descr.
ENDCASE.

************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_logsys.
************************************************************************
DATA: w_logsys TYPE logsys.
PERFORM get_selection_field_value USING    'P_LOGSYS'
                                  CHANGING w_logsys.
PERFORM f4_logsys USING w_logsys.

************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_oltpsr.
************************************************************************
DATA: w_logsys TYPE logsys,
      w_oltpsr TYPE roosourcer.
PERFORM get_selection_field_value: USING    'P_LOGSYS'
                                   CHANGING w_logsys,
                                   USING    'P_OLTPSR'
                                   CHANGING w_oltpsr.
TRY.
  lcl_logsys=>check_if_exists( w_logsys ).
  CATCH lcl_local_exception INTO local_exception.
  MESSAGE local_exception->local_text TYPE 'I'.
  EXIT.
ENDTRY.
PERFORM f4_oltpsr USING w_logsys w_oltpsr.

************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_target.
************************************************************************
DATA: w_logsys TYPE logsys,
      w_oltpsr TYPE roosourcer,
      w_target TYPE rsinfocube.
PERFORM get_selection_field_value: USING    'P_LOGSYS'
                                   CHANGING w_logsys,
                                   USING    'P_OLTPSR'
                                   CHANGING w_oltpsr,
                                   USING    'P_TARGET'
                                   CHANGING w_target.
TRY.
  lcl_logsys=>check_if_exists( w_logsys ).
  lcl_oltpsr=>check_if_exists( iv_logsys = w_logsys
                               iv_oltpsr = w_oltpsr ).
  CATCH lcl_local_exception INTO local_exception.
  MESSAGE local_exception->local_text TYPE 'I'.
  EXIT.
ENDTRY.
PERFORM f4_target USING w_logsys w_oltpsr w_target.

************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_rnr-low.
************************************************************************
DATA: w_logsys TYPE logsys,
      w_oltpsr TYPE roosourcer,
      w_target TYPE rsinfocube.
PERFORM get_selection_field_value: USING    'P_LOGSYS'
                                   CHANGING w_logsys,
                                   USING    'P_OLTPSR'
                                   CHANGING w_oltpsr,
                                   USING    'P_TARGET'
                                   CHANGING w_target.
TRY.
  lcl_logsys=>check_if_exists( w_logsys ).
  lcl_oltpsr=>check_if_exists( iv_logsys = w_logsys
                               iv_oltpsr = w_oltpsr ).
  lcl_target=>check_if_exists( iv_logsys = w_logsys
                               iv_oltpsr = w_oltpsr
                               iv_target = w_target ).
  CATCH lcl_local_exception INTO local_exception.
  MESSAGE local_exception->local_text TYPE 'I'.
  EXIT.
ENDTRY.
PERFORM f4_rnr USING w_logsys w_oltpsr w_target.

************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_packid.
************************************************************************
DATA: w_logsys TYPE logsys,
      w_oltpsr TYPE roosourcer,
      w_target TYPE rsinfocube,
      w_rnr    TYPE rsrequnr.

PERFORM get_selection_field_value: USING    'P_LOGSYS'
                                   CHANGING w_logsys,
                                   USING    'P_OLTPSR'
                                   CHANGING w_oltpsr,
                                   USING    'P_TARGET'
                                   CHANGING w_target,
                                   USING    'S_RNR-LOW'
                                   CHANGING w_rnr.
CALL FUNCTION 'CONVERSION_EXIT_RQUID_INPUT'
     EXPORTING
          input  = w_rnr
     IMPORTING
          output = w_rnr.
TRY.
  lcl_logsys=>check_if_exists( w_logsys ).
  lcl_oltpsr=>check_if_exists( iv_logsys = w_logsys
                               iv_oltpsr = w_oltpsr ).
  lcl_target=>check_if_exists( iv_logsys = w_logsys
                               iv_oltpsr = w_oltpsr
                               iv_target = w_target ).
  IF w_rnr = SPACE.
    MESSAGE i000(oo) WITH 'Request Number is not specified'.
    EXIT.
  ENDIF.
  lcl_rnr=>check_if_exists( iv_logsys = w_logsys
                            iv_oltpsr = w_oltpsr
                            iv_rnr    = w_rnr ).
  CATCH lcl_local_exception INTO local_exception.
  MESSAGE local_exception->local_text TYPE 'I'.
  EXIT.
ENDTRY.
PERFORM f4_packid USING w_rnr.

************************************************************************
AT SELECTION-SCREEN OUTPUT.
************************************************************************
LOOP AT SCREEN.
  IF screen-group1 = 'TCH'.
    CASE p_cntxt.
      WHEN 'X'.
        screen-input     = '0'.
        screen-invisible = '1'.
        CLEAR: p_packid.
      WHEN SPACE.
        screen-input     = '1'.
        screen-invisible = '0'.
    ENDCASE.
    MODIFY SCREEN.
  ENDIF.
  IF screen-group1 = 'CNT'.
    CASE p_cntxt.
      WHEN SPACE.
        screen-input     = '0'.
        screen-invisible = '1'.
        CLEAR: p_ndtrnr.
      WHEN 'X'.
        screen-input     = '1'.
        screen-invisible = '0'.
    ENDCASE.
    MODIFY SCREEN.
  ENDIF.

  IF screen-group1 = 'TXT'.
    screen-display_3d = '0'.
    screen-input     = '0'.
    MODIFY SCREEN.
  ENDIF.
ENDLOOP.


************************************************************************
INITIALIZATION.
************************************************************************
ws_functxt-icon_id    = icon_biw_rules_act.
ws_functxt-quickinfo  = 'Display Update Rules'.
ws_functxt-icon_text  = 'Update Rules'.
sscrfields-functxt_01 = ws_functxt.
ws_functxt-icon_id    = icon_transfer_structure.
ws_functxt-quickinfo  = 'Display Transfer Rules'.
ws_functxt-icon_text  = 'Transfer Rules'.
sscrfields-functxt_02 = ws_functxt.

slctxt1  = 'Source system'.
slctxt2  = 'DataSource'.
slctxt3  = 'Target'.
slctxt4  = 'Request'.
slctxt5  = 'Debug Transfer Rules'.
slctxt6  = 'Debug Update Rules'.
slctxt7  = 'Record selection type:'.
slctxt8  = 'Content-specific'.
slctxt9  = 'SAP standard'.
slctxt10 = 'Package Number'.
slctxt11 = 'SAP standard selection:'.
slctxt12 = 'Use All'.
slctxt14 = 'Select'.
slctxt15 = 'Data Packet Records:'.
slctxt16 = 'Transfer / Update Rules and Request(s) Selection:'.
slctxt17 = 'Debug options:'.
slctxt18 = 'Only loaded requests'.
slctxt19 = 'Data Staging Type'.
slctxt20 = 'BI'.
slctxt21 = 'BW 3.x'.
sy-title = 'Simulation Workbench: BW 3.x'.
g_repid = sy-repid.
g_dynnr = sy-dynnr.
c_bitran = icon_biw_rules_act.
c_bwismp = icon_transfer_structure.
c_bwupdr = icon_biw_rules_act.
c_sqr_1 = c_sqr_2 = icon_parameter.
c_arrow = icon_draw_arrow.
c_bi = 'Transformation'.
c_bw_3x = 'Transfer Rule / Update Rule'.


************************************************************************
START-OF-SELECTION.
************************************************************************
IF p_cntxt = 'X'.
  PERFORM psa_selection CHANGING g_returncode
                                 gt_selection
                                 g_odsname_tech.
  IF g_returncode = 'A'.
    RETURN.
  ENDIF.
  PERFORM select_from_psa.
  PERFORM display_psa_selection.
ENDIF.

IF p_tchncl = 'X'.
  PERFORM simulate_tech.
ENDIF.


************************************************************************
END-OF-SELECTION.
************************************************************************

*---------------------------------------------------------------------*
*       FORM f4_logsys                                                *
*---------------------------------------------------------------------*
*      -->P_LOGSYS_PATTERN   text
*---------------------------------------------------------------------*
FORM f4_logsys USING p_logsys_pattern TYPE logsys.
TYPES: t_field TYPE TABLE OF help_value.
TYPES: BEGIN OF value,
         value(30) TYPE c,
       END OF value,
       t_value TYPE TABLE OF value.
DATA: wa_field TYPE LINE OF t_field,
      wt_field TYPE t_field.
DATA: wa_value TYPE value,
      wt_value TYPE t_value.
DATA: w_logsys_pattern TYPE logsys.
DATA: ws_logsys TYPE s_logsys,
      wt_logsys TYPE t_logsys.
DATA: w_selected_logsys TYPE logsys.

  CASE p_logsys_pattern.
  WHEN SPACE.
    w_selected_logsys = '*'.
  WHEN OTHERS.
    w_selected_logsys = p_logsys_pattern.
  ENDCASE.

  CALL METHOD lcl_logsys=>get_list IMPORTING et_logsys = wt_logsys.
  LOOP AT wt_logsys INTO ws_logsys WHERE logsys CP w_selected_logsys.
    APPEND: ws_logsys-icon   TO wt_value,
            ws_logsys-logsys TO wt_value,
            ws_logsys-txtmd  TO wt_value.
  ENDLOOP.

  wa_field-tabname    = 'ICON'.
  wa_field-fieldname  = 'ID'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.
  wa_field-tabname    = 'TBDLS'.
  wa_field-fieldname  = 'LOGSYS'.
  wa_field-selectflag = 'X'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.
  wa_field-tabname    = 'TBDLST'.
  wa_field-fieldname  = 'STEXT'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.

  wa_field-tabname    = 'TBDLS'.
  wa_field-fieldname  = 'LOGSYS'.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
       EXPORTING
            tabname      = wa_field-tabname
            fieldname    = wa_field-fieldname
       IMPORTING
            select_value = w_selected_logsys
       TABLES
            fields       = wt_field
            valuetab     = wt_value
       EXCEPTIONS
            OTHERS       = 99.
  IF sy-subrc EQ 0.
    p_logsys = w_selected_logsys.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM f4_oltpsr                                                *
*---------------------------------------------------------------------*
*      -->P_LOGSYS   text
*      -->P_OLTPSR_PATTERN   text
*---------------------------------------------------------------------*
FORM f4_oltpsr USING p_logsys         TYPE logsys
                     p_oltpsr_pattern TYPE roosourcer.
TYPES: t_field TYPE TABLE OF help_value.
TYPES: BEGIN OF value,
         value(30) TYPE c,
       END OF value,
       t_value TYPE TABLE OF value.
DATA: wa_field TYPE LINE OF t_field,
      wt_field TYPE t_field.
DATA: wa_value TYPE value,
      wt_value TYPE t_value.
DATA: ws_oltpsr TYPE s_oltpsr,
      wt_oltpsr TYPE t_oltpsr.
DATA: w_oltpsr_pattern TYPE roosourcer.
DATA: w_selected_oltpsource TYPE roosourcer.

  CASE p_oltpsr_pattern.
  WHEN SPACE.
    w_oltpsr_pattern = '*'.
  WHEN OTHERS.
    w_oltpsr_pattern = p_oltpsr_pattern.
  ENDCASE.

  CALL METHOD lcl_oltpsr=>get_list EXPORTING iv_logsys = p_logsys
                                   IMPORTING et_oltpsr = wt_oltpsr.
  LOOP AT wt_oltpsr INTO ws_oltpsr WHERE oltpsr CP w_oltpsr_pattern.
    APPEND: ws_oltpsr-icon   TO wt_value,
            ws_oltpsr-oltpsr TO wt_value,
            ws_oltpsr-txtsh  TO wt_value.
   ENDLOOP.

  wa_field-tabname    = 'ICON'.
  wa_field-fieldname  = 'ID'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.
  wa_field-tabname    = 'RSOLTPSOURCE'.
  wa_field-fieldname  = 'OLTPSOURCE'.
  wa_field-selectflag = 'X'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.
  wa_field-tabname    = 'RSAOS_S_OLTPSOURCET'.
  wa_field-fieldname  = 'TXTSH'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.

  wa_field-tabname    = 'RSOLTPSOURCE'.
  wa_field-fieldname  = 'OLTPSOURCE'.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
       EXPORTING
            tabname      = wa_field-tabname
            fieldname    = wa_field-fieldname
       IMPORTING
            select_value = w_selected_oltpsource
       TABLES
            fields       = wt_field
            valuetab     = wt_value
       EXCEPTIONS
            OTHERS       = 99.
  IF sy-subrc EQ 0.
    p_oltpsr = w_selected_oltpsource.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_oltpsr_descr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_oltpsr_descr.
DATA: ws_oltpsr TYPE s_oltpsr.

  ws_oltpsr = lcl_oltpsr=>get_details( iv_logsys = p_logsys
                                       iv_oltpsr = p_oltpsr ).
  t_oltpsr = ws_oltpsr-txtsh.
  c_oltpsr = ws_oltpsr-icon.

ENDFORM.                    " get_oltpsr_descr

*&---------------------------------------------------------------------*
*&      Form  get_logsys_descr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_logsys_descr .
DATA: ws_logsys TYPE s_logsys.

  ws_logsys = lcl_logsys=>get_details( p_logsys ).

  t_logsys = ws_logsys-txtmd.
  c_logsys = ws_logsys-icon.

ENDFORM.                    " get_logsys_descr

*---------------------------------------------------------------------*
*       FORM f4_target                                                *
*---------------------------------------------------------------------*
*      -->P_LOGSYS   text
*      -->P_OLTPSR   text
*      -->P_TARGET_PATTERN   text
*---------------------------------------------------------------------*
FORM f4_target USING p_logsys         TYPE logsys
                     p_oltpsr         TYPE roosourcer
                     p_target_pattern TYPE rsinfocube.
TYPES: t_field TYPE TABLE OF help_value.
TYPES: BEGIN OF value,
         value(30) TYPE c,
       END OF value,
       t_value TYPE TABLE OF value.
DATA: w_selected_target TYPE rsinfocube.
DATA: wa_field TYPE LINE OF t_field,
      wt_field TYPE t_field.
DATA: wa_value TYPE value,
      wt_value TYPE t_value.
DATA: ws_target TYPE s_target,
      wt_target TYPE t_target.
DATA: w_target_pattern TYPE rsinfocube.

  CASE p_target_pattern.
  WHEN SPACE.
    w_target_pattern = '*'.
  WHEN OTHERS.
    w_target_pattern = p_target_pattern.
  ENDCASE.

  CALL METHOD lcl_target=>get_list EXPORTING iv_logsys = p_logsys
                                             iv_oltpsr = p_oltpsr
                                   IMPORTING et_target = wt_target.
  LOOP AT wt_target INTO ws_target WHERE target CP w_target_pattern.
    APPEND: ws_target-icon   TO wt_value,
            ws_target-target TO wt_value,
            ws_target-txtlg  TO wt_value.
  ENDLOOP.

  wa_field-tabname    = 'ICON'.
  wa_field-fieldname  = 'ID'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.
  wa_field-tabname    = 'RSMONICTAB'.
  wa_field-fieldname  = 'INFOCUBE'.
  wa_field-selectflag = 'X'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.
  wa_field-tabname    = 'RSDCUBET'.
  wa_field-fieldname  = 'TXTSH'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.

  wa_field-tabname    = 'RSMONICTAB'.
  wa_field-fieldname  = 'INFOCUBE'.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
       EXPORTING
            tabname      = wa_field-tabname
            fieldname    = wa_field-fieldname
       IMPORTING
            select_value = w_selected_target
       TABLES
            fields       = wt_field
            valuetab     = wt_value
       EXCEPTIONS
            OTHERS       = 99.

  IF sy-subrc EQ 0.
    p_target = w_selected_target.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_target_descr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_target_descr.
DATA: ws_target TYPE s_target.

  ws_target = lcl_target=>get_details( p_target ).
  t_target = ws_target-txtlg.
  c_target = ws_target-icon.

ENDFORM.                    " get_target_descr


*---------------------------------------------------------------------*
*       FORM f4_rnr                                                   *
*---------------------------------------------------------------------*
*      -->P_LOGSYS   text
*      -->P_OLTPSR   text
*      ---P_TARGET   text
*---------------------------------------------------------------------*
FORM f4_rnr USING p_logsys TYPE logsys
                  p_oltpsr TYPE roosourcer
                  p_target TYPE rsinfocube.
TYPES: t_field TYPE TABLE OF help_value.
TYPES: BEGIN OF value,
         value(30) TYPE c,
       END OF value,
       t_value TYPE TABLE OF value.
DATA: wa_value TYPE value,
      wt_value TYPE t_value.
DATA: wa_field TYPE LINE OF t_field,
      wt_field TYPE t_field.
DATA: ws_rnr TYPE s_rnr,
      wt_rnr TYPE t_rnr.
DATA: w_selected_rnr TYPE rsrequnr.

  CALL METHOD lcl_rnr=>get_list EXPORTING iv_logsys = p_logsys
                                          iv_oltpsr = p_oltpsr
                                          iv_target = p_target
                                IMPORTING et_rnr    = wt_rnr.
  LOOP AT wt_rnr INTO ws_rnr.
    IF ( lcl_bw_metadata=>is_cube( iv_cube = p_target ) = 'X' ) OR
       ( lcl_bw_metadata=>is_ods( iv_ods = p_target )  = 'X' ).
      APPEND ws_rnr-request_updated  TO wt_value.
    ENDIF.
    APPEND: ws_rnr-text           TO wt_value,
            ws_rnr-datum          TO wt_value,
            ws_rnr-monitor_status TO wt_value.
    wa_value = ws_rnr-records.
    SHIFT wa_value LEFT DELETING LEADING SPACE.
    APPEND: wa_value             TO wt_value,
            ws_rnr-rnr_ext TO wt_value,
            ws_rnr-rnr     TO wt_value.
  ENDLOOP.

  IF ( lcl_bw_metadata=>is_cube( iv_cube = p_target ) = 'X' ) OR
     ( lcl_bw_metadata=>is_ods( iv_ods = p_target )  = 'X' ).
    wa_field-tabname    = 'RSREQDONE'.
    wa_field-fieldname  = 'TSTATUS'.
    APPEND wa_field TO wt_field.
    CLEAR wa_field.
  ENDIF.
  wa_field-tabname    = 'RSLDPIOT'.
  wa_field-fieldname  = 'TEXT'.
  APPEND wa_field TO wt_field.
  CLEAR wa_field.
  wa_field-tabname    = 'RSREQDONE'.
  wa_field-fieldname  = 'DATUM'.
  APPEND wa_field TO wt_field.
  CLEAR wa_field.
  wa_field-tabname    = 'RSREQDONE'.
  wa_field-fieldname  = 'TSTATUS'.
  APPEND wa_field TO wt_field.
  CLEAR wa_field.
  wa_field-tabname    = 'PPHDX'.
  wa_field-fieldname  = 'TOTAL_REC'.
  APPEND wa_field TO wt_field.
  CLEAR wa_field.
  wa_field-tabname    = 'RSB2DYN01'.
  wa_field-fieldname  = 'PARTID_DP'.
  APPEND wa_field TO wt_field.
  CLEAR wa_field.
  wa_field-tabname    = 'RSREQDONE'.
  wa_field-fieldname  = 'RNR'.
  wa_field-selectflag = 'X'.
  APPEND wa_field TO wt_field.
  CLEAR wa_field.


  wa_field-tabname    = 'RSREQDONE'.
  wa_field-fieldname  = 'RNR'.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
       EXPORTING
            tabname      = wa_field-tabname
            fieldname    = wa_field-fieldname
       IMPORTING
            select_value = w_selected_rnr
       TABLES
            fields       = wt_field
            valuetab     = wt_value
       EXCEPTIONS
            OTHERS       = 99.

  IF sy-subrc EQ 0.
    s_rnr-low = w_selected_rnr.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_selection_field_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDNAME   text
*      <--P_FIELDVALUE  text
*----------------------------------------------------------------------*
FORM get_selection_field_value  USING    p_fieldname TYPE dynfnam
                                CHANGING p_fieldvalue.
TYPES: t_dynread TYPE TABLE OF dynpread.
DATA: wa_dynfields TYPE dynpread,
      wt_dynfields TYPE t_dynread.
DATA: w_d020s TYPE d020s.

  w_d020s-prog = g_repid.
  w_d020s-dnum = g_dynnr.

  wa_dynfields-fieldname = p_fieldname.
  APPEND wa_dynfields TO wt_dynfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname                   = w_d020s-prog
            dynumb                   = w_d020s-dnum
            perform_conversion_exits = 'X'
       TABLES
            dynpfields           = wt_dynfields
       EXCEPTIONS
            others               = 1.
   IF sy-subrc = 0.
     READ TABLE wt_dynfields INTO wa_dynfields
                             INDEX 1.
     p_fieldvalue = wa_dynfields-fieldvalue.
   ENDIF.
*  TRANSLATE p_fieldvalue TO UPPER CASE.

ENDFORM.                    " get_selection_field_value

*&---------------------------------------------------------------------*
*&      Form  psa_selection
*&---------------------------------------------------------------------*

*       text
*----------------------------------------------------------------------*
*      <--P_RETURNCODE  text
*      <--PT_SELECTION  text
*      <--P_ODSNAME_TECH  text
*----------------------------------------------------------------------*
FORM psa_selection CHANGING p_returncode   TYPE char01
                            pt_selection   TYPE t_selection
                            p_odsname_tech TYPE rsa_odsdbname.
TYPES: t_sval TYPE TABLE OF sval.
TYPES: BEGIN OF conversion,
        exstructure TYPE rostruc,
        fieldnm     TYPE rsfieldnm,
        convexit    TYPE convexit,
       END OF conversion,
       t_conversion TYPE SORTED TABLE OF conversion
                    WITH UNIQUE KEY fieldnm.
DATA: wa_conversion TYPE conversion,
      wt_conversion TYPE t_conversion.
DATA: w_funcname TYPE rs38l_fnam.
DATA: wr_sval TYPE t_sval.
DATA: w_popup_title TYPE string.
DATA: cla_type TYPE REF TO cl_abap_typedescr,
      cla_str  TYPE REF TO cl_abap_structdescr.
DATA: wt_ac    TYPE abap_compdescr_tab,
      wa_ac    TYPE abap_compdescr.
DATA: wa_selection TYPE selection.
DATA: wa_fields TYPE sval,
      wt_fields TYPE t_sval.
DATA: w_intern TYPE rslow,
      w_extern TYPE rslow.
DATA: dref TYPE REF TO DATA.

FIELD-SYMBOLS: <f> TYPE ANY, <fs> TYPE ANY.


  CALL FUNCTION 'RSAR_ODS_NAME_GET'
       EXPORTING
            i_logsys     = p_logsys
            i_isource    = p_oltpsr
       IMPORTING
            e_odsname_db = p_odsname_tech
       EXCEPTIONS
            parameter_failure    = 1
            no_ods_found         = 2
            no_fields_to_ods     = 3
       OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING parameter_error.
  ENDIF.

  CALL METHOD cl_abap_structdescr=>describe_by_name
       EXPORTING
            p_name         = p_odsname_tech
       RECEIVING
            p_descr_ref    = cla_type.
  cla_str ?= cla_type.
  wt_ac    = cla_str->components.


  CREATE DATA dref TYPE (g_odsname_tech).
  ASSIGN dref->* TO <fs>.

  wa_fields-tabname = p_odsname_tech.
  LOOP AT wt_ac INTO wa_ac WHERE name      <> 'REQUEST'
                             AND name      <> 'DATAPAKID'
                             AND name      <> 'RECORD'
                             AND name      <> 'ROCANCEL'
                             AND type_kind <> 'X'.
    ASSIGN COMPONENT wa_ac-name OF STRUCTURE <fs> TO <f>.
    wa_fields-value = <f>.
    wa_fields-fieldname = wa_ac-name.
    APPEND wa_fields TO wt_fields.
  ENDLOOP.

  w_popup_title =
    'Simulation Workbench: BW 3.x: PSA Data Selection Criteria'.
  CALL FUNCTION 'POPUP_GET_VALUES'
       EXPORTING popup_title = w_popup_title
       IMPORTING returncode  = p_returncode
       TABLES    fields      = wt_fields.

    CHECK NOT wt_fields[] IS INITIAL.
    SELECT ds~exstructure tsfld~fieldnm tsfld~convexit
    INTO TABLE wt_conversion
    FROM rsoltpsource AS ds INNER JOIN rsisosmap AS is
                                    ON ds~oltpsource = is~oltpsource
                                   AND ds~logsys     = is~logsys
                                   AND ds~objvers    = is~objvers
                            INNER JOIN rstsfield AS tsfld
                                 ON is~transtru = tsfld~transtru
    FOR ALL ENTRIES IN wt_fields
    WHERE is~oltpsource  =  p_oltpsr
      AND is~logsys      =  p_logsys
      AND is~objvers     =  'A'
      AND tsfld~convexit <> SPACE
      AND tsfld~fieldnm  =  wt_fields-fieldname.
    LOOP AT wt_fields INTO wa_fields.
      ASSIGN COMPONENT wa_fields-fieldname OF STRUCTURE <fs> TO <f>.
      <f> = wa_fields-value.
      READ TABLE wt_conversion INTO wa_conversion
        WITH TABLE KEY fieldnm = wa_fields-fieldname.
      IF sy-subrc = 0.
        CONCATENATE 'CONVERSION_EXIT_' wa_conversion-convexit
                    '_INPUT' INTO w_funcname.
        SELECT SINGLE funcname
        INTO w_funcname
        FROM tfdir
        WHERE funcname = w_funcname.
        IF sy-subrc = 0.
         CALL FUNCTION w_funcname
             EXPORTING
                  input  = <f>
             IMPORTING
                  output = <f>.
        ELSE.
          w_intern = <f>.
          CALL FUNCTION 'RSA0_INTO2EXT_GET'
               DESTINATION p_logsys
               EXPORTING
                    i_structure  = wa_conversion-exstructure
                    i_fieldname  = wa_conversion-fieldnm
                    i_convexit   = wa_conversion-convexit
                    i_intern     = w_intern
                    i_type       = '_INPUT'
                    i_datasource = p_oltpsr
               IMPORTING
                    e_extern     = w_extern
               EXCEPTIONS
                    not_exist    = 1
                    others       = 2.
          <f> = w_extern.
        ENDIF.
      ENDIF.
      IF ( NOT <f> IS INITIAL ).
        wa_selection-fieldname = wa_fields-fieldname.
        wa_selection-value     = <f>.
        APPEND wa_selection TO pt_selection.
      ENDIF.
    ENDLOOP.

ENDFORM.                    " psa_selection

*&---------------------------------------------------------------------*
*&      Form  select_from_psa
*&---------------------------------------------------------------------*
FORM select_from_psa .
DATA: dref   TYPE REF TO data,
      dref_t TYPE REF TO data.
DATA: wa_selection TYPE selection.
DATA: wa_rnr TYPE rsrange.
DATA: wr_rnr LIKE s_rnr[].
DATA: w_selection TYPE string.
FIELD-SYMBOLS: <f>.

  CREATE DATA dref TYPE (g_odsname_tech).
  ASSIGN dref->* TO <f>.

  CREATE DATA dref_t TYPE TABLE OF (g_odsname_tech).
  ASSIGN dref_t->* TO <gt_psa>.


  LOOP AT gt_selection INTO wa_selection.
    IF w_selection = SPACE.
      CONCATENATE wa_selection-fieldname
                  ' ='
                  ' '''
                  wa_selection-value
                  ''''
                  INTO w_selection.
    ELSE.
      CONCATENATE w_selection
                  'AND'
                  wa_selection-fieldname
                  INTO w_selection
                  SEPARATED BY SPACE.
      CONCATENATE w_selection
                  '='
                  ''''
                  INTO w_selection
                  SEPARATED BY SPACE.
      CONCATENATE w_selection
                  wa_selection-value
                  ''''
                  INTO w_selection.
    ENDIF.
  ENDLOOP.

  IF ( NOT s_rnr[] IS INITIAL ) AND ( p_ndtrnr = SPACE ).
    PERFORM get_rnr_selection USING    s_rnr[]
                              CHANGING w_selection.
  ELSEIF ( NOT s_rnr[] IS INITIAL ) AND ( p_ndtrnr = 'X' ).
    wa_rnr-sign   = 'I'.
    wa_rnr-option = 'EQ'.
    SELECT rnr
    INTO wa_rnr-low
    FROM rsmonicdp
    FOR ALL ENTRIES IN s_rnr
    WHERE rnr   = s_rnr-low
      AND icube = p_target
      AND dp_nr = '999999'.
      APPEND wa_rnr TO wr_rnr.
    ENDSELECT.
    MESSAGE i000(oo) WITH sy-dbcnt
                        'not deleted request(s) searched for data'.
    IF NOT wr_rnr[] IS INITIAL.
      PERFORM get_rnr_selection USING    wr_rnr[]
                                CHANGING w_selection.
    ENDIF.
  ELSEIF ( s_rnr[] IS INITIAL ) AND ( p_ndtrnr = 'X' ).
    wa_rnr-sign   = 'I'.
    wa_rnr-option = 'EQ'.
    SELECT rnr
    INTO wa_rnr-low
    FROM rsmonicdp
    WHERE icube = p_target
      AND dp_nr = '999999'.
      APPEND wa_rnr TO wr_rnr.
    ENDSELECT.
    MESSAGE i000(oo) WITH sy-dbcnt
                        'not deleted request(s) searched for data'.
    IF NOT wr_rnr[] IS INITIAL.
      PERFORM get_rnr_selection USING    wr_rnr[]
                                CHANGING w_selection.
    ENDIF.
  ENDIF.

  SELECT *
  INTO <f>
  FROM (g_odsname_tech)
  WHERE (w_selection).
    INSERT <f> INTO TABLE <gt_psa>.
  ENDSELECT.

ENDFORM.                    " select_from_psa


*&---------------------------------------------------------------------*
*&      Form  display_psa_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_psa_selection.

  PERFORM alv_build_eventtab CHANGING gt_events.
  PERFORM alv_grid_display.

ENDFORM.                    " display_psa_selection

*&---------------------------------------------------------------------*
*&      Form  alv_build_eventtab

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_EVENTS[]  text
*----------------------------------------------------------------------*

FORM alv_build_eventtab CHANGING    pt_events TYPE slis_t_event.
DATA: wa_event   TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type     = 0
       IMPORTING
            et_events       = pt_events.

  READ TABLE pt_events WITH KEY name = slis_ev_user_command
                           INTO wa_event.
  IF sy-subrc = 0.
    wa_event-form = 'ALV_USER_COMMAND_FORM'.
    APPEND wa_event TO pt_events.
  ENDIF.

ENDFORM.                    " alv_build_eventtab

*&---------------------------------------------------------------------*
*&      Form  alv_grid_display
*&---------------------------------------------------------------------*
FORM alv_grid_display .
DATA: ws_layout TYPE slis_layout_alv.
DATA: wt_fcat type slis_t_fieldcat_alv.
DATA: w_exit(1) TYPE c,
      w_exit_caused_by_caller TYPE c.


  PERFORM populate_layout CHANGING ws_layout.
  PERFORM populate_field_catalogues CHANGING wt_fcat
                                             gt_fieldcat.
  PERFORM populate_outtab.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'ALV_SET_PF_STATUS'
            is_layout                = ws_layout
            it_fieldcat              = wt_fcat
            it_events                = gt_events
       IMPORTING
            e_exit_caused_by_caller  = w_exit_caused_by_caller
       TABLES
            t_outtab                 = <gt_outtab>[].

ENDFORM.                    " alv_grid_display


*---------------------------------------------------------------------*
*       FORM user_command_form                                        *
*---------------------------------------------------------------------*

*       ........                                                      *

*---------------------------------------------------------------------*
*  -->  r_ucomm                                                       *
*  -->  rs_selfield                                                   *
*---------------------------------------------------------------------*
FORM alv_user_command_form USING r_ucomm LIKE sy-ucomm
                                 rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN 'UPDA'.
      PERFORM simulation.
    WHEN 'CHAN'.
      PERFORM change_simulation_data.
      rs_selfield-refresh = 'X'.
    WHEN 'F03' or 'F12' or 'F15'.
      rs_selfield-exit = 'X'.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM alv_set_pf_status                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

*  -->  pt_extab                                                      *

*---------------------------------------------------------------------*
FORM alv_set_pf_status  USING pt_extab TYPE slis_t_extab.
DATA: wt_ucomm TYPE TABLE OF sy-ucomm.

  APPEND: 'CREA' TO wt_ucomm,
          'DISP' TO wt_ucomm,
          'CHTX' TO wt_ucomm,
          'SIV0' TO wt_ucomm,
          'CHAL' TO wt_ucomm,
          '%PC'  TO wt_ucomm,
          'DELM' TO wt_ucomm,
          'PROT' TO wt_ucomm,
          'RCCT' TO wt_ucomm,
          'P'    TO wt_ucomm,
          'P-'   TO wt_ucomm,
          'P+'   TO wt_ucomm,
          'P++'  TO wt_ucomm,
          '&CRL' TO wt_ucomm,
          '&CRR' TO wt_ucomm,
          '&CRE' TO wt_ucomm,
          '&CRB' TO wt_ucomm,
          '&LFO' TO wt_ucomm,
          '&RNT' TO wt_ucomm,
          'CRTX' TO wt_ucomm,
          'DITX' TO wt_ucomm,
          'CHTX' TO wt_ucomm,
          'SIT0' TO wt_ucomm,
          'SIC0' TO wt_ucomm.

  SET PF-STATUS 'DATA_DISPLAY' EXCLUDING wt_ucomm
                               OF PROGRAM 'SAPLRSAODS'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  populate_layout
*&---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      <--PS_LAYOUT  text
*----------------------------------------------------------------------*
FORM populate_layout  CHANGING ps_layout TYPE slis_layout_alv.

  ps_layout-window_titlebar =
    'Simulation Workbench: BW 3.x: Pre-Selected PSA Data'.
  ps_layout-box_fieldname   = 'CHECKBOX'.

ENDFORM.                    " populate_layout


*&---------------------------------------------------------------------*
*&      Form  populate_field_catalogues
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--PT_FCAT  text
*      <--PT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM populate_field_catalogues  CHANGING pt_fcat TYPE
                                                   slis_t_fieldcat_alv
                                         pt_fieldcat TYPE lvc_t_fcat.
TYPES: BEGIN OF conversion,
        fieldnm  TYPE rsfieldnm,
        convexit TYPE convexit,
       END OF conversion,
       t_conversion TYPE SORTED TABLE OF conversion
                    WITH UNIQUE KEY fieldnm.
DATA: wa_conversion TYPE conversion,
      wt_conversion TYPE t_conversion.
DATA: wa_fcat type slis_fieldcat_alv,
      wa_fieldcat TYPE lvc_s_fcat.
DATA: wp_table TYPE REF TO DATA.
FIELD-SYMBOLS: <fs_psa>.
DATA: dref   TYPE REF TO data.
DATA: w_structure_name TYPE tabname.
DATA: w_funcname TYPE rs38l_fnam.

  w_structure_name = g_odsname_tech.

  CREATE DATA dref TYPE (g_odsname_tech).
  ASSIGN dref->* TO <fs_psa>.

  wa_fcat-fieldname = 'CHECKBOX'.
  wa_fcat-checkbox  = 'X'.
  wa_fcat-inttype   = 'C'.
  wa_fcat-datatype  = 'CHAR'.
  wa_fcat-intlen    = 1.
  wa_fcat-col_pos   = 1.
  wa_fcat-no_out    = 'X'.
  APPEND wa_fcat to pt_fcat.
  MOVE-CORRESPONDING wa_fcat TO wa_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = w_structure_name
       CHANGING
            ct_fieldcat      = pt_fcat
       EXCEPTIONS
            others           = 1.
  CHECK NOT pt_fcat[] IS INITIAL.
  SELECT tsfld~fieldnm tsfld~convexit
  INTO TABLE wt_conversion
  FROM rsisosmap AS is INNER JOIN rstsfield AS tsfld
                               ON is~transtru = tsfld~transtru
  FOR ALL ENTRIES IN pt_fcat
  WHERE is~oltpsource  =  p_oltpsr
    AND is~logsys      =  p_logsys
    AND is~objvers     =  'A'
    AND tsfld~fieldnm  =  pt_fcat-fieldname
    AND tsfld~convexit <> SPACE.
  LOOP AT pt_fcat INTO wa_fcat.
    READ TABLE wt_conversion INTO wa_conversion
      WITH TABLE KEY fieldnm = wa_fcat-fieldname.
    IF sy-subrc = 0.
      CONCATENATE 'CONVERSION_EXIT_' wa_conversion-convexit
                  '_OUTPUT' INTO w_funcname.
      SELECT SINGLE funcname
      INTO w_funcname
      FROM tfdir
      WHERE funcname = w_funcname.
      IF sy-subrc = 0.
        CONCATENATE '=='
                    wa_conversion-convexit
                    INTO wa_fcat-edit_mask.
        MODIFY pt_fcat FROM wa_fcat TRANSPORTING edit_mask.
      ENDIF.
    ENDIF.
  ENDLOOP.

  APPEND wa_fieldcat TO pt_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = w_structure_name
       CHANGING
            ct_fieldcat      = pt_fieldcat.

ENDFORM.                    " populate_field_catalogues

*&---------------------------------------------------------------------*
*&      Form  populate_outtab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM populate_outtab.
DATA: dref   TYPE REF TO DATA.
FIELD-SYMBOLS: <fs_psa>,
               <fs_outtab>.

  CALL METHOD cl_alv_table_create=>CREATE_DYNAMIC_TABLE
                        EXPORTING it_fieldcatalog = gt_fieldcat
                        IMPORTING ep_table        = dref.
  ASSIGN dref->* TO <gt_outtab>.

  CREATE DATA dref TYPE (g_odsname_tech).
  ASSIGN dref->* TO <fs_psa>.


  ASSIGN LOCAL COPY OF INITIAL LINE OF <gt_outtab> TO <fs_outtab>.

  LOOP AT <gt_psa> ASSIGNING <fs_psa>.
    MOVE-CORRESPONDING <fs_psa> TO <fs_outtab>.
    APPEND <fs_outtab> TO <gt_outtab>.
  ENDLOOP.

ENDFORM.                    " populate_outtab

*&---------------------------------------------------------------------*
*&      Form  get_selected_psa_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LINES  text
*----------------------------------------------------------------------*
FORM get_selected_psa_data  CHANGING p_lines TYPE i.
DATA: dref   TYPE REF TO DATA.
FIELD-SYMBOLS: <fs_psa>,
               <fs_outtab>.
FIELD-SYMBOLS: <f>.

  ASSIGN LOCAL COPY OF INITIAL LINE OF <gt_outtab> TO <fs_outtab>.

  CREATE DATA dref TYPE (g_odsname_tech).
  ASSIGN dref->* TO <fs_psa>.


  CLEAR <gt_psa>[].
  LOOP AT <gt_outtab> ASSIGNING <fs_outtab>.
    ASSIGN COMPONENT 1 OF STRUCTURE <fs_outtab> TO <f>.
    IF <f> = 'X'.
      MOVE-CORRESPONDING <fs_outtab> TO <fs_psa>.
      APPEND <fs_psa> TO <gt_psa>.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE <gt_psa> LINES p_lines.

ENDFORM.                    " get_selected_psa_data

*&---------------------------------------------------------------------*
*&      Form  simulation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM simulation.
DATA: ws_minfohead  TYPE rsminfohead,
      wt_minfocubes TYPE rssm_t_infocube.
DATA: w_select_sim TYPE c VALUE 'X',
      w_datapakid  TYPE rsdatapid VALUE '000000'.
DATA: w_lines TYPE i.
FIELD-SYMBOLS: <f>.

  PERFORM get_selected_psa_data CHANGING w_lines.
  IF w_lines = 0.
    MESSAGE i000(oo) WITH 'No records selected for simulation'.
    EXIT.
  ENDIF.

  PERFORM get_simulation_parameters  USING    w_select_sim
                                              w_datapakid
                                     CHANGING ws_minfohead
                                              wt_minfocubes.
  CALL FUNCTION 'RSAR_ODS_UPDATE_DATA'
       EXPORTING
            i_s_minfo_head    = ws_minfohead
       TABLES
            i_t_minfo_cubes   = wt_minfocubes
            e3_t_data         = <gt_psa>.

ENDFORM.                    " simulation

*&---------------------------------------------------------------------*
*&      Form  get_simulation_parameters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SELECT_SIM  text
*      -->P_DATAPAKID  text
*      <--PS_MINFOHEAD  text
*      <--PS_MINFOCUBES  text
*----------------------------------------------------------------------*
FORM get_simulation_parameters USING    p_select_sim  TYPE char01
                                        p_datapakid   TYPE rsdatapid
                               CHANGING ps_minfohead  TYPE rsminfohead
                                        pt_minfocubes TYPE
                                                        rssm_t_infocube.
DATA: wa_rnr TYPE rsrange.
DATA: ws_minfo TYPE rssm_s_minfo.
DATA: w_rnr TYPE rsrequnr.
DATA: w_isource TYPE rsa_isource.
DATA: w_odsname_tech TYPE rsa_odsdbname.
FIELD-SYMBOLS: <fs_outtab>,
               <f>.

  IF NOT s_rnr[] IS INITIAL.
    READ TABLE s_rnr INTO wa_rnr INDEX 1.
    w_rnr = wa_rnr-low.
  ELSE.
    ASSIGN LOCAL COPY OF INITIAL LINE OF <gt_outtab> TO <fs_outtab>.
    READ TABLE <gt_outtab> INTO <fs_outtab> INDEX 1.
    ASSIGN COMPONENT 'REQUEST' OF STRUCTURE <fs_outtab> TO <f>.
    w_rnr = <f>.
  ENDIF.

  SELECT typ rnrtyp
  INTO (ws_minfo-type, ws_minfo-istype)
  FROM rsseldone UP TO 1 ROWS
  WHERE rnr = w_rnr.
  ENDSELECT.
  IF sy-subrc <> 0.
    CLEAR: ws_minfo-type,
           ws_minfo-istype.
  ENDIF.

  SELECT SINGLE isource
  INTO w_isource
  FROM rsisosmap
  WHERE oltpsource = p_oltpsr
    AND logsys     = p_logsys
    AND objvers    ='A'.
  IF sy-subrc <> 0.
    CLEAR: w_isource.
  ENDIF.

  CALL FUNCTION 'RSAR_ODS_NAME_GET'
       EXPORTING
            i_logsys     = p_logsys
            i_isource    = p_oltpsr
       IMPORTING
            e_odsname_db = w_odsname_tech.

  ws_minfo-requnr     = w_rnr.
  ws_minfo-datapakid  = p_datapakid.
  ws_minfo-logsys     = p_logsys.
  ws_minfo-oltpsource = p_oltpsr.
  ws_minfo-isource    = w_isource.
  ws_minfo-simulation = 'X'.
  ws_minfo-select_sim = p_select_sim.

  CALL FUNCTION 'RSSM_UNIQUE_ID'
       IMPORTING
            e_uni_idc25 = ws_minfo-simrequnr.

  ws_minfo-debugmode    = p_dbgupd.
  ws_minfo-debugmode_ts = p_dbgtrn.

  MOVE-CORRESPONDING ws_minfo TO ps_minfohead.

  APPEND p_target TO pt_minfocubes.

ENDFORM.                    " get_simulation_parameters

*&---------------------------------------------------------------------*
*&      Form  change_simulation_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_simulation_data.
TYPES: t_sval TYPE TABLE OF sval.
DATA: cla_type TYPE REF TO cl_abap_typedescr,
      cla_str  TYPE REF TO cl_abap_structdescr.
DATA: wt_ac    TYPE abap_compdescr_tab,
      wa_ac    TYPE abap_compdescr.
DATA: w_lines TYPE i,
      w_tabix TYPE sytabix.
DATA: wa_fields TYPE sval,
      wt_fields TYPE t_sval.
DATA: w_popup_title(30) TYPE c.
DATA: w_returncode TYPE char01.
FIELD-SYMBOLS: <f>,
               <fs_outtab>.


  ASSIGN LOCAL COPY OF INITIAL LINE OF <gt_outtab> TO <fs_outtab>.
  LOOP AT <gt_outtab> INTO <fs_outtab>.
    ASSIGN COMPONENT 1 OF STRUCTURE <fs_outtab> TO <f>.
    IF <f> = 'X'.
      w_lines = w_lines + 1.
      w_tabix = sy-tabix.
    ENDIF.
  ENDLOOP.
  IF w_lines <> 1.
    MESSAGE i000(oo) WITH 'Only one PSA record at a time'
                          'should be selected for change'.
    EXIT.
  ELSE.
    READ TABLE <gt_outtab> INTO <fs_outtab> INDEX w_tabix.
  ENDIF.

  CALL METHOD cl_abap_structdescr=>describe_by_name
       EXPORTING
            p_name         = g_odsname_tech
       RECEIVING
            p_descr_ref    = cla_type.
  cla_str ?= cla_type.
  wt_ac    = cla_str->components.

  wa_fields-tabname = g_odsname_tech.
  LOOP AT wt_ac INTO wa_ac WHERE name      <> 'REQUEST'
                             AND name      <> 'DATAPAKID'
                             AND name      <> 'RECORD'
                             AND type_kind <> 'X'.
*                            AND name <> 'ROCANCEL'.
    ASSIGN COMPONENT wa_ac-name OF STRUCTURE <fs_outtab> TO <f>.
    wa_fields-value     = <f>.
    wa_fields-fieldname = wa_ac-name.
    APPEND wa_fields TO wt_fields.
  ENDLOOP.

  w_popup_title = 'Change PSA Data'.
  CALL FUNCTION 'POPUP_GET_VALUES'
       EXPORTING popup_title = w_popup_title
       IMPORTING returncode  = w_returncode
       TABLES    fields      = wt_fields.
  IF w_returncode = 'A'.
    MESSAGE i000(oo) WITH 'PSA Data change was cancelled'.
    EXIT.
  ENDIF.

  LOOP AT wt_fields INTO wa_fields.
    ASSIGN COMPONENT wa_fields-fieldname OF STRUCTURE <fs_outtab>
                                         TO <f>.
    <f> = wa_fields-value.
  ENDLOOP.
  MODIFY <gt_outtab> FROM <fs_outtab> INDEX w_tabix.

ENDFORM.                    " change_simulation_data

*&---------------------------------------------------------------------*
*&      Form  f4_packid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_RNR  text
*----------------------------------------------------------------------*
FORM f4_packid  USING  p_rnr TYPE rsrequnr.
TYPES: t_field TYPE TABLE OF help_value.
TYPES: BEGIN OF value,
         value(30) TYPE c,
       END OF value,
       t_value TYPE TABLE OF value.
DATA: wa_field TYPE LINE OF t_field,
      wt_field TYPE t_field.
DATA: wa_value TYPE value,
      wt_value TYPE t_value.
DATA: ws_packid TYPE s_packid,
      wt_packid TYPE t_packid.
DATA: w_selected_packid TYPE rsdatapid.


  CALL METHOD lcl_packid=>get_list EXPORTING iv_rnr    = p_rnr
                                   IMPORTING et_packid = wt_packid.
  LOOP AT wt_packid INTO ws_packid.
    wa_value = ws_packid-packid.
    SHIFT wa_value LEFT DELETING LEADING '0'.
    APPEND wa_value TO wt_value.
    wa_value = ws_packid-records.
    SHIFT wa_value LEFT DELETING LEADING SPACE.
    APPEND wa_value TO wt_value.
  ENDLOOP.

  CLEAR wt_field[].
  wa_field-tabname    = 'RSMONIPTAB'.
  wa_field-fieldname  = 'DP_NR'.
  wa_field-selectflag = 'X'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.
  wa_field-tabname    = 'PPHDX'.
  wa_field-fieldname  = 'TOTAL_REC'.
  APPEND wa_field TO wt_field.
  CLEAR: wa_field.

  wa_field-tabname    = 'RSMONIPTAB'.
  wa_field-fieldname  = 'DP_NR'.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
       EXPORTING
            tabname      = wa_field-tabname
            fieldname    = wa_field-fieldname
       IMPORTING
            select_value = w_selected_packid
       TABLES
            fields       = wt_field
            valuetab     = wt_value
       EXCEPTIONS

            OTHERS       = 99.

  IF sy-subrc EQ 0.
    p_packid = w_selected_packid.
  ENDIF.

ENDFORM.                    " f4_packid

*&---------------------------------------------------------------------*
*&      Form  simulate_tech
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM simulate_tech.
DATA: ws_minfohead  TYPE rsminfohead,
      ws_minfo      TYPE rssm_s_minfo,
      wt_minfocubes TYPE rssm_t_infocube.
DATA: w_select_sim  TYPE char01.
DATA: wt_idocstate TYPE rsarr_t_idocstate.
DATA: w_subrc TYPE sysubrc.
DATA: w_rnr TYPE rsrequnr.
DATA: ws_rnr TYPE rsrange.


  IF p_sl_sl = 'X'.
    w_select_sim = 'X'.
  ELSE.
    w_select_sim = SPACE.
  ENDIF.
  PERFORM get_simulation_parameters USING    w_select_sim
                                             p_packid
                                    CHANGING ws_minfohead
                                             wt_minfocubes.
  MOVE-CORRESPONDING ws_minfohead TO ws_minfo.
  ws_minfo-t_infocube[] = wt_minfocubes[].

  READ TABLE s_rnr INTO ws_rnr INDEX 1.
  IF sy-subrc = 0.
    w_rnr = ws_rnr-low.
  ENDIF.
  CALL FUNCTION 'RSAR_REQUEST_INTERPRETER_1'
       EXPORTING
            i_request   = w_rnr
            i_datapakid = p_packid
            i_s_minfo   = ws_minfo
       CHANGING
            c_t_idocstate = wt_idocstate
            c_subrc       = w_subrc.

ENDFORM.                    " simulate_tech

*---------------------------------------------------------------------*
*       CLASS lcl_local_exception  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_local_exception IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    local_text = text.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_bw_metadata IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_bw_metadata IMPLEMENTATION.
*---------------------------------------------------------------------*
* METHOD is_cube
*---------------------------------------------------------------------*
  METHOD is_cube.
    DATA: w_cube TYPE rsinfocube.

    SELECT SINGLE infocube
    INTO w_cube
    FROM rsdcube
    WHERE infocube = iv_cube
      AND objvers  = 'A'.
    IF sy-subrc = 0.
      rv_is_cube = 'X'.
    ELSE.
      rv_is_cube = SPACE.
    ENDIF.
  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD is_ods
*---------------------------------------------------------------------*
  METHOD is_ods.
    DATA: w_ods TYPE rsdodsobject.

    SELECT SINGLE odsobject
    INTO w_ods
    FROM rsdodso
    WHERE odsobject = iv_ods
      AND objvers   = 'A'.
    IF sy-subrc = 0.
      rv_is_ods = 'X'.
    ELSE.
      rv_is_ods = SPACE.
    ENDIF.
  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD is_infoobject
*---------------------------------------------------------------------*
  METHOD is_infoobject.
    DATA: w_infoobject TYPE rsdodsobject.

    SELECT SINGLE iobjnm
    INTO w_infoobject
    FROM rsdiobj
    WHERE iobjnm  = iv_infoobject
      AND objvers = 'A'.
    IF sy-subrc = 0.
      rv_is_infoobject = 'X'.
    ELSE.
      rv_is_infoobject = SPACE.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_logsys IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_logsys IMPLEMENTATION.
*---------------------------------------------------------------------*
* METHOD check_if_exists
*---------------------------------------------------------------------*
  METHOD check_if_exists.
  DATA: w_text TYPE string.
  DATA: w_logsys TYPE logsys.

    IF iv_logsys = SPACE.
      w_text = 'Source System is not specified'.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    SELECT SINGLE logsys
    INTO w_logsys
    FROM tbdls
    WHERE logsys = iv_logsys.
    IF sy-subrc <> 0.
      CONCATENATE 'Source System' iv_logsys 'does not exist'
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_list
*---------------------------------------------------------------------*
  METHOD get_list.
  DATA: ws_logsys TYPE s_logsys.

    SELECT logsys
    INTO ws_logsys-logsys
    FROM tbdls.
      ws_logsys = get_details( ws_logsys-logsys ).
      APPEND ws_logsys TO et_logsys.
    ENDSELECT.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_details
*---------------------------------------------------------------------*
  METHOD get_details.

    SELECT SINGLE logsys stext
    INTO (rs_logsys-logsys, rs_logsys-txtmd)
    FROM tbdlst
    WHERE langu  = 'E'
      AND logsys = iv_logsys.
    IF sy-subrc <> 0.
      CLEAR: rs_logsys-txtmd.
    ENDIF.

    CALL FUNCTION 'RSAR_LOGICAL_SYSTEM_GET'
         EXPORTING
              i_logsys = iv_logsys
         IMPORTING
              e_icon   = rs_logsys-icon.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_oltpsr IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_oltpsr IMPLEMENTATION.
*---------------------------------------------------------------------*
* METHOD check_if_exists
*---------------------------------------------------------------------*
  METHOD check_if_exists.
  DATA: w_text TYPE string.
  DATA: w_oltpsr TYPE roosourcer,
        w_type   TYPE rsrequtype.

    IF iv_oltpsr = SPACE.
      w_text = 'DataSource is not specified'.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.
*
    SELECT SINGLE oltpsource type
    INTO (w_oltpsr, w_type)
    FROM rsoltpsource
    WHERE oltpsource = iv_oltpsr
      AND logsys     = iv_logsys
      AND objvers    = 'A'.
    CASE sy-subrc.
    WHEN 0.
      IF w_type = 'H'.
        CONCATENATE p_oltpsr 'is a Hierarchy DataSource.'
                    'Simulation is not supported'
                    INTO w_text SEPARATED BY SPACE.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      ENDIF.
    WHEN OTHERS.
      CONCATENATE 'Active DataSource' p_oltpsr
                  'does not exist in Source System'
                  INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDCASE.
*
    SELECT SINGLE is~oltpsource
    INTO w_oltpsr
    FROM rsoltpsource AS is INNER JOIN rsisosmap AS ismap
                                    ON is~logsys     = ismap~logsys
                                   AND is~oltpsource = ismap~oltpsource
                                   AND is~objvers    = ismap~objvers
    WHERE is~oltpsource = iv_oltpsr
      AND is~logsys     = iv_logsys
      AND is~objvers    = 'A'.
    IF sy-subrc <> 0.
      CONCATENATE 'No active Transfer Rules for DataSource' iv_oltpsr
                   'Source System' iv_logsys
                   INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.
*
    SELECT SINGLE ismap~oltpsource
    INTO w_oltpsr
    FROM rsisosmap AS ismap INNER JOIN rsts AS ts
                                    ON ismap~transtru = ts~transtru
                                   AND ismap~objvers  = ts~objvers
    WHERE ismap~logsys     = iv_logsys
      AND ismap~oltpsource = iv_oltpsr
      AND ismap~objvers    = 'A'.
    IF sy-subrc <> 0.
      CONCATENATE 'No active Transfer Structure for DataSource'
                  iv_oltpsr 'and Source System' iv_logsys
                  INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_infosource
*---------------------------------------------------------------------*
  METHOD get_infosource.

  SELECT SINGLE isource transtru isostype istype
  INTO (cv_isource, cv_transtru, cv_isostype, cv_istype)
    FROM rsisosmap
    WHERE logsys      = iv_logsys
      AND oltpsource  = iv_oltpsr
      AND objvers     = 'A'.
    IF sy-subrc <> 0.
      CLEAR: cv_isource, cv_transtru, cv_isostype, cv_istype.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_list
*---------------------------------------------------------------------*
  METHOD get_list.
  DATA: ws_oltpsr TYPE s_oltpsr.
  DATA: w_type TYPE rsrequtype.

    SELECT is~oltpsource is~type
    INTO (ws_oltpsr-oltpsr, w_type)
    FROM rsoltpsource AS is INNER JOIN rsisosmap AS ismap
                                    ON is~logsys     = ismap~logsys
                                   AND is~oltpsource = ismap~oltpsource
                                   AND is~objvers    = ismap~objvers
    WHERE is~logsys  = iv_logsys
      AND is~objvers = 'A'
      AND is~type IN ('T', 'M', 'D')
    ORDER BY is~type.
      ws_oltpsr = get_details( iv_logsys = iv_logsys
                               iv_oltpsr = ws_oltpsr-oltpsr ).
      APPEND ws_oltpsr TO et_oltpsr.
    ENDSELECT.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_details
*---------------------------------------------------------------------*
  METHOD get_details.
  DATA: w_s_oltpsource  TYPE rsaos_s_oltpsource,
        w_s_oltpsourcet TYPE rsaos_s_oltpsourcet.
  DATA: w_with_buf TYPE rs_bool.

    rs_oltpsr-oltpsr = iv_oltpsr.
    CALL FUNCTION 'RSAOS_OLTPSOURCE_GET'
         EXPORTING
              i_oltpsource    = iv_oltpsr
              i_logsys        = iv_logsys
              i_objvers       = 'A'
              i_langu         = 'E'
              i_with_buf      = w_with_buf
         IMPORTING
              e_s_oltpsource  = w_s_oltpsource
              e_s_oltpsourcet = w_s_oltpsourcet.
    rs_oltpsr-txtsh = w_s_oltpsourcet-txtsh.
    CASE w_s_oltpsource-type.
    WHEN 'T'.
      rs_oltpsr-icon = icon_text_act.
    WHEN 'M'.
      rs_oltpsr-icon = icon_master_data_act.
    WHEN 'D'.
      rs_oltpsr-icon = icon_moving_data_act.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_target IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_target IMPLEMENTATION.
*---------------------------------------------------------------------*
* METHOD check_if_exists
*---------------------------------------------------------------------*
  METHOD check_if_exists.
  DATA: w_text TYPE string.
  DATA: w_isource TYPE rsisource.

    IF iv_target = SPACE.
      w_text = 'DataTarget is not specified'.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.
*
    IF ( lcl_bw_metadata=>is_ods( iv_target )        <> 'X' ) AND
       ( lcl_bw_metadata=>is_cube( iv_target )       <> 'X' ) AND
       ( lcl_bw_metadata=>is_infoobject( iv_target ) <> 'X' ).
      CONCATENATE iv_target 'is not active ODS, Cube or InfoObject'
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.
*
    IF ( is_direct_update( iv_logsys = iv_logsys
                           iv_oltpsr = iv_oltpsr
                           iv_target = iv_target ) <> 'X' ).
      TRY.
        is_flexible_update( iv_logsys = iv_logsys
                            iv_oltpsr = iv_oltpsr
                            iv_target = iv_target ).
        CATCH lcl_local_exception INTO local_exception.
          RAISE EXCEPTION TYPE lcl_local_exception
            EXPORTING TEXT = local_exception->local_text.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD is_flexible_update
*---------------------------------------------------------------------*
  METHOD is_flexible_update.
  DATA: w_updid   TYPE rsupdid,
        w_isource TYPE rsisource.
  DATA: w_target_t TYPE rsinfocube.
  DATA: w_text TYPE string.

    CONCATENATE iv_target '$T' INTO w_target_t.
    SELECT upd~updid upd~isource
    INTO (w_updid, w_isource)
    FROM rsupdinfo AS upd INNER JOIN rsisosmap AS trn
                                  ON trn~isource = upd~isource
                                 AND trn~objvers = upd~objvers
    UP TO 1 ROWS
    WHERE trn~logsys      = iv_logsys
      AND trn~oltpsource  = iv_oltpsr
      AND trn~objvers     = 'A'
      AND upd~infocube   IN (iv_target, w_target_t).
    ENDSELECT.
    IF sy-subrc <> 0.
      CONCATENATE 'No active Update Rules from InfoSource' w_isource
                  'to Target' iv_target INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD is_direct_update
*---------------------------------------------------------------------*
  METHOD is_direct_update.
  DATA: w_isource  TYPE rsisource.

    SELECT SINGLE ismap~isource
    INTO w_isource
    FROM rsisosmap AS ismap INNER JOIN rsdiobj AS iobj
                                    ON ismap~isource = iobj~iobjnm
                                   AND ismap~objvers = iobj~objvers
    WHERE ismap~logsys      = iv_logsys
      AND ismap~oltpsource  = iv_oltpsr
      AND ismap~objvers     = 'A'
      AND ismap~istype     IN ('M', 'T')
      AND ismap~isource     = iv_target.
    CASE sy-subrc.
    WHEN 0.
      rv_direct_update = 'X'.
    WHEN OTHERS.
      rv_direct_update = SPACE.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_list
*---------------------------------------------------------------------*
  METHOD get_list.
  DATA: ws_target TYPE s_target.

    SELECT infocube AS target
    INTO ws_target-target
    FROM rsupdinfo AS upd INNER JOIN rsisosmap AS trn
                                  ON trn~isource = upd~isource
                                 AND trn~objvers = upd~objvers
    WHERE trn~logsys      = iv_logsys
      AND trn~oltpsource  = iv_oltpsr
      AND trn~objvers     = 'A'.
      IF ws_target-target CP '*$T*'.
        REPLACE '$T' WITH SPACE INTO ws_target-target.
      ENDIF.
      ws_target = get_details( iv_target = ws_target-target ).
      APPEND ws_target TO et_target.
      CLEAR ws_target.
    ENDSELECT.
*
    SELECT SINGLE ismap~isource
    INTO ws_target-target
    FROM rsisosmap AS ismap INNER JOIN rsdiobj AS iobj
                                    ON ismap~isource = iobj~iobjnm
                                   AND ismap~objvers = iobj~objvers
    WHERE ismap~logsys      = iv_logsys
      AND ismap~oltpsource  = iv_oltpsr
      AND ismap~objvers     = 'A'
      AND ismap~istype     IN ('M', 'T').
    IF sy-subrc = 0.
      ws_target = get_details( iv_target = ws_target-target ).
      APPEND ws_target TO et_target.
      CLEAR ws_target.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_details
*---------------------------------------------------------------------*
  METHOD get_details.
  DATA: w_txtlg TYPE rstxtmd.

    rs_target-target = iv_target.
    IF ( lcl_bw_metadata=>is_cube( iv_target ) = 'X' ).
      rs_target-icon = icon_biw_info_cube.
      SELECT SINGLE txtlg
      INTO w_txtlg
      FROM rsdcubet
      WHERE langu    = 'E'
        AND infocube = iv_target
        AND objvers  = 'A'.
      IF sy-subrc = 0.
        rs_target-txtlg = w_txtlg.
      ELSE.
        SELECT txtlg
        INTO w_txtlg
        FROM rsdcubet UP TO 1 ROWS
        WHERE infocube = iv_target
          AND objvers  = 'A'.
        ENDSELECT.
        IF sy-subrc = 0.
          rs_target-txtlg = w_txtlg.
        ENDIF.
      ENDIF.
    ELSEIF ( lcl_bw_metadata=>is_ods( iv_target ) = 'X' ).
      rs_target-icon = icon_database_table.
      SELECT SINGLE txtlg
      INTO w_txtlg
      FROM rsdodsot
      WHERE langu    = 'E'
        AND odsobject = iv_target
        AND objvers  = 'A'.
      IF sy-subrc = 0.
        rs_target-txtlg = w_txtlg.
      ELSE.
        SELECT txtlg
        INTO w_txtlg
        FROM rsdodsot UP TO 1 ROWS
        WHERE odsobject = iv_target
          AND objvers  = 'A'.
        ENDSELECT.
        IF sy-subrc = 0.
          rs_target-txtlg = w_txtlg.
          ENDIF.
      ENDIF.
    ELSEIF ( lcl_bw_metadata=>is_infoobject( iv_target ) = 'X' ).
      rs_target-icon = icon_biw_info_object.
      SELECT SINGLE txtlg
      INTO w_txtlg
      FROM rsdiobjt
      WHERE langu    = 'E'
        AND iobjnm = iv_target
        AND objvers  = 'A'.
      IF sy-subrc = 0.
        rs_target-txtlg = w_txtlg.
      ELSE.
        SELECT txtlg
        INTO w_txtlg
        FROM rsdiobjt UP TO 1 ROWS
        WHERE iobjnm = iv_target
          AND objvers  = 'A'.
        ENDSELECT.
        IF sy-subrc = 0.
          rs_target-txtlg = w_txtlg.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_rnr IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_rnr IMPLEMENTATION.
*---------------------------------------------------------------------*
* METHOD check_if_exists
*---------------------------------------------------------------------*
  METHOD check_if_exists.
  DATA: w_odsname_tech TYPE rsa_odsdbname,
        w_odsname      TYPE rsa_odsname.
  DATA: w_text TYPE string.
  DATA: w_rnr TYPE rsrequid.
  DATA: w_records TYPE i.

    CALL FUNCTION 'RSAR_ODS_NAME_GET'
         EXPORTING
              i_logsys     = iv_logsys
              i_isource    = iv_oltpsr
         IMPORTING
              e_odsname    = w_odsname
              e_odsname_db = w_odsname_tech.

*     SELECT SINGLE rnr
*     INTO w_rnr
*     FROM rsreqdone
*     WHERE odsname = w_odsname
*       AND rnr     = iv_rnr.
*     IF sy-subrc <> 0.
*       CALL FUNCTION 'CONVERSION_EXIT_RQUID_OUTPUT'
*            EXPORTING
*                 input  = iv_rnr
*            IMPORTING
*                 output = w_rnr.
*       CONCATENATE 'Request' w_rnr 'was not loaded from DataSource'
*         p_oltpsr 'and Source System' iv_logsys
*         INTO w_text SEPARATED BY SPACE.
*       RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text
*.
*    ENDIF.

     CALL FUNCTION 'CONVERSION_EXIT_RQUID_OUTPUT'
          EXPORTING
               input  = iv_rnr
          IMPORTING
               output = w_rnr.

    SELECT SINGLE mon_stat~records
    INTO w_records
    FROM rsreqicods AS req_upd INNER JOIN rsreqdone AS mon_stat
                                       ON req_upd~rnr = mon_stat~rnr
    WHERE req_upd~rnr     = iv_rnr
      AND req_upd~tabname = w_odsname
      AND req_upd~typ     = 'O'.
    CASE sy-subrc.
    WHEN 0.
      IF w_records = 0.
        CONCATENATE 'Request' w_rnr 'is empty. No PSA data to simulate'
          INTO w_text SEPARATED BY SPACE.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      ENDIF.
    WHEN OTHERS.
      SELECT SINGLE records
      INTO w_records
      FROM rsreqdone
      WHERE rnr     = iv_rnr
        AND odsname = w_odsname.
      CASE sy-subrc.
      WHEN 0.
        CONCATENATE 'Request' w_rnr 'is not in PSA'
          INTO w_text SEPARATED BY SPACE.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      WHEN OTHERS.
        CONCATENATE 'Request' w_rnr 'was not loaded from DataSource'
           p_oltpsr 'and Source System' iv_logsys
        INTO w_text SEPARATED BY SPACE.
        RAISE EXCEPTION TYPE lcl_local_exception
           EXPORTING TEXT = w_text.
      ENDCASE.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_list
*---------------------------------------------------------------------*
  METHOD get_list.
  DATA: ws_rnr TYPE s_rnr.
  DATA: ws_rnr_4sort TYPE s_rnr_4sort,
        wt_rnr_4sort TYPE t_rnr_4sort.
  DATA: w_odsname_tech TYPE rsa_odsdbname,
        w_odsname      TYPE rsa_odsname.
  DATA: w_logdpid TYPE rslogdpid.

    CALL FUNCTION 'RSAR_ODS_NAME_GET'
         EXPORTING
              i_logsys     = iv_logsys
              i_isource    = iv_oltpsr
         IMPORTING
              e_odsname    = w_odsname
              e_odsname_db = w_odsname_tech.

    SELECT mon_stat~rnr mon_stat~records
           mon_stat~tstatus AS monitor_status
    INTO CORRESPONDING FIELDS OF TABLE wt_rnr_4sort
    FROM rsreqicods AS req_upd INNER JOIN rsreqdone AS mon_stat
                                       ON req_upd~rnr = mon_stat~rnr
    WHERE req_upd~tabname = w_odsname
      AND req_upd~typ     = 'O'.

    LOOP AT wt_rnr_4sort INTO ws_rnr_4sort.
      CALL FUNCTION 'CONVERSION_EXIT_RQUID_OUTPUT'
           EXPORTING
                input  = ws_rnr_4sort-rnr
           IMPORTING
                output = ws_rnr_4sort-rnr_ext_4sort.
      ws_rnr_4sort-rnr_ext = ws_rnr_4sort-rnr_ext_4sort.
      SHIFT ws_rnr_4sort-rnr_ext LEFT DELETING LEADING '0'.
      SELECT SINGLE datum logdpid
      INTO (ws_rnr_4sort-datum, w_logdpid)
      FROM rsreqdone
      WHERE rnr = ws_rnr_4sort-rnr.

      IF ( lcl_bw_metadata=>is_cube( iv_cube = iv_target ) = 'X' ) OR
         ( lcl_bw_metadata=>is_ods( iv_ods = iv_target )  = 'X' ).
        CASE ws_rnr_4sort-records.
        WHEN 0.
          ws_rnr_4sort-request_updated = icon_failure.
        WHEN OTHERS.
          SELECT SINGLE rnr
          INTO ws_rnr_4sort-rnr
          FROM rsmonicdp
          WHERE rnr   = ws_rnr_4sort-rnr
            AND icube = iv_target
            AND dp_nr = '999999'.
          IF sy-subrc = 0.
            ws_rnr_4sort-request_updated = icon_action_success.
          ELSE.
            ws_rnr_4sort-request_updated = icon_action_fault.
          ENDIF.
        ENDCASE.
      ENDIF.

      SELECT text
      INTO ws_rnr_4sort-text
      FROM rsldpiot UP TO 1 ROWS
      WHERE logdpid = w_logdpid
        AND objvers = 'A'.
      ENDSELECT.
      CASE ws_rnr_4sort-monitor_status.
      WHEN '@0A\'.
        ws_rnr_4sort-monitor_status = icon_red_light.
      WHEN '@09\'.
        ws_rnr_4sort-monitor_status = icon_yellow_light.
      WHEN '@08\'.
        ws_rnr_4sort-monitor_status = icon_green_light.
      ENDCASE.

      MODIFY wt_rnr_4sort FROM ws_rnr_4sort
        TRANSPORTING request_updated datum monitor_status
                     rnr_ext         text  rnr_ext_4sort.
    ENDLOOP.

    SORT wt_rnr_4sort BY rnr_ext_4sort DESCENDING.
    LOOP AT wt_rnr_4sort INTO ws_rnr_4sort.
      MOVE-CORRESPONDING ws_rnr_4sort TO ws_rnr.
      APPEND ws_rnr TO et_rnr.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* CLASS lcl_packid IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_packid IMPLEMENTATION.
*---------------------------------------------------------------------*
* METHOD check_if_exists
*---------------------------------------------------------------------*
  METHOD check_if_exists.
  DATA: w_dp_nr TYPE rsdatapid.
  DATA: w_text TYPE string.
  DATA: w_rnr TYPE rsrequid.

    IF iv_packid = '000000'.
      w_text = 'Data Packet Number is not specified'.
       RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.


    SELECT dp_nr
    INTO w_dp_nr
    FROM rsmoniptab UP TO 1 ROWS
    WHERE rnr   = iv_rnr
      AND dp_nr = iv_packid.
    ENDSELECT.
    IF sy-subrc <> 0.
      CALL FUNCTION 'CONVERSION_EXIT_RQUID_OUTPUT'
            EXPORTING
                 input  = iv_rnr
            IMPORTING
                 output = w_rnr.
      CONCATENATE 'Request' w_rnr 'has no Data Package' iv_packid
         INTO w_text SEPARATED BY SPACE.
       RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* METHOD get_list
*---------------------------------------------------------------------*
  METHOD get_list.
  DATA: w_rqdrecord TYPE rsrqrecord.
  DATA: ws_packid TYPE s_packid.

    SELECT dp_nr rqdrecord
    INTO (ws_packid-packid, w_rqdrecord)
    FROM rsmoniptab
    WHERE rnr = iv_rnr
      AND dp_nr <> 0.
      ws_packid-records = w_rqdrecord.
      APPEND ws_packid TO et_packid.
    ENDSELECT.

  ENDMETHOD.

ENDCLASS.


*---------------------------------------------------------------------*
* CLASS lcl_psa IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_psa IMPLEMENTATION.
*---------------------------------------------------------------------*
* METHOD check_if_data_exists
*---------------------------------------------------------------------*
  METHOD check_if_data_exists.
  DATA: w_odsname_tech TYPE rsodstech.
  DATA: w_text TYPE string.


    CALL FUNCTION 'RSAR_ODS_NAME_GET'
         EXPORTING
              i_logsys     = iv_logsys
              i_isource    = iv_oltpsr
         IMPORTING
              e_odsname_db = w_odsname_tech.

    SELECT request
    INTO w_rnr
    FROM (w_odsname_tech) UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc <> 0.
      CONCATENATE 'No PSA data for simultaion of DataSource ' iv_oltpsr
                  'and Source System' iv_logsys
                  INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  get_rnr_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_RNR  text
*      <--P_SELECTION  text
*----------------------------------------------------------------------*
FORM get_rnr_selection  USING    pr_rnr      LIKE s_rnr[]
                        CHANGING p_selection TYPE string.
DATA: wa_rnr TYPE rsrange.

  IF p_selection = SPACE.
    p_selection = 'REQUEST IN ('.
  ELSE.
    CONCATENATE p_selection
                'AND'
                'REQUEST IN ('
                INTO p_selection
                SEPARATED BY SPACE.
  ENDIF.
  LOOP AT pr_rnr INTO wa_rnr.
    IF sy-tabix <> 1.
      CONCATENATE p_selection ', ' INTO p_selection.
    ENDIF.
    CONCATENATE p_selection
                ''''
                wa_rnr-low
                ''''
                INTO p_selection.
  ENDLOOP.
  CONCATENATE p_selection ')'
              INTO p_selection.

ENDFORM.                    " get_rnr_selection

*&---------------------------------------------------------------------*
*&      Form  display_update_rules
*&---------------------------------------------------------------------*
FORM display_update_rules .
DATA: w_logsys   TYPE logsys,
      w_oltpsr   TYPE roosourcer,
      w_target   TYPE rsinfocube,
      w_target$t TYPE rsinfocube,
      w_isource  TYPE rsisource.
DATA: w_updid TYPE rsupdid.

  PERFORM get_selection_field_value: USING 'P_LOGSYS'
                                     CHANGING w_logsys,
                                     USING 'P_OLTPSR'
                                     CHANGING w_oltpsr,
                                     USING 'P_TARGET'
                                     CHANGING w_target.

  IF lcl_target=>is_direct_update( iv_logsys = w_logsys
                                   iv_oltpsr = w_oltpsr
                                   iv_target = w_target ) = 'X'.
    MESSAGE i000(oo) WITH 'InfoObject' w_target 'is updated directly.'
                          'Update Rules display is not possible'.
    EXIT.
  ELSE.
    CALL METHOD lcl_oltpsr=>get_infosource
      EXPORTING iv_logsys  = w_logsys
                iv_oltpsr  = w_oltpsr
      CHANGING  cv_isource = w_isource.
    CALL FUNCTION 'RSAU_UPDR_CALL_TRANSACTION'
         EXPORTING
              i_dta     = w_target
              i_isource = w_isource
              i_mode    = '3'
              i_objvers = 'A'.
  ENDIF.


ENDFORM.                    " display_update_rules

*&---------------------------------------------------------------------*
*&      Form  display_transfer_rules
*&---------------------------------------------------------------------*
FORM display_transfer_rules.
DATA: w_logsys   TYPE logsys,
      w_oltpsr   TYPE roosourcer.
DATA: w_isource  TYPE rsisource,
      w_transtru TYPE rstranstru,
      w_isostype TYPE rsisostype,
      w_istype   TYPE rsrequtype.

  PERFORM get_selection_field_value: USING 'P_LOGSYS'
                                     CHANGING w_logsys,
                                     USING 'P_OLTPSR'
                                     CHANGING w_oltpsr.

  CALL METHOD lcl_oltpsr=>get_infosource
       EXPORTING iv_logsys   = w_logsys
                 iv_oltpsr   = w_oltpsr
       CHANGING  cv_isource  = w_isource
                 cv_transtru = w_transtru
                 cv_isostype = w_isostype
                 cv_istype   = w_istype.
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
       EXPORTING
            tcode = 'RSU3I'
       EXCEPTIONS
            ok     = 1
            not_ok = 2.
  CASE sy-subrc.
  WHEN 1.
    CALL FUNCTION 'RSAR_INFOSOURCE_MAINTAIN'
         EXPORTING
              i_isource      = w_isource
              i_isostype     = w_isostype
              i_logsys       = w_logsys
              i_oltpsource   = w_oltpsr
              i_istype       = w_istype
              i_modus        = '3'.
  WHEN 2.
    MESSAGE i000(oo)
      WITH 'You are not authorized to use transaction RSU3I'.
  ENDCASE.

ENDFORM.                    " display_update_rules

*&---------------------------------------------------------------------*
*&      Form  swap_staging_type
*&---------------------------------------------------------------------*
FORM swap_staging_type.

   IF p_bi = 'X'.
     SUBMIT z_simulation_workbench_bi VIA SELECTION-SCREEN.
   ENDIF.

ENDFORM.                    " swap_staging_type