REPORT z_simulation_workbench_bi MESSAGE-ID rsbk.

************************************************************************
* Data Declaration                                                     *
************************************************************************
CLASS lcl_event_receiver DEFINITION DEFERRED.
TYPE-POOLS icon.
TYPE-POOLS: rs.
TYPES: t_requid TYPE RANGE OF rsbkrequid,
       BEGIN OF s_bw_obj,
         bw_obj   TYPE rsawbnobjnm,
         text     TYPE rstxtlg,
         id       TYPE icon_d,
       END OF s_bw_obj,
       BEGIN OF s_target,
         tgt      TYPE rsbktgtnm,
         tgttlogo TYPE rsbktgttlogo,
         text     TYPE rstxtlg,
         id       TYPE icon_d,
       END OF s_target,
       t_target TYPE TABLE OF s_target WITH DEFAULT KEY,
       BEGIN OF s_dtp,
         name  TYPE rsbkdtpnm,
         text  TYPE rstxtlg,
         updmode      TYPE rsbkupdmode,
         updmode_text TYPE rstxtlg,
         dtptype      TYPE rsbkdtptype,
         dtptype_text TYPE rstxtlg,
         id           TYPE icon_d,
         filter_text  TYPE char100,
       END OF s_dtp,
       t_dtp TYPE TABLE OF s_dtp WITH DEFAULT KEY,
       BEGIN OF s_request,
         rnr             TYPE rssid,
         request_status  TYPE rsstatus,
         datamart_status TYPE rsstatus,
         date            TYPE dats,
         time            TYPE tims,
         text            TYPE rsbook25,
         rcrdextr        TYPE rsrcrdextr,
       END OF s_request,
       t_request TYPE TABLE OF s_request WITH DEFAULT KEY,
       BEGIN OF s_infopackage,
         name TYPE rslogdpid,
         text TYPE rstxtlg,
       END OF s_infopackage.
DATA: g_repid LIKE sy-repid,
      g_dynnr TYPE sydynnr.
DATA: w_rnr TYPE rssid.
DATA: ws_functxt TYPE smp_dyntxt.
TABLES sscrfields.
DATA: gs_layout_x TYPE lvc_s_layo,
      gs_layout_t TYPE lvc_s_layo.
DATA: gt_fcat_x        TYPE lvc_t_fcat,
      gt_fcat_t        TYPE lvc_t_fcat,
      gt_fcat_t_expand TYPE lvc_t_fcat,
      gt_fcat_t_collps TYPE lvc_t_fcat.
DATA: main_container    TYPE REF TO cl_gui_custom_container,
      splitter          TYPE REF TO cl_gui_splitter_container,
      container_top     TYPE REF TO cl_gui_container,
      container_bottom  TYPE REF TO cl_gui_container,
      alv_grid_top      TYPE REF TO cl_gui_alv_grid,
      alv_grid_bottom   TYPE REF TO cl_gui_alv_grid,
      event_receiver    TYPE REF TO lcl_event_receiver.
FIELD-SYMBOLS: <buffer_t_x>        TYPE ANY TABLE,
               <buffer_t_t>        TYPE ANY TABLE,
               <buffer_t_t_collps> TYPE ANY TABLE,
               <buffer_t_t_expand> TYPE ANY TABLE.


************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN: FUNCTION KEY 1,
                  FUNCTION KEY 2.

SELECTION-SCREEN BEGIN OF BLOCK data_staging WITH FRAME TITLE slctxt17.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt18 FOR FIELD p_bi.
PARAMETERS: p_bi RADIOBUTTON GROUP stgt DEFAULT 'X'
                 USER-COMMAND SEL_STAGING_TYPE.
SELECTION-SCREEN COMMENT 26(27) c_bitran VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 43(27) c_bi.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt19 FOR FIELD p_bw_3x.
PARAMETERS: p_bw_3x RADIOBUTTON GROUP stgt.
SELECTION-SCREEN COMMENT 26(4) c_bwismp VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 29(4) c_sqr_1 VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 32(4) c_arrow VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 36(4) c_bwupdr VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 39(4) c_sqr_2 VISIBLE LENGTH 1.
SELECTION-SCREEN COMMENT 43(27) c_bw_3x.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK data_staging.

SELECTION-SCREEN BEGIN OF BLOCK dtp WITH FRAME TITLE slctxt5.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(21) slctxt1 FOR FIELD p_target.
PARAMETERS: p_target TYPE rsawbnobjnm.
SELECTION-SCREEN COMMENT 65(4) c_target FOR FIELD p_target.
SELECTION-SCREEN POSITION 69.
PARAMETERS: t_target TYPE rstxtlg MODIF ID txt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(21) slctxt2 FOR FIELD p_source.
PARAMETERS: p_source TYPE rsawbnobjnm.
SELECTION-SCREEN COMMENT 65(4) c_source FOR FIELD p_source.
SELECTION-SCREEN POSITION 69.
PARAMETERS: t_source TYPE rstxtlg MODIF ID txt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(21) slctxt3 FOR FIELD p_dtp.
PARAMETERS: p_dtp TYPE rsbkdtpnm.
SELECTION-SCREEN COMMENT 65(4) c_dtp FOR FIELD p_dtp.
SELECTION-SCREEN POSITION 69.
PARAMETERS: t_dtp TYPE rstxtlg MODIF ID txt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(18) slctxt4 FOR FIELD s_requid.
SELECT-OPTIONS: s_requid FOR w_rnr NO INTERVALS.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 6(26) slctxt21 MODIF ID op.
SELECTION-SCREEN POSITION 3.
PARAMETERS: p_sel_op AS CHECKBOX DEFAULT SPACE MODIF ID op.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK dtp.

SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE slctxt8.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt9 FOR FIELD p_mode.
PARAMETERS: p_mode AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK mode.

SELECTION-SCREEN BEGIN OF BLOCK buffer WITH FRAME TITLE slctxt6.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt7 FOR FIELD p_bffr_t.
PARAMETERS: p_bffr_t AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt10 FOR FIELD p_bffr_x.
PARAMETERS: p_bffr_x AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK buffer.

SELECTION-SCREEN BEGIN OF BLOCK break WITH FRAME TITLE slctxt15.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt16 FOR FIELD p_bp_t.
PARAMETERS: p_bp_t AS CHECKBOX DEFAULT SPACE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK break.

SELECTION-SCREEN BEGIN OF BLOCK output_frmt WITH FRAME TITLE slctxt11.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt12 FOR FIELD p_outsap.
PARAMETERS: p_outsap RADIOBUTTON GROUP outf
                     USER-COMMAND SEL_OUTPUT.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(22) slctxt13 FOR FIELD p_outcst.
PARAMETERS: p_outcst RADIOBUTTON GROUP outf DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 6(37) slctxt14 MODIF ID cst.
SELECTION-SCREEN POSITION 3.
PARAMETERS: p_cmpout AS CHECKBOX DEFAULT 'X' MODIF ID cst.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 6(23) slctxt20 MODIF ID cst.
SELECTION-SCREEN POSITION 3.
PARAMETERS: p_dscr_h AS CHECKBOX DEFAULT 'X' MODIF ID cst.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK output_frmt.


************************************************************************
INITIALIZATION.
************************************************************************
ws_functxt-icon_id    = icon_biw_rules_act.
ws_functxt-quickinfo  = 'Display Transformation'.
ws_functxt-icon_text  = 'Transformation'.
sscrfields-functxt_01 = ws_functxt.
ws_functxt-icon_id    = icon_target_group.
ws_functxt-quickinfo  = 'Target Management'.
ws_functxt-icon_text  = 'Target'.
sscrfields-functxt_02 = ws_functxt.

sy-title = 'Simulation Workbench: BI'.
g_repid  = sy-repid.
g_dynnr  = sy-dynnr.
slctxt1  = 'Target'.
slctxt2  = 'Source'.
slctxt3  = 'DTP'.
slctxt4  = 'Request'.
slctxt5  = 'Transformation and Request(s) selection'.
slctxt6  = 'Temporary Storage'.
slctxt7  = 'After Extraction'.
slctxt8  = 'Mode'.
slctxt9  = 'Expert'.
slctxt10 = 'After Transformation'.
slctxt11 = 'Output format'.
slctxt12 = 'SAP Standard'.
slctxt13 = 'Customized'.
slctxt14 = 'Collapse Package After Transformation'.
slctxt15 = 'Breakpoints'.
slctxt16 = 'Before Transformation'.
slctxt17 = 'Data Staging Type'.
slctxt18 = 'BI'.
slctxt19 = 'BW 3.x'.
slctxt20 = 'Use Descriptive Headers'.
slctxt21 = 'Optimize Request Selection'.
c_bitran = icon_biw_rules_act.
c_bwismp = icon_transfer_structure.
c_bwupdr = icon_biw_rules_act.
c_sqr_1 = c_sqr_2 = icon_parameter.
c_arrow = icon_draw_arrow.
c_bi = 'Transformation'.
c_bw_3x = 'Transfer Rule / Update Rule'.


*---------------------------------------------------------------------*
*       INTERFACE lif_request
*---------------------------------------------------------------------*
INTERFACE lif_request.
  METHODS:
    get_list         EXPORTING et_request TYPE t_request.
ENDINTERFACE.

*---------------------------------------------------------------------*
*       INTERFACE lif_psa
*---------------------------------------------------------------------*
INTERFACE lif_psa.
  METHODS:
    get_psa_tech_name RETURNING VALUE(rv_psa_tech_name) TYPE rsodstech.
ENDINTERFACE.

*---------------------------------------------------------------------*
*       CLASS lcl_local_exception  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_local_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA:
      local_text TYPE string.
    METHODS:
      constructor IMPORTING text TYPE string.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_application  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_dtp DEFINITION DEFERRED.
CLASS lcl_application DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA:
      application          TYPE REF TO lcl_application,
      descriptive_headers_t TYPE c,
      descriptive_headers_x TYPE c.
    CLASS-METHODS:
      main,
      get_field_value      IMPORTING iv_fieldname   TYPE dynfnam
                           EXPORTING ev_fieldvalue  TYPE ANY,
      swap_staging_type,
      check_target,
      check_source,
      check_dtp,
      check_request,
      check_select_optim,
      convert_request,
      f4_target,
      f4_source,
      f4_dtp,
      f4_request,
      display_transform,
      manage_target,
      update_sel_screen,
      get_fcat             IMPORTING it_field      TYPE rsbk_th_field_rt
                                     iv_cmdtype    TYPE rsbcmdtype
                                     iv_cmp_output TYPE char1
                                     iv_srctp      TYPE rsbksrctp
                           RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat,
      swap_fcat_technm_n_descr
                           CHANGING  ct_fcat       TYPE lvc_t_fcat,
      get_layout           IMPORTING iv_title       TYPE lvc_title
                           RETURNING VALUE(rs_layout) TYPE lvc_s_layo.
    METHODS:
      constructor          IMPORTING iv_source     TYPE rsawbnobjnm
                                     iv_target     TYPE rsawbnobjnm
                                     iv_dtp        TYPE rsbkdtpnm
                                     iv_buffer_t   TYPE char1
                                     iv_buffer_x   TYPE char1
                                     iv_bp_t       TYPE char1
                                     iv_std_output TYPE char1
                                     iv_sel_optim  TYPE char1
                                     it_requid     TYPE t_requid
                           RAISING   lcl_local_exception,
      doit.
    DATA:
      _dtp                 TYPE REF TO lcl_dtp.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_toolbar_x     FOR EVENT toolbar OF cl_gui_alv_grid
                           IMPORTING e_object
                                     e_interactive,
      handle_user_command_x FOR EVENT user_command OF cl_gui_alv_grid
                           IMPORTING e_ucomm,
      handle_toolbar_t     FOR EVENT toolbar OF cl_gui_alv_grid
                           IMPORTING e_object
                                     e_interactive,
      handle_user_command_t FOR EVENT user_command OF cl_gui_alv_grid
                           IMPORTING e_ucomm.
  PRIVATE SECTION.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_dtp  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_dtp DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor          IMPORTING iv_dtp        TYPE rsbkdtpnm
                                     iv_buffer_t   TYPE char1
                                     iv_buffer_x   TYPE char1
                                     iv_bp_t       TYPE char1
                                     iv_std_output TYPE char1
                                     iv_sel_optim  TYPE char1
                                     iv_source     TYPE rsawbnobjnm
                                     iv_psa_tech_name
                                                   TYPE rsodstech
                                     it_requid     TYPE t_requid
                           RAISING   lcl_local_exception,
      simulate.
    CLASS-METHODS:
      check_authorization  IMPORTING iv_dtp        TYPE rsbkdtpnm
                           RAISING   lcl_local_exception,
      bw_obj_get_details   IMPORTING iv_bw_obj     TYPE rsawbnobjnm
                           EXPORTING es_bw_obj     TYPE s_bw_obj,
      target_get_list      EXPORTING et_target     TYPE t_target,
      if_target_exists     IMPORTING iv_target     TYPE rsawbnobjnm
                           RAISING   lcl_local_exception,
      select_source        IMPORTING iv_target     TYPE rsawbnobjnm
                           EXPORTING ev_source     TYPE rsawbnobjnm
                                     ev_source_type TYPE icon_d,
      if_source_exists     IMPORTING iv_source     TYPE rsawbnobjnm
                           RAISING   lcl_local_exception,
      dtp_get_details      IMPORTING iv_dtp        TYPE rsbkdtpnm
                           EXPORTING es_dtp        TYPE s_dtp,
      dtp_get_list         IMPORTING iv_source     TYPE rsawbnobjnm
                                     iv_target     TYPE rsawbnobjnm
                           EXPORTING et_dtp        TYPE t_dtp
                           RAISING   lcl_local_exception,
      get_transformation   IMPORTING iv_dtp        TYPE rsbkdtpnm
                           RETURNING VALUE(rv_tranid) TYPE rstranid,
      if_exists            IMPORTING iv_dtp        TYPE rsbkdtpnm
                                     iv_source     TYPE rsawbnobjnm
                                     OPTIONAL
                                     iv_target     TYPE rsawbnobjnm
                                     OPTIONAL
                           RETURNING VALUE(rv_exists) TYPE char01,
      get_filter_text      IMPORTING iv_dtp        TYPE rsbkdtpnm
                           RETURNING VALUE(rv_text) TYPE char100
                           RAISING   lcl_local_exception.
  PRIVATE SECTION.
    METHODS:
      get_genuid_debug     RETURNING VALUE(rv_genuid_debug)
                                                   TYPE rssguid25,
      check_active_request IMPORTING iv_requid     TYPE rsbkrequid
                           RAISING   cx_rs_failed
                                     cx_rs_foreign_lock,
      display_request,
      display_request_cust RAISING   lcl_local_exception,
      get_cmd_node_id      IMPORTING iv_cmdtype    TYPE rsbcmdtype
                           EXPORTING ev_step_from  TYPE rsbcmdnode
                                     ev_step_to    TYPE rsbcmdnode,
      read_buffer          IMPORTING iv_cmdtype    TYPE rsbcmdtype
                                     iv_cmp_output TYPE char1
                           CHANGING  cr_data       TYPE REF TO DATA
                                     cr_data_collps TYPE REF TO DATA
                                                   OPTIONAL
                                     cr_data_expand TYPE REF TO DATA
                                                   OPTIONAL
                                     ct_field     TYPE rsbk_th_field_rt,
      upd_fld_text         CHANGING  ct_field     TYPE rsbk_th_field_rt.
    DATA:
      _r_dtp               TYPE REF TO cl_rsbk_dtp,
      _r_request           TYPE REF TO cl_rsbk_request,
      _buffer_x            TYPE char1,
      _buffer_t            TYPE char1,
      _bp_t                TYPE char1,
      _std_output          TYPE char1.
    DATA:
      g_r_viewer           TYPE REF TO cl_rsbk_dtp_206_b1,
      g_r_custom_container TYPE REF TO cl_gui_custom_container.
    CONSTANTS:
      c_cmdtype_x          TYPE rsbcmdtype VALUE 'X',
      c_cmdtype_t          TYPE rsbcmdtype VALUE 'T'.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_datastore  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_datastore DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_request,
      lif_psa.
    METHODS:
      constructor          IMPORTING iv_datastore  TYPE rsawbnobjnm
                                     iv_target     TYPE rsawbnobjnm.
    CLASS-METHODS:
      if_exists            IMPORTING iv_datastore  TYPE rsdodsobject
                           RETURNING VALUE(rv_exists) TYPE char01,
      if_requid_in         IMPORTING iv_requid     TYPE rsbkrequid
                                     OPTIONAL
                                     iv_odsrnrsid  TYPE numc10
                                     OPTIONAL
                                     iv_datastore  TYPE rsawbnobjnm
                           RETURNING VALUE(rv_requid_in) TYPE char01.
  PRIVATE SECTION.
    DATA:
      _datasource          TYPE rsawbnobjnm,
      _datastore           TYPE rsstatmandta,
      _target              TYPE rsawbnobjnm.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_infocube  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_infocube DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_request.
    METHODS:
      constructor          IMPORTING iv_infocube   TYPE rsawbnobjnm
                                     iv_target     TYPE rsawbnobjnm.
    CLASS-METHODS:
      if_exists            IMPORTING iv_infocube   TYPE rsinfocube
                           RETURNING VALUE(rv_exists) TYPE char01,
      if_requid_in         IMPORTING iv_requid     TYPE rsbkrequid
                                     iv_infocube   TYPE rsawbnobjnm
                           RETURNING VALUE(rv_requid_in) TYPE char01.
  PRIVATE SECTION.
    DATA:
      _infocube            TYPE rsinfocube,
      _target              TYPE rsawbnobjnm.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_infoobject  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_infoobject DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor          IMPORTING iv_infoobject TYPE rsdiobjnm.
    DATA:
      attr_exists          TYPE char01 READ-ONLY,
      text_exists          TYPE char01 READ-ONLY.
    CLASS-METHODS:
      if_exists            IMPORTING iv_infoobject TYPE rsdiobjnm
                           RETURNING VALUE(rv_exists) TYPE char01,
      if_requid_in         IMPORTING iv_requid     TYPE rsbkrequid
                                     iv_infoobject TYPE rsawbnobjnm
                           RETURNING VALUE(rv_requid_in) TYPE char01.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_datasource  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_datasource DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_request,
      lif_psa.
    METHODS:
      constructor          IMPORTING iv_datasource TYPE rsawbnobjnm
                                     iv_target     TYPE rsawbnobjnm.
    CLASS-METHODS:
      get_psa              IMPORTING iv_datasource TYPE rsawbnobjnm
                           RETURNING VALUE(rv_psa) TYPE rsodsname,
      if_exists            IMPORTING iv_datasource TYPE rsawbnobjnm
                           RETURNING VALUE(rv_exists) TYPE char01,
      if_requid_in         IMPORTING iv_requid     TYPE rsbkrequid
                                     iv_datasource TYPE rsawbnobjnm
                           RETURNING VALUE(rv_requid_in) TYPE char01.
  PRIVATE SECTION.
    DATA:
      _datasource          TYPE rsawbnobjnm,
      _psa                 TYPE rsodsname,
      _target              TYPE rsawbnobjnm.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_request  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_request DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      if_requid_in         IMPORTING iv_requid     TYPE rsbkrequid
                                                   OPTIONAL
                                     iv_odsrnrsid  TYPE numc10
                                                   OPTIONAL
                                     iv_datasource TYPE rsawbnobjnm
                                                   OPTIONAL
                                     iv_datastore  TYPE rsawbnobjnm
                                                   OPTIONAL
                                     iv_infocube   TYPE rsawbnobjnm
                                                   OPTIONAL
                                     iv_infoobject TYPE rsawbnobjnm
                                                   OPTIONAL
                           RETURNING VALUE(rv_requid_in) TYPE char01,
      convert_2_src_requid IMPORTING  iv_requid    TYPE rsbkrequid
                           RETURNING VALUE(rt_requid) TYPE t_requid,
      convert_2_requid     IMPORTING iv_odsrnrsid  TYPE numc10
                                     iv_datastore  TYPE rsawbnobjnm
                           RETURNING VALUE(rv_requid) TYPE rsbkrequid,
      convert_2_odsrnrsid  IMPORTING iv_requid     TYPE rsbkrequid
                                     iv_datastore  TYPE rsawbnobjnm
                           RETURNING VALUE(rv_odsrnrsid) TYPE numc10,
      optimize_selection   IMPORTING iv_psa_tech_name
                                                    TYPE rsodstech
                                     iv_limit_datapakid
                                                    TYPE char1
                                     it_range       TYPE rsbk_th_range
                           RETURNING VALUE(rt_range)
                                                    TYPE rsbk_th_range,
      build_where          IMPORTING it_range       TYPE rsbk_th_range
                                     iv_psa_tech_name
                                                    TYPE rsodstech
                           RETURNING VALUE(rt_where)
                                                    TYPE rsdmd_t_where.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_infopackage  DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_infopackage DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      if_exists            IMPORTING iv_infopackage TYPE rslogdpid
                           RETURNING VALUE(rv_exists) TYPE char01,
      get_details          IMPORTING iv_infopackage TYPE rslogdpid
                           EXPORTING ev_infopackage TYPE s_infopackage.
ENDCLASS.

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
*       CLASS lcl_application  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
*---------------------------------------------------------------------*
* main
*---------------------------------------------------------------------*
  METHOD main.
  DATA: local_exception TYPE REF TO lcl_local_exception.

    descriptive_headers_t = descriptive_headers_x = p_dscr_h.
*   Create instance
    TRY.
      CREATE OBJECT application  EXPORTING iv_source     = p_source
                                           iv_target     = p_target
                                           iv_dtp        = p_dtp
                                           iv_buffer_t   = p_bffr_t
                                           iv_buffer_x   = p_bffr_x
                                           iv_bp_t       = p_bp_t
                                           iv_std_output = p_outsap
                                           iv_sel_optim  = p_sel_op
                                           it_requid     = s_requid[].
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'I'.
        EXIT.
    ENDTRY.
    CALL METHOD application->doit.

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_field_value
*---------------------------------------------------------------------*
  METHOD get_field_value.
  TYPES: t_dynread TYPE TABLE OF dynpread.
  DATA: wa_dynfields TYPE dynpread,
        wt_dynfields TYPE t_dynread.
  DATA: w_d020s TYPE d020s.

    w_d020s-prog = g_repid.
    w_d020s-dnum = g_dynnr.

    wa_dynfields-fieldname = iv_fieldname.
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
       TRANSLATE wa_dynfields-fieldvalue TO UPPER CASE.
       ev_fieldvalue = wa_dynfields-fieldvalue.
     ENDIF.

   ENDMETHOD.

*---------------------------------------------------------------------*
* swap_staging_type
*---------------------------------------------------------------------*
  METHOD swap_staging_type.

   IF p_bw_3x = 'X'.
     SUBMIT z_simulation_workbench_bw_3x VIA SELECTION-SCREEN.
   ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* check_target
*---------------------------------------------------------------------*
  METHOD check_target.
  DATA: local_exception TYPE REF TO lcl_local_exception.

    TRY.
      lcl_dtp=>if_target_exists( p_target ).
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'E'.
        EXIT.
    ENDTRY.

  ENDMETHOD.

*---------------------------------------------------------------------*
* check_source
*---------------------------------------------------------------------*
  METHOD check_source.
  DATA: local_exception TYPE REF TO lcl_local_exception.

    TRY.
      lcl_dtp=>if_source_exists( p_source ).
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'E'.
        EXIT.
    ENDTRY.

  ENDMETHOD.

*---------------------------------------------------------------------*
* check_dtp
*---------------------------------------------------------------------*
  METHOD check_dtp.

    IF p_dtp = SPACE.
      MESSAGE e000(oo) WITH 'DTP is not specified'.
    ENDIF.

    IF lcl_dtp=>if_exists( iv_dtp = p_dtp ) = SPACE.
      MESSAGE e000(oo) WITH 'Active DTP' p_dtp 'does not exist'.
    ENDIF.

    IF lcl_dtp=>if_exists( iv_dtp = p_dtp
                           iv_source = p_source
                           iv_target = p_target ) = SPACE.
      MESSAGE e000(oo) WITH 'Source and/or Target of DTP' p_dtp
                            'is specified incorrectly'.
    ENDIF.


  ENDMETHOD.

*---------------------------------------------------------------------*
* check_request
*---------------------------------------------------------------------*
  METHOD check_request.
  DATA: ws_requid TYPE LINE OF t_requid.
  DATA: w_odsrnrsid TYPE numc10.

    IF NOT s_requid[] IS INITIAL.
      CASE p_source+30(10).
      WHEN 'ATTRIBUTES'.
        MESSAGE e000(oo) WITH 'No Request(s) selection possible'
                             'for InfoObject Attribures Source of data'.
      WHEN 'TEXTS'.
        MESSAGE e000(oo) WITH 'No Request(s) selection possible'
                              'for InfoObject Texts Source of data'.
      ENDCASE.
    ENDIF.

    CALL METHOD lcl_application=>get_field_value:
         EXPORTING iv_fieldname  = 'P_SEL_OP'
         IMPORTING ev_fieldvalue = p_sel_op.
*
    IF ( ( lcl_datasource=>if_exists( p_source ) = 'X' ) OR
       (   lcl_datastore=>if_exists( p_source(30) ) = 'X' ) ) AND
       ( NOT s_requid[] IS INITIAL ) AND
       ( p_sel_op = 'X' ).
      MESSAGE e000(oo) WITH '''Optimize Request Selection'''
                            'option assumes no input for Request field'.
    ENDIF.

    IF ( lcl_datasource=>if_exists( p_source ) = 'X' ).
      LOOP AT s_requid INTO ws_requid.
        IF lcl_request=>if_requid_in( iv_requid    = ws_requid-low
                                     iv_datasource = p_source ) = SPACE.
          MESSAGE e000(oo) WITH 'There is no'  ws_requid-low
                                'request in DataSource' p_source.
        ENDIF.
      ENDLOOP.
    ELSEIF ( lcl_datastore=>if_exists( p_source(30) ) = 'X' ).
      LOOP AT s_requid INTO ws_requid.
        w_odsrnrsid = ws_requid-low.
        IF lcl_request=>if_requid_in( iv_odsrnrsid = w_odsrnrsid
                                      iv_datastore = p_source ) = SPACE.
          MESSAGE e000(oo) WITH 'There is no' w_odsrnrsid
                                'request in DataStore' p_source.
        ENDIF.
      ENDLOOP.
    ELSEIF ( lcl_infocube=>if_exists( p_source(30) ) = 'X' ).
      LOOP AT s_requid INTO ws_requid.
        IF lcl_request=>if_requid_in( iv_requid   = ws_requid-low
                                      iv_infocube = p_source ) = SPACE.
          MESSAGE e000(oo) WITH 'There is no'  ws_requid-low
                                'request in DataStore' p_source.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* check_select_optim
*---------------------------------------------------------------------*
  METHOD check_select_optim.
    IF ( lcl_datasource=>if_exists( p_source ) <> 'X' ) AND
       ( lcl_datastore=>if_exists( p_source(30) ) <> 'X' ) AND
       ( p_sel_op = 'X' ).
      MESSAGE e000(oo) WITH 'Request Selection Optimization'
                            'is only supported for DataSource and'
                            'DataStore only'.
    ENDIF.
  ENDMETHOD.

*---------------------------------------------------------------------*
* convert_request
*---------------------------------------------------------------------*
  METHOD convert_request.
  DATA: ws_requid     TYPE LINE OF t_requid,
        wt_requid     TYPE t_requid,
        wt_requid_tmp TYPE t_requid.
  DATA: w_requid    TYPE rsbkrequid,
        w_odsrnrsid TYPE numc10.

    IF ( lcl_datastore=>if_exists( p_target(30) ) = 'X' ).
      LOOP AT s_requid INTO ws_requid.
        w_requid = ws_requid-low.
        IF lcl_request=>if_requid_in( iv_requid    = w_requid
                                      iv_datastore = p_target ) = 'X'.
          wt_requid_tmp =
            lcl_request=>convert_2_src_requid( w_requid ).
          APPEND LINES OF wt_requid_tmp TO wt_requid.
          CLEAR: wt_requid_tmp[].
          DELETE s_requid.
          CONTINUE.
        ENDIF.
*
        w_odsrnrsid = ws_requid-low.
        IF lcl_request=>if_requid_in( iv_odsrnrsid = w_odsrnrsid
                                      iv_datastore = p_target ) = 'X'.
          w_requid = lcl_request=>convert_2_requid(
                                        iv_odsrnrsid = w_odsrnrsid
                                        iv_datastore = p_target ).
          wt_requid_tmp =
            lcl_request=>convert_2_src_requid( w_requid ).
          APPEND LINES OF wt_requid_tmp TO wt_requid.
          CLEAR: wt_requid_tmp[].
          DELETE s_requid.
          CONTINUE.
        ENDIF.
      ENDLOOP.
*
      APPEND LINES OF wt_requid TO s_requid.
      CLEAR: wt_requid[].
    ENDIF.

    IF ( lcl_infocube=>if_exists( p_target(30) ) = 'X' ).
      LOOP AT s_requid INTO ws_requid.
        IF lcl_request=>if_requid_in( iv_requid   = ws_requid-low
                                      iv_infocube = p_target ) = 'X'.
          wt_requid_tmp =
            lcl_request=>convert_2_src_requid( ws_requid-low ).
          APPEND LINES OF wt_requid_tmp TO wt_requid.
          CLEAR: wt_requid_tmp[].
          DELETE s_requid.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF wt_requid TO s_requid.
      CLEAR: wt_requid[].
    ENDIF.

    IF ( lcl_datastore=>if_exists( p_source(30) ) = 'X' ).
      LOOP AT s_requid INTO ws_requid.
        IF lcl_request=>if_requid_in( iv_requid    = ws_requid-low
                                      iv_datastore = p_source ) = 'X'.
          ws_requid-low = lcl_request=>convert_2_odsrnrsid(
                                        iv_requid    = ws_requid-low
                                        iv_datastore = p_source ).
          MODIFY s_requid FROM ws_requid TRANSPORTING low.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF ( lcl_infoobject=>if_exists( p_target(30) ) = 'X' ).
      LOOP AT s_requid INTO ws_requid.
        IF lcl_request=>if_requid_in( iv_requid     = ws_requid-low
                                      iv_infoobject = p_target ) = 'X'.
          wt_requid_tmp =
            lcl_request=>convert_2_src_requid( ws_requid-low ).
          APPEND LINES OF wt_requid_tmp TO wt_requid.
          CLEAR: wt_requid_tmp[].
          DELETE s_requid.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF wt_requid TO s_requid.
      CLEAR: wt_requid[].
    ENDIF.

  ENDMETHOD.


*---------------------------------------------------------------------*
* f4_target
*---------------------------------------------------------------------*
  METHOD f4_target.
  TYPES: t_field TYPE TABLE OF help_value.
  TYPES: BEGIN OF value,
           value(40) TYPE c,
         END OF value,
         t_value TYPE TABLE OF value.
  DATA: wa_field TYPE LINE OF t_field,
        wt_field TYPE t_field.
  DATA: wa_value TYPE value,
        wt_value TYPE t_value.
  DATA: w_selected_target TYPE rsawbnobjnm.
  DATA: wa_target TYPE s_target,
        wt_target TYPE t_target.
  DATA: w_target TYPE rsawbnobjnm.


    CALL METHOD lcl_application=>get_field_value
         EXPORTING iv_fieldname  = 'P_TARGET'
         IMPORTING ev_fieldvalue = w_target.
    IF w_target = SPACE.
      w_target = '*'.
    ENDIF.
    CALL METHOD lcl_dtp=>target_get_list
         IMPORTING et_target = wt_target.
    SORT wt_target BY id tgt.
    LOOP AT wt_target INTO wa_target WHERE tgt CP w_target.
      APPEND: wa_target-id   TO wt_value,
              wa_target-tgt  TO wt_value,
              wa_target-text TO wt_value.
    ENDLOOP.

    CLEAR wt_field[].
    wa_field-tabname    = 'ICON'.
    wa_field-fieldname  = 'ID'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSBKDTP'.
    wa_field-fieldname  = 'TGT'.
    wa_field-selectflag = 'X'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSDCUBET'.
    wa_field-fieldname  = 'TXTLG'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.

    wa_field-tabname    = 'RSBKDTP'.
    wa_field-fieldname  = 'TGT'.
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

  ENDMETHOD.

*---------------------------------------------------------------------*
* f4_source
*---------------------------------------------------------------------*
  METHOD f4_source.
  DATA: w_target TYPE rsawbnobjnm.
  DATA: w_source      TYPE rsawbnobjnm,
        w_source_type TYPE icon_d.
  DATA: local_exception TYPE REF TO lcl_local_exception.

    CALL METHOD lcl_application=>get_field_value
         EXPORTING iv_fieldname  = 'P_TARGET'
         IMPORTING ev_fieldvalue = w_target.
    TRY.
      lcl_dtp=>if_target_exists( w_target ).
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'I'.
        EXIT.
    ENDTRY.

    CALL METHOD lcl_dtp=>select_source
         EXPORTING iv_target      = w_target
         IMPORTING ev_source      = w_source
                   ev_source_type = w_source_type.
    IF w_source <> SPACE.
      p_source = w_source.
      c_source = w_source_type.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* f4_dtp
*---------------------------------------------------------------------*
  METHOD f4_dtp.
  TYPES: t_field TYPE TABLE OF help_value.
  TYPES: BEGIN OF value,
           value(60) TYPE c,
         END OF value,
         t_value TYPE TABLE OF value.
  DATA: wa_field TYPE LINE OF t_field,
        wt_field TYPE t_field.
  DATA: wa_value TYPE value,
        wt_value TYPE t_value.
  DATA: w_selected_dtp TYPE rsbkdtpnm.
  DATA: wa_dtp TYPE s_dtp,
        wt_dtp TYPE t_dtp.
  DATA: w_target TYPE rsawbnobjnm,
        w_source TYPE rsawbnobjnm.
  DATA: local_exception TYPE REF TO lcl_local_exception.

    CALL METHOD lcl_application=>get_field_value:
         EXPORTING iv_fieldname  = 'P_TARGET'
         IMPORTING ev_fieldvalue = w_target,
         EXPORTING iv_fieldname  = 'P_SOURCE'
         IMPORTING ev_fieldvalue = w_source.
    TRY.
      lcl_dtp=>if_target_exists( w_target ).
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'I'.
        EXIT.
    ENDTRY.
    TRY.
      lcl_dtp=>if_source_exists( w_source ).
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'I'.
        EXIT.
    ENDTRY.

    TRY.
      CALL METHOD lcl_dtp=>dtp_get_list
           EXPORTING iv_source = w_source
                     iv_target = w_target
           IMPORTING et_dtp    = wt_dtp.
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'I'.
        EXIT.
    ENDTRY.
    LOOP AT wt_dtp INTO wa_dtp.
      APPEND: wa_dtp-name         TO wt_value,
              wa_dtp-text         TO wt_value,
              wa_dtp-updmode      TO wt_value,
              wa_dtp-updmode_text TO wt_value,
              wa_dtp-dtptype      TO wt_value,
              wa_dtp-dtptype_text TO wt_value,
              wa_dtp-filter_text  TO wt_value.
    ENDLOOP.

    CLEAR wt_field[].
    wa_field-tabname    = 'RSBKDTP'.
    wa_field-fieldname  = 'DTP'.
    wa_field-selectflag = 'X'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSBKDTPT'.
    wa_field-fieldname  = 'TXTLG'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSBKDTP'.
    wa_field-fieldname  = 'UPDMODE'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'DD07T'.
    wa_field-fieldname  = 'DDTEXT'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSBKDTP'.
    wa_field-fieldname  = 'DTPTYPE'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'DD07T'.
    wa_field-fieldname  = 'DDTEXT'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSBMDYNP104'.
    wa_field-fieldname  = 'FILTER_TXT'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.

    wa_field-tabname    = 'RSBKDTP'.
    wa_field-fieldname  = 'DTP'.
    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
         EXPORTING
              tabname      = wa_field-tabname
              fieldname    = wa_field-fieldname
         IMPORTING
              select_value = w_selected_dtp
         TABLES
              fields       = wt_field
              valuetab     = wt_value
         EXCEPTIONS
              OTHERS       = 99.
    IF sy-subrc EQ 0.
      p_dtp = w_selected_dtp.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* f4_request
*---------------------------------------------------------------------*
  METHOD f4_request.
  TYPES: t_field TYPE TABLE OF help_value.
  TYPES: BEGIN OF value,
           value(60) TYPE c,
         END OF value,
         t_value TYPE TABLE OF value.
  DATA: wa_field TYPE LINE OF t_field,
        wt_field TYPE t_field.
  DATA: wt_value TYPE t_value.
  DATA: w_selected_request TYPE rssid.
  DATA: w_rnr(60)      TYPE c,
        w_rcrdextr(60) TYPE c.
  DATA: w_target TYPE rsawbnobjnm,
        w_source TYPE rsawbnobjnm.
  DATA: wa_request TYPE s_request,
        wt_request TYPE t_request.
  DATA: request    TYPE REF TO lif_request,
        datastore  TYPE REF TO lcl_datastore,
        infocube   TYPE REF TO lcl_infocube,
        datasource TYPE REF TO lcl_datasource.
  DATA: local_exception TYPE REF TO lcl_local_exception.

    CALL METHOD lcl_application=>get_field_value:
         EXPORTING iv_fieldname  = 'P_SOURCE'
         IMPORTING ev_fieldvalue = w_source,
         EXPORTING iv_fieldname  = 'P_TARGET'
         IMPORTING ev_fieldvalue = w_target.
    TRY.
      lcl_dtp=>if_target_exists( w_target ).
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'I'.
        EXIT.
    ENDTRY.
    TRY.
      lcl_dtp=>if_source_exists( w_source ).
      CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'I'.
        EXIT.
    ENDTRY.

    CASE w_source+30(10).
    WHEN 'ATTRIBUTES'.
      MESSAGE i000(oo) WITH 'No Request(s) selection possible'
                            'for InfoObject Attribures Source of data'.
      EXIT.
    WHEN 'TEXTS'.
      MESSAGE i000(oo) WITH 'No Request(s) selection possible'
                            'for InfoObject Texts Source of data'.
      EXIT.
    ENDCASE.

    IF ( lcl_datasource=>if_exists( w_source ) = 'X' ).
      CREATE OBJECT datasource
        EXPORTING iv_datasource = w_source
                  iv_target     = w_target.
      request ?= datasource.
    ELSEIF ( lcl_datastore=>if_exists( w_source(30) ) = 'X' ).
      CREATE OBJECT datastore
        EXPORTING iv_datastore = w_source
                  iv_target    = w_target.
      request ?= datastore.
    ELSEIF ( lcl_infocube=>if_exists( w_source(30) )  = 'X' ).
      CREATE OBJECT infocube
        EXPORTING iv_infocube = w_source
                  iv_target   = w_target.
      request ?= infocube.
    ENDIF.

    CALL METHOD request->get_list
      IMPORTING et_request = wt_request.

    SORT wt_request BY rnr DESCENDING.
    LOOP AT wt_request INTO wa_request.
      w_rnr = wa_request-rnr.
      SHIFT w_rnr LEFT DELETING LEADING SPACE.
      w_rcrdextr = wa_request-rcrdextr.
      SHIFT w_rcrdextr RIGHT DELETING TRAILING SPACE.
      APPEND: wa_request-datamart_status TO wt_value,
              w_rnr                      TO wt_value,
              wa_request-request_status  TO wt_value,
              wa_request-date            TO wt_value,
              wa_request-time            TO wt_value,
              wa_request-text            TO wt_value,
              w_rcrdextr                 TO wt_value.
    ENDLOOP.

    CLEAR wt_field[].
    CLEAR: wa_field.
    wa_field-tabname    = 'RSREQDONE'.
    wa_field-fieldname  = 'TSTATUS'.
    APPEND wa_field TO wt_field.
    wa_field-tabname    = 'RSB2DYN01'.
    wa_field-fieldname  = 'PARTID_DP'.
    wa_field-selectflag = 'X'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSREQDONE'.
    wa_field-fieldname  = 'QMSTATUS'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSREQDONE'.
    wa_field-fieldname  = 'DATUM'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSREQDONE'.
    wa_field-fieldname  = 'UZEIT'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSLDPIOT'.
    wa_field-fieldname  = 'TEXT'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.
    wa_field-tabname    = 'RSCRT_DEMON_MONITOR'.
    wa_field-fieldname  = 'RECS_IN_REQUEST'.
    APPEND wa_field TO wt_field.
    CLEAR: wa_field.


    wa_field-tabname    = 'RSB2DYN01'.
    wa_field-fieldname  = 'PARTID_DP'.
    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
         EXPORTING
              tabname      = wa_field-tabname
              fieldname    = wa_field-fieldname
         IMPORTING
              select_value = w_selected_request
         TABLES
              fields       = wt_field
              valuetab     = wt_value
         EXCEPTIONS
              OTHERS       = 99.
    IF sy-subrc EQ 0.
      s_requid-low = w_selected_request.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* display_transform
*---------------------------------------------------------------------*
  METHOD display_transform.
  DATA: w_dtp    TYPE rsbkdtpnm,
        w_tranid TYPE rstranid.

    CALL METHOD lcl_application=>get_field_value
         EXPORTING iv_fieldname  = 'P_DTP'
         IMPORTING ev_fieldvalue = w_dtp.
    w_tranid = lcl_dtp=>get_transformation( w_dtp ).

    SUBMIT rstran_gui_start WITH tranid = w_tranid AND RETURN.

  ENDMETHOD.

*---------------------------------------------------------------------*
* manage_target
*---------------------------------------------------------------------*
  METHOD manage_target.
  DATA: w_target TYPE rsawbnobjnm.
  DATA: ws_icube TYPE LINE OF rssm_t_icube,
        wt_icube TYPE rssm_t_icube.

    CALL METHOD lcl_application=>get_field_value
         EXPORTING iv_fieldname  = 'P_TARGET'
         IMPORTING ev_fieldvalue = w_target.
    IF  w_target+30(10) = 'TEXTS'.
      CONCATENATE w_target(30) '$T' INTO w_target.
    ENDIF.
    ws_icube-icube = w_target.
    ws_icube-check = 'X'.
    APPEND ws_icube TO wt_icube.
    CALL FUNCTION 'RSSM_CALL_ICUBE_PFLEGE'
         EXPORTING
              e_content_first = 'X'
         TABLES
              e_t_icube           = wt_icube.

  ENDMETHOD.

*---------------------------------------------------------------------*
* update_sel_screen
*---------------------------------------------------------------------*
  METHOD update_sel_screen.
  DATA: ws_target TYPE s_bw_obj,
        ws_source TYPE s_bw_obj,
        ws_dtp    TYPE s_dtp.

    CALL METHOD lcl_dtp=>bw_obj_get_details:
         EXPORTING iv_bw_obj = p_target
         IMPORTING es_bw_obj = ws_target,
         EXPORTING iv_bw_obj = p_source
         IMPORTING es_bw_obj = ws_source.
    c_target = ws_target-id.
    t_target = ws_target-text.
    c_source = ws_source-id.
    t_source = ws_source-text.

    CALL METHOD lcl_dtp=>dtp_get_details
         EXPORTING iv_dtp = p_dtp
         IMPORTING es_dtp = ws_dtp.
    c_dtp = ws_dtp-id.
    t_dtp = ws_dtp-text.

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_fcat
*---------------------------------------------------------------------*
  METHOD get_fcat.
  DATA: ws_field TYPE rsbk_s_field_rt,
        ws_fcat  TYPE lvc_s_fcat.

    LOOP AT it_field INTO ws_field.
      ws_fcat-fieldname = ws_field-fieldname.
      IF ( iv_cmdtype = 'X' ) AND ( iv_srctp = 'DTASRC' ).
        ws_fcat-tooltip = ws_field-fieldname.
      ELSE.
        IF ws_field-fieldname CP '/BIC/*'.
          ws_fcat-tooltip = ws_field-fieldname+5.
        ELSE.
          CONCATENATE '0' ws_field-fieldname INTO ws_fcat-tooltip.
        ENDIF.
      ENDIF.
      ws_fcat-inttype   = ws_field-inttype.
      ws_fcat-intlen    = ws_field-intlen.
      ws_fcat-scrtext_s = ws_fcat-scrtext_l
                        = ws_fcat-scrtext_m
                        = ws_fcat-coltext
                        = ws_fcat-reptext
                        = ws_field-txtlg.
      IF ( iv_cmp_output = 'X' ) AND ( iv_cmdtype = 'T' ).
        CASE ws_field-fieldname.
        WHEN 'SID'    OR 'DATAPAKID' OR 'RECORD'  OR
             'CHNGID' OR 'REQUID'    OR 'RECORDTP'.
         ws_fcat-tech = 'X'.
        WHEN OTHERS.
        ENDCASE.
      ENDIF.
      APPEND ws_fcat TO rt_fcat.
      CLEAR: ws_fcat.
    ENDLOOP.

  ENDMETHOD.

*---------------------------------------------------------------------*
* swap_fcat_technm_n_descr
*---------------------------------------------------------------------*
  METHOD swap_fcat_technm_n_descr.
  DATA: ws_fcat  TYPE lvc_s_fcat.
  DATA: w_tooltip TYPE lvc_tip.
    LOOP AT ct_fcat INTO ws_fcat.
      w_tooltip = ws_fcat-tooltip.
      ws_fcat-tooltip = ws_fcat-scrtext_l.
      ws_fcat-scrtext_s = ws_fcat-scrtext_m
                        = ws_fcat-scrtext_l
                        = ws_fcat-coltext
                        = ws_fcat-reptext = w_tooltip.
      MODIFY ct_fcat FROM ws_fcat.
    ENDLOOP.

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_layout
*---------------------------------------------------------------------*
  METHOD get_layout.

    rs_layout-zebra      = 'X'.
    rs_layout-detailinit = 'X'.
    rs_layout-smalltitle = 'X'.
    rs_layout-grid_title = iv_title.

  ENDMETHOD.

*---------------------------------------------------------------------*
* doit
*---------------------------------------------------------------------*
  METHOD doit.

    CALL METHOD _dtp->simulate.

  ENDMETHOD.

*---------------------------------------------------------------------*
* constructor.
*---------------------------------------------------------------------*
  METHOD constructor.
  DATA: wa_request TYPE s_request,
        wt_request TYPE t_request.
  DATA: request    TYPE REF TO lif_request,
        psa        TYPE REF TO lif_psa,
        datastore  TYPE REF TO lcl_datastore,
        infocube   TYPE REF TO lcl_infocube,
        datasource TYPE REF TO lcl_datasource.
  DATA: local_exception TYPE REF TO lcl_local_exception.
  DATA: wa_requid TYPE LINE OF t_requid,
        wt_requid TYPE t_requid.
  DATA: w_psa_tech_name  TYPE rsodstech.

    IF ( it_requid[] IS INITIAL ) AND
       ( lcl_infoobject=>if_exists( iv_source(30) ) <> 'X' ).
      IF ( lcl_datasource=>if_exists( iv_source ) = 'X' ).
        CREATE OBJECT datasource
          EXPORTING iv_datasource = iv_source
                    iv_target     = iv_target.
        request ?= datasource.
        psa ?= datasource.
      ELSEIF ( lcl_datastore=>if_exists( iv_source(30) ) = 'X' ).
        CREATE OBJECT datastore
          EXPORTING iv_datastore = iv_source
                    iv_target    = iv_target.
        request ?= datastore.
        psa ?= datastore.
      ELSEIF ( lcl_infocube=>if_exists( iv_source(30) ) = 'X' ).
        CREATE OBJECT infocube
          EXPORTING iv_infocube = iv_source
                   iv_target    = iv_target.
        request ?= infocube.
      ENDIF.
      CALL METHOD request->get_list
        IMPORTING et_request = wt_request.
      wa_requid-sign   = 'I'.
      wa_requid-option = 'EQ'.
      LOOP AT wt_request INTO wa_request.
        wa_requid-low = wa_request-rnr.
        APPEND wa_requid TO wt_requid.
      ENDLOOP.
      IF ( lcl_datasource=>if_exists( iv_source ) = 'X' ) OR
         ( lcl_datastore=>if_exists( iv_source(30) ) = 'X' ).
        w_psa_tech_name = psa->get_psa_tech_name( ).
      ENDIF.
    ELSE.
      wt_requid[] = it_requid[].
    ENDIF.

    TRY.
      CREATE OBJECT _dtp EXPORTING iv_dtp           = iv_dtp
                                   iv_buffer_t      = iv_buffer_t
                                   iv_buffer_x      = iv_buffer_x
                                   iv_bp_t          = iv_bp_t
                                   iv_std_output    = iv_std_output
                                   iv_sel_optim     = iv_sel_optim
                                   iv_psa_tech_name = w_psa_tech_name
                                   iv_source        = iv_source
                                   it_requid        = wt_requid[].
      CATCH lcl_local_exception INTO local_exception.
        RAISE EXCEPTION TYPE lcl_local_exception
                EXPORTING TEXT = local_exception->local_text.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

*---------------------------------------------------------------------*
* handle_toolbar_x.
*---------------------------------------------------------------------*
  METHOD handle_toolbar_x.
  DATA: ls_toolbar  TYPE stb_button.

    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'TECHNM_DESCR'    TO ls_toolbar-function.
    CASE lcl_application=>descriptive_headers_x.
    WHEN 'X'.
      ls_toolbar-icon      = icon_working_plan.
      ls_toolbar-quickinfo = 'Show Technical Names'.
      ls_toolbar-text      = 'Technical Names'.
    WHEN SPACE.
      ls_toolbar-icon      = icon_working_plan.
      ls_toolbar-quickinfo = 'Show Descriptions'.
      ls_toolbar-text      = 'Descriptions'.
    ENDCASE.
    ls_toolbar-disabled = SPACE.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

*---------------------------------------------------------------------*
* handle_user_command_x.
*---------------------------------------------------------------------*
  METHOD handle_user_command_x.

    CASE e_ucomm.
    WHEN 'TECHNM_DESCR'.
      CALL METHOD lcl_application=>swap_fcat_technm_n_descr
        CHANGING ct_fcat = gt_fcat_x.
      alv_grid_top->set_frontend_fieldcatalog( gt_fcat_x ).
      CASE lcl_application=>descriptive_headers_x.
      WHEN SPACE.
        lcl_application=>descriptive_headers_x = 'X'.
      WHEN 'X'.
        lcl_application=>descriptive_headers_x = SPACE.
      ENDCASE.
    ENDCASE.
    alv_grid_top->refresh_table_display( ).

  ENDMETHOD.

*---------------------------------------------------------------------*
* handle_toolbar_t.
*---------------------------------------------------------------------*
  METHOD handle_toolbar_t.
  DATA: ls_toolbar  TYPE stb_button.

    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'EXPAND_COLLPS'    TO ls_toolbar-function.
    CASE p_cmpout.
    WHEN 'X'.
      ls_toolbar-icon      = icon_expand.
      ls_toolbar-quickinfo = 'Expand Data Package'.
      ls_toolbar-text      = 'Expand'.
    WHEN SPACE.
      ls_toolbar-icon      = icon_collapse.
      ls_toolbar-quickinfo = 'Collapse Data Package'.
      ls_toolbar-text      = 'Collapse'.
    ENDCASE.
    ls_toolbar-disabled = SPACE.
    APPEND ls_toolbar TO e_object->mt_toolbar.
*
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar.
    CLEAR ls_toolbar.
    MOVE 'TECHNM_DESCR'    TO ls_toolbar-function.
    CASE lcl_application=>descriptive_headers_t.
    WHEN 'X'.
      ls_toolbar-icon      = icon_working_plan.
      ls_toolbar-quickinfo = 'Show Technical Names'.
      ls_toolbar-text      = 'Technical Names'.
    WHEN SPACE.
      ls_toolbar-icon      = icon_working_plan.
      ls_toolbar-quickinfo = 'Show Descriptions'.
      ls_toolbar-text      = 'Descriptions'.
    ENDCASE.
    ls_toolbar-disabled = SPACE.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

*---------------------------------------------------------------------*
* handle_user_command_t.
*---------------------------------------------------------------------*
  METHOD handle_user_command_t.

    CASE e_ucomm.
    WHEN 'EXPAND_COLLPS'.
      CASE p_cmpout.
      WHEN 'X'.
        alv_grid_bottom->set_frontend_fieldcatalog( gt_fcat_t_expand ).
        <buffer_t_t> = <buffer_t_t_expand>.
        p_cmpout = SPACE.
      WHEN SPACE.
        alv_grid_bottom->set_frontend_fieldcatalog( gt_fcat_t_collps ).
        <buffer_t_t> = <buffer_t_t_collps>.
        p_cmpout = 'X'.
      ENDCASE.
    WHEN 'TECHNM_DESCR'.
      CALL METHOD lcl_application=>swap_fcat_technm_n_descr:
        CHANGING ct_fcat = gt_fcat_t_expand,
        CHANGING ct_fcat = gt_fcat_t_collps.
      CASE p_cmpout.
      WHEN 'X'.
        alv_grid_bottom->set_frontend_fieldcatalog( gt_fcat_t_collps ).
      WHEN SPACE.
        alv_grid_bottom->set_frontend_fieldcatalog( gt_fcat_t_expand ).
      ENDCASE.
      CASE lcl_application=>descriptive_headers_t.
      WHEN SPACE.
        lcl_application=>descriptive_headers_t = 'X'.
      WHEN 'X'.
        lcl_application=>descriptive_headers_t = SPACE.
      ENDCASE.
    ENDCASE.
    alv_grid_bottom->refresh_table_display( ).

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_dtp  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_dtp IMPLEMENTATION.
*---------------------------------------------------------------------*
* constructor.
*---------------------------------------------------------------------*
  METHOD constructor.
  DATA: l_r_dtp_a type ref to cl_rsbk_dtp_a,
        l_r_dtp_display type ref to cl_rsbk_dtp_a,
        _answer.
  DATA: local_exception TYPE REF TO lcl_local_exception.

*******************
    TRY.
      lcl_dtp=>check_authorization( iv_dtp ).
      CATCH lcl_local_exception INTO local_exception.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = local_exception->local_text.
    ENDTRY.
*******************
*******************
    _buffer_t   = iv_buffer_t.
    _buffer_x   = iv_buffer_x.
    _bp_t       = iv_bp_t.
    _std_output = iv_std_output.
*******************
    try.
      _r_dtp = cl_rsbk_dtp=>factory( iv_dtp ).
      l_r_dtp_a ?=
       _r_dtp->get_obj_ref_objvers( i_objvers =  rs_c_objvers-active ).
      l_r_dtp_display ?=
       _r_dtp->get_obj_ref_display( i_objvers =  rs_c_objvers-active ).
      l_r_dtp_display = l_r_dtp_a.
      l_r_dtp_a->set_simulation( rs_c_true ).

      _r_request = _r_dtp->create_request( ).
      _r_request->set_ctype( rsbc_c_ctype-sync ).

* ===== error handling ======
      data l_r_t100 type ref to if_t100_message.
      data l_r_error type ref to cx_static_check.
      catch cx_rs_failed into l_r_error.
        while not l_r_error->previous is initial.
          l_r_error ?= l_r_error->previous.
        endwhile.
        case cl_rsbk_reuse=>get_classname( l_r_error ).
        when 'CX_RSBK_REQUEST_LOCKED'.
          try.
            data l_r_request_locked type ref to cx_rsbk_request_locked.
            l_r_request_locked ?= l_r_error.
* --- this means, that a request is standing in our way! ----
            data: l_requid_act type rsbkrequid,
                  l_requid_tmp type string.
            l_requid_tmp = l_r_request_locked->requid.
            replace '.' in l_requid_tmp with ''.
            move l_requid_tmp to l_requid_act.
            call method me->check_active_request
                 exporting iv_requid = l_requid_act.
* --- if it worked, we can try to create the new request once more ---
            data: l_rx_static_check type ref to cx_rs_static_check.
            try.
              _r_request = _r_dtp->create_request( ).
              _r_request->set_ctype( rsbc_c_ctype-sync ).
              catch cx_rs_static_check into l_rx_static_check.
                message x299.
              endtry.
              catch cx_rs_foreign_lock into l_r_error.
                data l_text type string.
                l_text = l_r_error->if_message~get_text( ).
                message e018 with l_text.
              catch cx_rs_static_check.
                exit.
            endtry.
        when 'CX_RSBK_REQUEST_CREATION'.
          l_r_t100 ?= l_r_error.
          case l_r_t100->t100key-msgno.
          when '032'.
            call function 'POPUP_TO_CONFIRM'
                 exporting
                  titlebar      = 'Requesterzeugung fehlgeschlagen'(019)
                  text_question = text-018
                 importing
                  answer        = _answer.
            if _answer = '1'.
              try.
                _r_dtp->activate( ).
                _r_request = _r_dtp->create_request( ).
                _r_request->set_ctype( rsbc_c_ctype-sync ).
                catch cx_static_check into l_r_error.
                  call function 'RS_EXCEPTION_TO_SYMSG'
                       exporting
                             i_r_exception = l_r_error.
                  message id sy-msgid type 'E' number sy-msgno
                          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              endtry.
            else.
              exit.
            endif.
          when others.
            call function 'RS_EXCEPTION_TO_SYMSG'
                 exporting
                      i_r_exception = l_r_error.
            message id sy-msgid type 'E' number sy-msgno
                    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endcase.
        when others.
          call function 'RS_EXCEPTION_TO_SYMSG'
            exporting
              i_r_exception = l_r_error.
          message id sy-msgid type 'E' number sy-msgno
                     with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endcase.
      catch cx_rs_foreign_lock cx_rs_not_found into l_r_error.
        message l_r_error type 'E'.
    endtry.

* === if the expert-debug flag is set, change the request attributes ===
*  if rsbkdynp206-debugexpertmode = 'X'
*    and g_r_viewer->n_processmode = rsbc_c_processmode-sync_debug.
    data l_genuid_debug type rssguid25.
    l_genuid_debug = me->get_genuid_debug( ).
    data l_r_cmd_x type ref to cl_rsbk_cmd_x.
    l_r_cmd_x ?= _r_request->get_obj_ref_cmd_x( ).
    l_r_cmd_x->if_rsbk_filter_model~set_gen_progid( l_genuid_debug ).

************
   data: l_s_range  type rsbk_s_range,
         l_th_range type rsbk_th_range.
   DATA: wa_requid TYPE LINE OF t_requid.

   IF ( iv_sel_optim = SPACE ).
     l_th_range = _r_request->get_th_range( ).
     DELETE l_th_range WHERE fieldnm = 'REQUID'.
     LOOP AT it_requid[] INTO wa_requid.
       l_s_range-sign    = wa_requid-sign.
       l_s_range-option  = wa_requid-option.
       l_s_range-low     = wa_requid-low.
       SHIFT l_s_range-low LEFT DELETING LEADING SPACE.
       l_s_range-posit   = sy-tabix.
       l_s_range-fieldnm = 'REQUID'.
       INSERT l_s_range INTO TABLE l_th_range.
     ENDLOOP.
     l_r_cmd_x->set_th_range( l_th_range ).
   ELSE.
     CLEAR: l_th_range[].
     l_r_cmd_x->set_th_range( l_th_range ).
   ENDIF.
************

    try.
        data l_r_request_edit type ref to if_rsbk_request_edit.
        l_r_request_edit = _r_request->get_obj_ref_edit( ).
      catch cx_rs_not_found.
        message x299.
    endtry.

**********
      data: s_th_datapakid type rsbk_th_datapakid.
      data l_s_datapakid like line of s_th_datapakid.
      if s_th_datapakid is initial.
        while l_s_datapakid-datapakid < 200.
          add 1 to l_s_datapakid-datapakid.
          insert l_s_datapakid into table s_th_datapakid.
         endwhile.
      endif.
      data l_s_cmd_r type rsbk_s_cmd_r.
      data l_r_cmd type ref to if_rsbk_cmd.
      data: l_s_bp type rsbk_s_bp,
            l_th_bp type rsbk_th_bp.
      data l_th_cmd_r type rsbk_th_cmd_r.
      l_th_cmd_r = l_r_request_edit->get_th_trf_r( ).
      loop at l_th_cmd_r into l_s_cmd_r.
        if _bp_t = 'X'.
          l_s_bp-bpid = 1.
          l_s_bp-th_datapakid = s_th_datapakid.
          insert l_s_bp into table l_th_bp.
          l_s_cmd_r-r_cmd->set_breakpoints( l_th_bp ).
        endif.
        l_s_cmd_r-r_cmd->n_persist_data = iv_buffer_t.
      endloop.
      l_r_cmd ?= l_r_request_edit->get_obj_ref_cmd_x( ).
      l_r_cmd->n_persist_data = iv_buffer_x.

*****************
    IF p_mode = 'X'.
      try.
        call function 'RSBM_EDIT_REQUEST_GUI'
          exporting
            i_r_request_edit = l_r_request_edit.
        catch cx_rs_cancelled .
          _r_request->free( ).
          RAISE EXCEPTION TYPE lcl_local_exception
                EXPORTING TEXT = `Simulation was cancelled`.
          exit.
      endtry.
    ELSE.
***
      data l_r_filter type ref to if_rsbk_filter_model.
      l_r_filter = l_r_request_edit->get_obj_ref_filter_model( ).
      call function 'RSBC_FILTER_GUI2_INIT'
        exporting
          i_r_filter = l_r_filter
          i_mode     = rsbc_c_mode-maintain.
      call function 'RSBC_FILTER_GUI2_RESET'
        exporting
          i_r_filter = l_r_filter.
***
    ENDIF.

***
    IF ( iv_sel_optim = 'X' ).
      l_th_range = _r_request->get_th_range( ).
      DELETE l_th_range[] WHERE fieldnm = 'DATAPAKID' OR
                                fieldnm = 'RECORD'    OR
                                fieldnm = 'REQUID'.
      IF NOT l_th_range[] IS INITIAL.
        IF ( lcl_datasource=>if_exists( iv_source ) = 'X' ).
          l_th_range = lcl_request=>optimize_selection(
            iv_psa_tech_name   = iv_psa_tech_name
            iv_limit_datapakid = 'X'
            it_range           = l_th_range ).
        ELSEIF ( lcl_datastore=>if_exists( iv_source+0(30) ) = 'X' ).
          l_th_range = lcl_request=>optimize_selection(
            iv_psa_tech_name   = iv_psa_tech_name
            iv_limit_datapakid = SPACE
            it_range           = l_th_range ).
        ENDIF.
        l_r_cmd_x->set_th_range( l_th_range ).
        IF l_th_range[] IS INITIAL.
          _r_request->free( ).
          RAISE EXCEPTION TYPE lcl_local_exception
                EXPORTING TEXT = `No data for simulation`.
        ENDIF.
      ENDIF.
      l_r_filter = l_r_request_edit->get_obj_ref_filter_model( ).
      call function 'RSBC_FILTER_GUI2_INIT'
           exporting
                i_r_filter = l_r_filter
                i_mode     = rsbc_c_mode-maintain.
      call function 'RSBC_FILTER_GUI2_RESET'
           exporting
                i_r_filter = l_r_filter.
    ENDIF.
***

  ENDMETHOD.

*---------------------------------------------------------------------*
* check_authorization
*---------------------------------------------------------------------*
  METHOD check_authorization.
  DATA: w_src         TYPE rsbksrcnm,
        w_srctp       TYPE rsbksrctp,
        w_srctlogo    TYPE rsbksrctlogo,
        w_srctlogosub TYPE rssb_tlogo_sub_dtp_src,
        w_tgt         TYPE rsbktgtnm,
        w_tgttp       TYPE rsbktgttp,
        w_tgttlogo    TYPE rsbktgttlogo,
        w_tgttlogosub TYPE rssb_tlogo_sub_dtp_tgt.
  DATA: w_text TYPE string.

    SELECT SINGLE src srctp srctlogo tgt tgttp tgttlogo
    INTO (w_src, w_srctp, w_srctlogo, w_tgt, w_tgttp, w_tgttlogo)
    FROM rsbkdtp
    WHERE dtp     = iv_dtp
      AND objvers = 'A'.
    IF sy-subrc = 0.
      CASE w_srctp.
      WHEN 'IOBJA'.
        w_srctlogosub = 'ATTR'.
      WHEN 'IOBJT'.
        w_srctlogosub = 'TEXT'.
      WHEN 'IOBJH'.
        w_srctlogosub = 'HIERARCHY'.
      ENDCASE.
      CASE w_tgttp.
      WHEN 'IOBJA'.
        w_tgttlogosub = 'ATTR'.
      WHEN 'IOBJT'.
        w_tgttlogosub = 'TEXT'.
      WHEN 'IOBJH'.
        w_tgttlogosub = 'HIERARCHY'.
      ENDCASE.
    ENDIF.
    AUTHORITY-CHECK OBJECT 'S_RS_DTP'
      ID 'RSTLDTPSRC'  FIELD w_srctlogo
      ID 'RSSTDTPSRC'  FIELD w_srctlogosub
      ID 'RSONDTPSRC'  FIELD w_src
      ID 'RSTLDTPTGT'  FIELD w_tgttlogo
      ID 'RSSTDTPTGT'  FIELD w_tgttlogosub
      ID 'RSONDTPTGT'  FIELD w_tgt
      ID 'ACTVT'       FIELD rssb_c_auth_actvt-display.
    IF sy-subrc <> 0.
      CONCATENATE 'No Display/Simulate authorization for DTP' iv_dtp
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception
        EXPORTING TEXT = w_text.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* simulate
*---------------------------------------------------------------------*
  METHOD simulate.
  data l_r_error type ref to cx_static_check.
  data l_answer.

* === continue by starting request =========
    try.
* ===== EXECUTE REQUEST ========
      _r_request->doit( ).
      if _r_request->get_simulation( ) = rs_c_true.
        l_answer = '1'.
      else.
        data l_tstate type rsbktstate.
        l_tstate = _r_request->get_tstate( ).
        data l_tstate_text type string.
        l_tstate_text =
          cl_rsbk_request_state=>get_text_from_tstate( l_tstate ).
        condense l_tstate_text.
        concatenate text-016
                    l_tstate_text '.'
                    text-017
          into l_tstate_text separated by space.
        call function 'POPUP_TO_CONFIRM'
          exporting
            titlebar      = 'Request-Status'(015)
            text_question = l_tstate_text
          importing
            answer        = l_answer.
      endif.

      if l_answer = '1'.
        CALL METHOD me->display_request( ).
      else.
        _r_request->free( ).
      endif.
    catch cx_rs_not_found cx_rs_failed
      cx_rs_error_with_message cx_rs_access_error into l_r_error.
      message l_r_error type 'E'.
    endtry.

  ENDMETHOD.

*---------------------------------------------------------------------*
* display_request
*---------------------------------------------------------------------*
  METHOD display_request.
  DATA: local_exception TYPE REF TO lcl_local_exception.

    CASE _std_output.
    WHEN 'X'.
*     perform check_changed_and_save in program saplrsbk_gui.
      call function 'RSBM_SHOW_REQUEST'
           exporting
                i_requid = _r_request->n_requid.
    WHEN SPACE.
      TRY.
        CALL METHOD me->display_request_cust.
        CATCH lcl_local_exception INTO local_exception.
        MESSAGE local_exception->local_text TYPE 'I'.
      ENDTRY.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* display_request_cust
*---------------------------------------------------------------------*
  METHOD display_request_cust.
  DATA: wr_buffer_x        TYPE REF TO data,
        wr_buffer_t        TYPE REF TO data,
        wr_buffer_t_collps TYPE REF TO data,
        wr_buffer_t_expand TYPE REF TO data.
  DATA: wt_field_x  TYPE rsbk_th_field_rt,
        wt_field_t  TYPE rsbk_th_field_rt.
  DATA: w_srctp     TYPE rsbksrctp.

    w_srctp = _r_dtp->get_srctp( ).
    IF _buffer_x = 'X'.
      CALL METHOD me->read_buffer
           EXPORTING iv_cmdtype    = c_cmdtype_x
                     iv_cmp_output = SPACE
           CHANGING  cr_data       = wr_buffer_x
                     ct_field      = wt_field_x.
      IF wr_buffer_x IS BOUND.
        ASSIGN wr_buffer_x->* TO <buffer_t_x>.
        IF gt_fcat_x[] IS INITIAL.
          gt_fcat_x =
            lcl_application=>get_fcat( it_field      = wt_field_x
                                       iv_cmdtype    = c_cmdtype_x
                                       iv_cmp_output = SPACE
                                       iv_srctp      = w_srctp ).
          IF lcl_application=>descriptive_headers_x = SPACE.
            CALL METHOD lcl_application=>swap_fcat_technm_n_descr
              CHANGING ct_fcat = gt_fcat_x.
          ENDIF.
        ENDIF.
        gs_layout_x = lcl_application=>get_layout( 'After Extraction' ).
      ELSE.
        RAISE EXCEPTION TYPE lcl_local_exception
                EXPORTING TEXT = `No data for simulation`.
      ENDIF.
    ENDIF.
    IF _buffer_t = 'X'.
      CALL METHOD me->read_buffer
           EXPORTING iv_cmdtype     = c_cmdtype_t
                     iv_cmp_output  = 'X'
           CHANGING  cr_data        = wr_buffer_t
                     cr_data_collps = wr_buffer_t_collps
                     cr_data_expand = wr_buffer_t_expand
                     ct_field       = wt_field_t.
      ASSIGN: wr_buffer_t->*        TO <buffer_t_t>,
              wr_buffer_t_collps->* TO <buffer_t_t_collps>,
              wr_buffer_t_expand->* TO <buffer_t_t_expand>.
      IF gt_fcat_t_expand[] IS INITIAL.
        gt_fcat_t_expand =
         lcl_application=>get_fcat( it_field      = wt_field_t
                                    iv_cmdtype    = c_cmdtype_t
                                    iv_cmp_output = SPACE
                                    iv_srctp      = w_srctp ).
        gt_fcat_t_collps =
          lcl_application=>get_fcat( it_field      = wt_field_t
                                     iv_cmdtype    = c_cmdtype_t
                                     iv_cmp_output = 'X'
                                     iv_srctp      = w_srctp ).
        IF lcl_application=>descriptive_headers_t = SPACE.
          CALL METHOD lcl_application=>swap_fcat_technm_n_descr:
            CHANGING ct_fcat = gt_fcat_t_expand,
            CHANGING ct_fcat = gt_fcat_t_collps.
        ENDIF.
      ENDIF.
      gs_layout_t =
        lcl_application=>get_layout( 'After Transformation' ).
    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_cmd_node_id
*---------------------------------------------------------------------*
  METHOD get_cmd_node_id.
  DATA: w_cmd TYPE rsbcmd.
  DATA: ws_cmd_r TYPE rsbk_s_cmd_r,
        wt_cmd_r TYPE rsbk_th_cmd_r.

    CASE iv_cmdtype.
    WHEN 'X'.

      SELECT cmd
      INTO w_cmd
      FROM rsbkcmdprop UP TO 1 ROWS
      WHERE srctp IN ( SELECT srctp
                       FROM rsbkdtp
                       WHERE dtp     = _r_dtp->n_dtp
                         AND objvers = 'A'           ).
      ENDSELECT.
      IF sy-subrc <> 0.
        CLEAR w_cmd.
      ENDIF.
    WHEN 'T'.
      w_cmd = 'TRFN'.
    ENDCASE.

    wt_cmd_r = _r_request->get_th_cmd_r( ).
    LOOP AT wt_cmd_r INTO ws_cmd_r.
      IF ws_cmd_r-r_cmd->n_cmd = w_cmd.
        ev_step_from = ws_cmd_r-node.
        EXIT.
      ENDIF.
    ENDLOOP.

    READ TABLE wt_cmd_r INTO ws_cmd_r
      WITH TABLE KEY node = ev_step_from.
    IF NOT ws_cmd_r-child IS INITIAL.
      READ TABLe wt_cmd_r INTO ws_cmd_r
        WITH TABLE KEY node = ws_cmd_r-child.
    ELSEIF NOT ws_cmd_r-next IS INITIAL.
      READ TABLE wt_cmd_r INTO ws_cmd_r
        WITH TABLE KEY node = ws_cmd_r-next.
    ENDIF.
    ev_step_to = ws_cmd_r-node.

  ENDMETHOD.

*---------------------------------------------------------------------*
* read_buffer
*---------------------------------------------------------------------*
  METHOD read_buffer.
  DATA: w_step_from TYPE rsbcmdnode,
        w_step_to TYPE rsbcmdnode.
  DATA: ws_dp_r TYPE rsbk_s_dp_r,
        wt_dp_r TYPE rsbk_th_dp_r.
  DATA: wr_data TYPE REF TO cl_rsbk_data,
        wr_log  TYPE REF TO cl_rsbm_log_cursor_step.
  DATA: ws_segment_r TYPE LINE OF cl_rsbk_data=>th_segment_r,
        wt_segment_r TYPE cl_rsbk_data=>th_segment_r,
        wr_segment   TYPE REF TO cl_rsbk_data_segment.
  DATA: wr_segment_data     TYPE REF TO DATA,
        wr_data_temp        TYPE REF TO DATA,
        wr_data_temp_collps TYPE REF TO DATA,
        wr_data_temp_expand TYPE REF TO DATA.
  FIELD-SYMBOLS: <segment>       TYPE ANY,
                 <data_t>        TYPE ANY TABLE,
                 <data_t_collps> TYPE ANY TABLE,
                 <data_t_expand> TYPE ANY TABLE,
                 <segment_t>   TYPE ANY TABLE.
  FIELD-SYMBOLS: <sid>       TYPE ANY,
                 <datapakid> TYPE ANY,
                 <record>    TYPE ANY,
                 <chngid>    TYPE ANY,
                 <requid>    TYPE ANY,
                 <recordtp>  TYPE ANY.

    CALL METHOD me->get_cmd_node_id EXPORTING iv_cmdtype   = iv_cmdtype
                                    IMPORTING ev_step_from = w_step_from
                                              ev_step_to   = w_step_to.
    wt_dp_r = _r_request->get_th_dp_r( ).


    LOOP AT wt_dp_r INTO ws_dp_r.
      CHECK ( ws_dp_r-r_dp->get_lines_read( ) >  0 ).
      CALL METHOD cl_rsbk_data=>create_from_dtp_buffer
           EXPORTING
                i_r_request = _r_request
                i_datapakid = ws_dp_r-datapakid
                i_step_from = w_step_from
                i_step_to   = w_step_to
                i_r_log     = wr_log
           RECEIVING
                r_r_data    = wr_data.
      wt_segment_r = wr_data->n_th_segment_r.
      LOOP AT wt_segment_r INTO ws_segment_r.
        wr_segment = ws_segment_r-r_segment.
        wr_segment_data  = wr_segment->get_data( 'X' ).
        ASSIGN: wr_segment_data->* TO <segment_t>.
        IF cr_data IS NOT BOUND.
          CREATE DATA wr_data_temp LIKE <segment_t>.
          ASSIGN wr_data_temp->* TO <data_t>.
          IF cr_data_collps IS SUPPLIED.
            CREATE DATA: wr_data_temp_expand LIKE <segment_t>,
                         wr_data_temp_collps LIKE <segment_t>.
            ASSIGN: wr_data_temp_collps->* TO <data_t_collps>,
                    wr_data_temp_expand->* TO <data_t_expand>.
          ENDIF.
          ct_field = wr_segment->get_fieldlist( 'X' ).
          IF ( iv_cmdtype = 'T' ) OR
             ( iv_cmdtype = 'X' AND _r_dtp->get_srctp( ) <> 'DTASRC' ).
            CALL METHOD me->upd_fld_text CHANGING ct_field = ct_field.
          ENDIF.
        ENDIF.

        LOOP AT <segment_t> ASSIGNING <segment>.
          INSERT <segment> INTO TABLE <data_t>.
          IF cr_data_collps IS SUPPLIED.
            INSERT <segment> INTO TABLE <data_t_expand>.
            ASSIGN COMPONENT:
              'SID'       OF STRUCTURE <segment> TO <sid>,
              'DATAPAKID' OF STRUCTURE <segment> TO <datapakid>,
              'RECORD'    OF STRUCTURE <segment> TO <record>,
              'CHNGID'    OF STRUCTURE <segment> TO <chngid>,
              'REQUID'    OF STRUCTURE <segment> TO <requid>,
              'RECORDTP'  OF STRUCTURE <segment> TO <recordtp>.
              IF <sid>       IS ASSIGNED. CLEAR <sid>.       ENDIF.
              IF <datapakid> IS ASSIGNED. CLEAR <datapakid>. ENDIF.
              IF <record>    IS ASSIGNED. CLEAR <record>.    ENDIF.
              IF <chngid>    IS ASSIGNED. CLEAR <chngid>.    ENDIF.
              IF <requid>    IS ASSIGNED. CLEAR <requid>.    ENDIF.
              IF <recordtp>  IS ASSIGNED. CLEAR <recordtp>.  ENDIF.
            COLLECT <segment> INTO <data_t_collps>.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      GET REFERENCE OF <data_t> INTO cr_data.
      IF cr_data_collps IS SUPPLIED.
        GET REFERENCE OF: <data_t_collps> INTO cr_data_collps,
                          <data_t_expand> INTO cr_data_expand.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

*---------------------------------------------------------------------*
* upd_fld_text
*---------------------------------------------------------------------*
  METHOD upd_fld_text.
  DATA: ws_field TYPE rsbk_s_field_rt.
  DATA: w_iobjnm TYPE rsdiobjnm.

    LOOP AT ct_field INTO ws_field.
      IF ws_field-fieldname CP '/BIC/*'.
        w_iobjnm = ws_field-fieldname+5.
      ELSEIF ws_field-fieldname = 'SID'.
        ws_field-txtlg = 'Simulation ID'.
*       MODIFY ct_field FROM ws_field TRANSPORTING txtlg.                                       "P1805 (-)
        MODIFY ct_field FROM ws_field TRANSPORTING txtlg WHERE fieldname = ws_field-fieldname.  "P1805 (+)
      ELSE.
        CONCATENATE '0' ws_field-fieldname INTO w_iobjnm.
      ENDIF.
      SELECT SINGLE txtsh
      INTO ws_field-txtlg
      FROM rsdiobjt
      WHERE iobjnm  = w_iobjnm
        AND objvers = 'A'
        AND langu   = 'E'.
      IF sy-subrc = 0.
*       MODIFY ct_field FROM ws_field TRANSPORTING txtlg.                                       "P1805 (-)
        MODIFY ct_field FROM ws_field TRANSPORTING txtlg WHERE fieldname = ws_field-fieldname.  "P1805 (+)
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_genuid_debug
*---------------------------------------------------------------------*
  METHOD get_genuid_debug.

    select single genuid_debug
    from rsbkdtpstat
    into rv_genuid_debug
    where dtp = _r_dtp->n_dtp.
    if sy-subrc <> 0.
      message x299.
    endif.
    if rv_genuid_debug is initial.
      data: l_uid type rssguid25.
      call function 'RSS_SYSTEM_GET_UNIQUE_ID'
           importing
                e_uni_idc25 = rv_genuid_debug.
      update rsbkdtpstat
      set genuid_debug = rv_genuid_debug
      where dtp = _r_dtp->n_dtp.
      call function 'DB_COMMIT'.
    endif.

  ENDMETHOD.

*---------------------------------------------------------------------*
* check_active_request
*---------------------------------------------------------------------*
  METHOD check_active_request.
  data l_r_request_act type ref to if_rsbk_request.

    try.
       l_r_request_act = cl_rsbk_request=>create_from_db( iv_requid ).
      catch cx_rs_not_found.
        exit.
    endtry.
    l_r_request_act->check_tstate( ).
    case l_r_request_act->get_tstate( ).
    when rsbc_c_tstate-active.
* ---- if it's active, we can't do anything -------
      message e205 with iv_requid.
      data: l_key type string,
            l_object type string,
            l_action type string.
      l_key = iv_requid.
      l_object = 'Request'(012).
      l_action = 'Ändern'(006).
      raise exception type cx_rs_failed
        exporting
          object = l_object
          key    = l_key
          action = l_action.
    when rsbc_c_tstate-ready.
* ---- if it's ready, we have to delete it first -------
      data l_text type string.
      concatenate text-005 text-003 text-004 into l_text.
      data l_answer(1).
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar      = 'Request starten'(014)
          text_question = l_text
          text_button_1 = 'Löschen'(006)
          icon_button_1 = ''
          text_button_2 = 'Reparieren'(013)
        importing
          answer        = l_answer.
      case l_answer.
        when '1'.
          l_r_request_act->delete( ).
        when '2'.
          l_r_request_act->resume( ).
        when others.
          l_key = iv_requid.
          l_object = 'Request'(012).
          l_action = 'Löschen'(006).
          raise exception type cx_rs_failed
            exporting
              object = l_object
              key    = l_key
              action = l_action.
      endcase.
    when rsbc_c_tstate-red.
* ---- if it's red, we have to delete it first (or set to ustate green)
      concatenate text-005 text-008 text-004 into l_text.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar      = 'Request starten'(014)
          text_question = l_text
          text_button_1 = 'Löschen'(006)
          icon_button_1 = ' '
          text_button_2 = 'Reparieren'(013)
        importing
          answer        = l_answer.
      case l_answer.
        when '1'.
          l_r_request_act->set_ustate_red( ).
          l_r_request_act->delete( ).
        when '2'.
          l_r_request_act->resume( ).
        when others.
          l_key = iv_requid.
          l_object = 'Request'(012).
          l_action = 'Löschen'(006).
          raise exception type cx_rs_failed
            exporting
              object = l_object
              key    = l_key
              action = l_action.
      endcase.
    when rsbc_c_tstate-green.
* ---- if it's red, we have to set the ustate to green first (or delete
      concatenate text-005 text-009 text-010 into l_text.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar      = 'Request starten'(014)
          text_question = l_text
          text_button_1 = 'Status Grün'(011)
          icon_button_1 = ' '
        importing
          answer        = l_answer.
      if l_answer = '1'.
        l_r_request_act->set_ustate_green( ).
      else.
        l_key = iv_requid.
        l_object = 'Request'(012).
        l_action =  'Status Grün'(011).
        raise exception type cx_rs_failed
          exporting
            object = l_object
            key    = l_key
            action = l_action.
      endif.
    when others.
    endcase.

  ENDMETHOD.

*---------------------------------------------------------------------*
* bw_obj_get_details
*---------------------------------------------------------------------*
  METHOD bw_obj_get_details.

    es_bw_obj-bw_obj = iv_bw_obj.
    IF ( lcl_datasource=>if_exists( iv_bw_obj ) = 'X' ).
      es_bw_obj-id = icon_bw_datasource.
      SELECT SINGLE txtsh
      INTO es_bw_obj-text
      FROM rsdst
      WHERE langu = 'E'
        AND datasource = iv_bw_obj(30)
        AND logsys     = iv_bw_obj+30(10)
        AND objvers    = 'A'.
    ELSEIF ( lcl_infocube=>if_exists( iv_bw_obj(30) ) = 'X' ).
      es_bw_obj-id = icon_biw_info_cube.
      SELECT SINGLE txtsh
      INTO es_bw_obj-text
      FROM rsdcubet
      WHERE infocube = iv_bw_obj(30)
        AND objvers  = 'A'
        AND langu    = 'E'.
    ELSEIF ( lcl_datastore=>if_exists( iv_bw_obj(30) ) = 'X' ).
      es_bw_obj-id = icon_database_table.
      SELECT SINGLE txtsh
      INTO es_bw_obj-text
      FROM rsdodsot
      WHERE odsobject = iv_bw_obj(30)
        AND objvers   = 'A'
        AND langu     = 'E'.
    ELSEIF ( lcl_infoobject=>if_exists( iv_bw_obj(30) ) = 'X' ).
      CASE iv_bw_obj+30(10).
      WHEN 'ATTRIBUTES'.
        es_bw_obj-id = icon_master_data_act.
      WHEN 'TEXTS'.
        es_bw_obj-id = icon_text_act.
      ENDCASE.
      SELECT SINGLE txtsh
      INTO es_bw_obj-text
      FROM rsdiobjt
      WHERE iobjnm  = iv_bw_obj(30)
        AND objvers = 'A'
        AND langu   = 'E'.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* target_get_list
*---------------------------------------------------------------------*
  METHOD target_get_list.
  DATA: wa_target TYPE s_target.
  DATA: w_tgttp TYPE  rsbktgttp.

    SELECT DISTINCT tgt tgttlogo tgttp
    INTO (wa_target-tgt, wa_target-tgttlogo, w_tgttp)
    FROM rsbkdtp
    WHERE objvers = 'A'.
      CASE wa_target-tgttlogo.
      WHEN 'CUBE'.
        SELECT SINGLE txtlg
        INTO wa_target-text
        FROM rsdcubet
        WHERE infocube = wa_target-tgt
          AND objvers  = 'A'
          AND langu    = 'E'.
        wa_target-id = icon_biw_info_cube.
      WHEN 'ODSO'.
        SELECT SINGLE txtlg
        INTO wa_target-text
        FROM rsdodsot
        WHERE odsobject = wa_target-tgt
          AND objvers   = 'A'
          AND langu     = 'E'.
        wa_target-id = icon_database_table.
      WHEN 'IOBJ'.
        SELECT SINGLE txtlg
        INTO wa_target-text
        FROM rsdiobjt
        WHERE iobjnm  = wa_target-tgt
          AND objvers = 'A'
          AND langu   = 'E'.
        CASE w_tgttp.
        WHEN 'IOBJA'.
          wa_target-tgt+30(10) = 'ATTRIBUTES'.
          wa_target-id = icon_master_data_act.
        WHEN 'IOBJT'.
          wa_target-tgt+30(10) = 'TEXTS'.
          wa_target-id = icon_text_act.
        WHEN 'IOBJH'.
          wa_target-tgt+30(10) = 'HIERARCHY'.
          wa_target-id = icon_hierarchy_act.
        ENDCASE.
      ENDCASE.
      COLLECT wa_target INTO et_target.
    ENDSELECT.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_target_exists
*---------------------------------------------------------------------*
  METHOD if_target_exists.
  DATA: w_text TYPE string.
  DATA: wa_target TYPE s_target.
  DATA: infoobject TYPE REF TO lcl_infoobject.
  DATA: w_tgttp TYPE rsbktgttp.

    IF iv_target = SPACE.
      w_text = 'Target is not specified'.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( iv_target+30(10) <> SPACE        ) AND
       ( iv_target+30(10) <> 'ATTRIBUTES' ) AND
       ( iv_target+30(10) <> 'TEXTS'      ) AND
       ( iv_target+30(10) <> 'HIERARCHY'  ).
      CONCATENATE iv_target+30(10) 'Is Invalid InfoObject Subtype'
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( ( iv_target+30(10) = 'ATTRIBUTES' ) OR
         ( iv_target+30(10) = 'TEXTS' ) OR
         ( iv_target+30(10) = 'HIERARCHY' ) ) AND
       ( lcl_infoobject=>if_exists( iv_target(30) ) = SPACE ).
      CONCATENATE iv_target(30) 'InfoObject does not exists'
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( lcl_infoobject=>if_exists( iv_target(30) ) = 'X' ) AND
       ( iv_target+30(10) = SPACE ).
      CONCATENATE 'Specify Target' iv_target(30) 'InfoObject Subtype'
                  INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( lcl_infoobject=>if_exists( iv_target(30) ) = 'X' ).
      CREATE OBJECT infoobject EXPORTING iv_infoobject = iv_target(30).
      IF ( iv_target+30(10)       = 'ATTRIBUTES' ) AND
         ( infoobject->attr_exists = SPACE ).
        CONCATENATE iv_target(30)
                    'InfoObject does not have Attributes'
                    INTO w_text SEPARATED BY SPACE.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      ENDIF.
      IF ( iv_target+30(10)       = 'TEXTS' ) AND
         ( infoobject->text_exists = SPACE ).
        CONCATENATE iv_target(30)
                    'InfoObject does not have Texts'
                    INTO w_text SEPARATED BY SPACE.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      ENDIF.
    ENDIF.

    IF ( iv_target+30(10) = SPACE                          ) AND
       ( lcl_datastore=>if_exists( iv_target(30) ) = SPACE ) AND
       ( lcl_infocube=>if_exists( iv_target(30) )  = SPACE ).
      CONCATENATE iv_target(30)
                  'is not an active DataStore or Cube or InfoObject'
                  INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( lcl_datastore=>if_exists( iv_target(30) ) = 'X' ) OR
       ( lcl_infocube=>if_exists( iv_target(30) )  = 'X' ).
      SELECT tgt tgttp
      INTO (wa_target-tgt, wa_target-tgttlogo)
      FROM rsbkdtp UP TO 1 ROWS
      WHERE tgt     =  iv_target(30)
        AND tgttp   IN ('CUBE' , 'ODSO')
        AND objvers =  'A'.
      ENDSELECT.
      IF sy-subrc <> 0.
        IF ( lcl_datastore=>if_exists( iv_target(30) ) = 'X' ).
          CONCATENATE iv_target
            'DataStore is not updated by any active DTP'
            INTO w_text SEPARATED BY SPACE.
        ELSEIF ( lcl_infocube=>if_exists( iv_target(30) )  = 'X' ).
          CONCATENATE iv_target 'Cube is not updated by any active DTP'
            INTO w_text SEPARATED BY SPACE.
        ENDIF.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      ENDIF.
    ENDIF.

    IF ( lcl_infoobject=>if_exists( iv_target(30) ) = 'X' ).
      CASE iv_target+30(10).
      WHEN 'ATTRIBUTES'.
        w_tgttp = 'IOBJA'.
      WHEN 'TEXTS'.
        w_tgttp = 'IOBJT'.
      WHEN 'HIERARCHY'.
        w_tgttp = 'IOBJH'.
      ENDCASE.
      SELECT tgt tgttp
      INTO (wa_target-tgt, wa_target-tgttlogo)
      FROM rsbkdtp UP TO 1 ROWS
      WHERE tgt     = iv_target(30)
        AND tgttp   = w_tgttp
        AND objvers =  'A'.
      ENDSELECT.
      IF sy-subrc <> 0.
        CASE iv_target+30(10).
        WHEN 'ATTRIBUTES'.
          CONCATENATE iv_target(30)
            'InfoObject Attributes are not updated by any active DTP'
            INTO w_text SEPARATED BY SPACE.
        WHEN 'TEXTS'.
          CONCATENATE iv_target(30)
            'InfoObject Texts are not updated by any active DTP'
            INTO w_text SEPARATED BY SPACE.
        ENDCASE.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      ENDIF.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* select_source
*---------------------------------------------------------------------*
  METHOD select_source.
  DATA: wr_screen TYPE REF TO cl_gui_container,
        ws_awbobject1 type rsawbn_s_awbobject,
        ws_awbobject2 type rsawbn_s_awbobject,
        wt_path type rsawbn_t_awbobject.

    ws_awbobject1-objnm = iv_target(30).
    IF ( lcl_datastore=>if_exists( iv_target(30) ) = 'X' ).
      ws_awbobject1-awbobj = 'ODSO'.
    ELSEIF ( lcl_infocube=>if_exists( iv_target(30) ) = 'X' ).
      ws_awbobject1-awbobj = 'CUBE'.
    ELSEIF ( lcl_infoobject=>if_exists( iv_target(30) ) = 'X' ).
      ws_awbobject1-awbobj = 'IOBJ'.
      CASE iv_target+30(10).
      WHEN 'ATTRIBUTES'.
        ws_awbobject1-awbsubobj = 'ATTR'.
      WHEN 'TEXTS'.
        ws_awbobject1-awbsubobj = 'TEXT'.
      WHEN 'HIERARCHY'.
        ws_awbobject1-awbsubobj = 'HIER'.
      ENDCASE.
    ENDIF.
    wr_screen = cl_gui_container=>screen1.
    CALL METHOD cl_rsawbn_dtp_f4=>call_dtp_f4
         EXPORTING
              i_s_awbobject          = ws_awbobject1
              i_target_flg           = 'X'
              i_r_screen             = wr_screen
         IMPORTING
              e_t_path               = wt_path
              e_s_selected_awbobject = ws_awbobject2.
    ev_source = ws_awbobject2-objnm.
    CASE ws_awbobject2-awbsubobj.
    WHEN 'ATTR'.
      ev_source+30(10) = 'ATTRIBUTES'.
    WHEN 'TEXT'.
      ev_source+30(10) = 'TEXTS'.
    WHEN 'HIER'.
      ev_source+30(10) = 'HIERARCHY'.
    ENDCASE.
    CASE ws_awbobject2-awbsubobj.
    WHEN 'CUBE'.
      ev_source_type = icon_biw_info_cube.
    WHEN 'ODSO'.
      ev_source_type = icon_database_table.
    WHEN 'RSDS'.
      ev_source_type = icon_bw_datasource.
    WHEN 'ATTR'.
     ev_source_type  = icon_master_data_act.
    WHEN 'TEXT'.
      ev_source_type = icon_text_act.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_source_exists
*---------------------------------------------------------------------*
  METHOD if_source_exists.
  DATA: w_src TYPE rsbksrcnm.
  DATA: w_text TYPE string.
  DATA: w_srctp TYPE rsbksrctp.


    IF iv_source = SPACE.
      w_text = 'Source is not specified'.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( iv_source+30(10) = SPACE ) AND
       ( lcl_datastore=>if_exists( iv_source(30) )  = SPACE ) AND
       ( lcl_infocube=>if_exists( iv_source(30) )   = SPACE ).
      CONCATENATE iv_source 'is not an active DataStore or Cube'
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( lcl_datasource=>if_exists( iv_source ) = SPACE ) AND
       ( iv_source+30(10) <> SPACE        ) AND
       ( iv_source+30(10) <> 'ATTRIBUTES' ) AND
       ( iv_source+30(10) <> 'TEXTS'      ).
      CONCATENATE iv_source+30(10) 'Is Invalid InfoObject Subtype'
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( ( iv_source+30(10) = 'ATTRIBUTES' ) OR
         ( iv_source+30(10) = 'TEXTS' ) ) AND
       ( lcl_infoobject=>if_exists( iv_source(30) ) = SPACE ).
      CONCATENATE iv_source(30) 'InfoObject does not exists'
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( lcl_infoobject=>if_exists( iv_source(30) ) = 'X' ) AND
       ( iv_source+30(10) = SPACE ).
      CONCATENATE 'Specify Source' iv_source(30) 'InfoObject Subtype'
                  INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( lcl_datasource=>if_exists( iv_source ) = SPACE ) AND
       ( lcl_infoobject=>if_exists( iv_source(30) ) = SPACE ) AND
       ( iv_source+30(10) <> SPACE        ) AND
       ( iv_source+30(10) <> 'ATTRIBUTES' ) AND
       ( iv_source+30(10) <> 'TEXTS'      ).
      CONCATENATE iv_source 'is not an active InfoObject or DataSource'
        INTO w_text SEPARATED BY SPACE.
      RAISE EXCEPTION TYPE lcl_local_exception EXPORTING TEXT = w_text.
    ENDIF.

    IF ( lcl_datasource=>if_exists( iv_source )    = 'X' ) OR
       ( lcl_datastore=>if_exists( iv_source(30) ) = 'X' ) OR
       ( lcl_infocube=>if_exists( iv_source(30) )  = 'X' ).
      SELECT src
      INTO w_src
      FROM rsbkdtp UP TO 1 ROWS
      WHERE src     =  iv_source
        AND srctp   IN ('DTASRC', 'CUBE', 'ODSO')
        AND objvers =  'A'.
      ENDSELECT.
      IF sy-subrc <> 0.
        IF ( lcl_datasource=>if_exists( iv_source ) = 'X' ).
          CONCATENATE iv_source
            'DataSource is not a Source for any active DTP'
            INTO w_text SEPARATED BY SPACE.
        ELSEIF ( lcl_datastore=>if_exists( iv_source(30) ) = 'X' ).
          CONCATENATE iv_source
            'DataStore is not a Source in any active DTP'
            INTO w_text SEPARATED BY SPACE.
        ELSEIF ( lcl_infocube=>if_exists( iv_source(30) ) = 'X' ).
          CONCATENATE iv_source
                      'Cube is not a Source for any active DTP'
                      INTO w_text SEPARATED BY SPACE.
        ENDIF.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      ENDIF.
    ENDIF.

    IF ( lcl_infoobject=>if_exists( iv_source(30) ) = 'X' ).
      CASE iv_source+30(10).
      WHEN 'ATTRIBUTES'.
        w_srctp = 'IOBJA'.
      WHEN 'TEXTS'.
        w_srctp = 'IOBJT'.
      WHEN 'HIERARCHY'.
        w_srctp = 'IOBJH'.
      ENDCASE.
      SELECT src
      INTO w_src
      FROM rsbkdtp UP TO 1 ROWS
      WHERE src     = iv_source(30)
        AND srctp   = w_srctp
        AND objvers =  'A'.
      ENDSELECT.
      IF sy-subrc <> 0.
        CASE iv_source+30(10).
        WHEN 'ATTRIBUTES'.
          CONCATENATE iv_source(30)
            'InfoObject Attributes is not a Source for any active DTP'
            INTO w_text SEPARATED BY SPACE.
        WHEN 'TEXTS'.
          CONCATENATE iv_source(30)
            'InfoObject Texts is not a Source any active DTP'
            INTO w_text SEPARATED BY SPACE.
        ENDCASE.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = w_text.
      ENDIF.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* dtp_get_details
*---------------------------------------------------------------------*
  METHOD dtp_get_details.
  DATA: wa_dtp TYPE s_dtp.
  DATA: w_langu TYPE langu.
  DATA: w_src TYPE rsbksrcnm,
        w_tgt TYPE rsbktgtnm.

    SELECT d~dtp     dt~txtlg dt~langu
           d~updmode tt1~ddtext
           d~dtptype tt2~ddtext
    INTO (wa_dtp-name,    wa_dtp-text, w_langu,
          wa_dtp-updmode, wa_dtp-updmode_text,
          wa_dtp-dtptype, wa_dtp-dtptype_text)
    FROM  ( ( rsbkdtp AS d LEFT JOIN rsbkdtpt AS dt
                                  ON dt~dtp     = d~dtp
                                 AND dt~objvers = d~objvers )
                       INNER JOIN dd07l AS tl1
                               ON tl1~domvalue_l = d~updmode
                       INNER JOIN dd07t AS tt1
                               ON tt1~domname    = tl1~domname
                              AND tt1~as4local   = tl1~as4local
                              AND tt1~valpos     = tl1~valpos
                              AND tt1~as4vers    = tl1~as4vers )
                       INNER JOIN dd07l AS tl2
                               ON tl2~domvalue_l = d~dtptype
                       INNER JOIN dd07t AS tt2
                               ON tt2~domname    = tl2~domname
                              AND tt2~as4local   = tl2~as4local
                              AND tt2~valpos     = tl2~valpos
                              AND tt2~as4vers    = tl2~as4vers
    WHERE d~dtp          = iv_dtp
      AND d~objvers      = 'A'
      AND tl1~domname    = 'RSBKUPDMODE'
      AND tl1~as4local   = 'A'
      AND tl1~as4vers    = '0000'
      AND tt1~ddlanguage = 'E'
      AND tl2~domname    = 'RSBKDTPTYPE'
      AND tl2~as4local   = 'A'
      AND tl2~as4vers    = '0000'
      AND tt2~ddlanguage = 'E'.
      IF ( w_langu = 'E' ) OR ( w_langu = SPACE ).
        IF wa_dtp-text = SPACE.
          SELECT SINGLE src tgt
          INTO (w_src, w_tgt)
          FROM rsbkdtp
          WHERE dtp     = wa_dtp-name
            AND objvers = 'A'.
          IF sy-subrc = 0.
            CONCATENATE w_src '->' w_tgt INTO wa_dtp-text
                                          SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
        wa_dtp-id = icon_bw_dtp_active.
        es_dtp    = wa_dtp.
      ENDIF.
    ENDSELECT.
    IF sy-subrc <> 0.
      CLEAR: es_dtp.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* dtp_get_list
*---------------------------------------------------------------------*
  METHOD dtp_get_list.
  DATA: wa_dtp TYPE s_dtp.
  DATA: w_langu TYPE langu.
  DATA: w_src   TYPE rsbksrcnm,
        w_tgt   TYPE rsbktgtnm,
        w_srctp TYPE rsbksrctp,
        w_tgttp TYPE rsbktgttp.
  DATA: local_exception TYPE REF TO lcl_local_exception.

    IF ( lcl_datastore=>if_exists( iv_target(30) ) = 'X' ).
      w_tgt   = iv_target(30).
      w_tgttp = 'ODSO'.
    ELSEIF ( lcl_infocube=>if_exists( iv_target(30) )  = 'X' ).
      w_tgt   = iv_target(30).
      w_tgttp = 'CUBE'.
    ELSEIF ( lcl_infoobject=>if_exists( iv_target(30) ) = 'X' ).
      w_tgt   = iv_target(30).
      CASE iv_target+30(10).
      WHEN 'ATTRIBUTES'.
        w_tgttp = 'IOBJA'.
      WHEN 'TEXTS'.
        w_tgttp = 'IOBJT'.
      WHEN 'HIERARCHY'.
        w_tgttp = 'IOBJH'.
      ENDCASE.
    ENDIF.

    IF ( lcl_datasource=>if_exists( iv_source ) = 'X' ).
      w_src   = iv_source.
      w_srctp = 'DTASRC'.
    ELSEIF ( lcl_datastore=>if_exists( iv_source(30) ) = 'X' ).
      w_src   = iv_source(30).
      w_srctp = 'ODSO'.
    ELSEIF ( lcl_infocube=>if_exists( iv_source(30) )  = 'X' ).
      w_src   = iv_source(30).
      w_srctp = 'CUBE'.
    ELSEIF ( lcl_infoobject=>if_exists( iv_source(30) ) = 'X' ).
      w_src   = iv_source(30).
      CASE iv_source+30(10).
      WHEN 'ATTRIBUTES'.
        w_srctp = 'IOBJA'.
      WHEN 'TEXTS'.
        w_srctp = 'IOBJT'.
      WHEN 'TEXTS'.
        w_srctp = 'IOBJH'.
      ENDCASE.
    ENDIF.

    SELECT d~dtp     dt~txtlg dt~langu
           d~updmode tt1~ddtext
           d~dtptype tt2~ddtext
    INTO (wa_dtp-name,    wa_dtp-text, w_langu,
          wa_dtp-updmode, wa_dtp-updmode_text,
          wa_dtp-dtptype, wa_dtp-dtptype_text)
    FROM  ( ( rsbkdtp AS d LEFT JOIN rsbkdtpt AS dt
                                  ON dt~dtp     = d~dtp
                                 AND dt~objvers = d~objvers )
                       INNER JOIN dd07l AS tl1
                               ON tl1~domvalue_l = d~updmode
                       INNER JOIN dd07t AS tt1
                               ON tt1~domname    = tl1~domname
                              AND tt1~as4local   = tl1~as4local
                              AND tt1~valpos     = tl1~valpos
                              AND tt1~as4vers    = tl1~as4vers )
                       INNER JOIN dd07l AS tl2
                               ON tl2~domvalue_l = d~dtptype
                       INNER JOIN dd07t AS tt2
                               ON tt2~domname    = tl2~domname
                              AND tt2~as4local   = tl2~as4local
                              AND tt2~valpos     = tl2~valpos
                              AND tt2~as4vers    = tl2~as4vers
    WHERE d~objvers      = 'A'
      AND d~src          = w_src
      AND d~srctp        = w_srctp
      AND d~tgt          = w_tgt
      AND d~tgttp        = w_tgttp
      AND tl1~domname    = 'RSBKUPDMODE'
      AND tl1~as4local   = 'A'
      AND tl1~as4vers    = '0000'
      AND tt1~ddlanguage = 'E'
      AND tl2~domname    = 'RSBKDTPTYPE'
      AND tl2~as4local   = 'A'
      AND tl2~as4vers    = '0000'
      AND tt2~ddlanguage = 'E'.
      IF ( w_langu = 'E' ) OR ( w_langu = SPACE ).
        IF wa_dtp-text = SPACE.
          SELECT SINGLE src tgt
          INTO (w_src, w_tgt)
          FROM rsbkdtp
          WHERE dtp     = wa_dtp-name
            AND objvers = 'A'.
          IF sy-subrc = 0.
            CONCATENATE w_src '->' w_tgt INTO wa_dtp-text
                                          SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
*
        TRY.
          wa_dtp-filter_text = lcl_dtp=>get_filter_text( wa_dtp-name ).
          CATCH lcl_local_exception INTO local_exception.
            RAISE EXCEPTION TYPE lcl_local_exception
              EXPORTING TEXT = local_exception->local_text.
          EXIT.
        ENDTRY.
*
        APPEND wa_dtp TO et_dtp.
      ENDIF.
    ENDSELECT.
    IF sy-subrc <> 0.
      CLEAR: et_dtp[].
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_transformation
*---------------------------------------------------------------------*
  METHOD get_transformation.

    SELECT tran~tranid
    INTO rv_tranid
    FROM rstran AS tran INNER JOIN rsbkdtp AS dtp
                                ON tran~sourcetype = dtp~srctlogo
                               AND tran~sourcename = dtp~src
                               AND tran~targettype = dtp~tgttlogo
                               AND tran~targetname = dtp~tgt
    UP TO 1 ROWS
    WHERE dtp~objvers = 'A'
      AND dtp~dtp     = iv_dtp
      AND tran~objvers = 'A'.
    ENDSELECT.
    IF sy-subrc <> 0.
      CLEAR rv_tranid.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_exists
*---------------------------------------------------------------------*
  METHOD if_exists.
  DATA: w_dtp   TYPE rsbkdtpnm,
        w_src   TYPE rsbksrcnm,
        w_srctp TYPE rsbksrctp,
        w_tgt   TYPE rsbktgtnm,
        w_tgttp TYPE rsbktgttp.

    SELECT SINGLE dtp
    INTO w_dtp
    FROM rsbkdtp
    WHERE dtp     = iv_dtp
      AND objvers = 'A'.
    CASE sy-subrc.
    WHEN 0.
      rv_exists = 'X'.
    WHEN OTHERS.
      rv_exists = SPACE.
    ENDCASE.

    IF ( iv_source IS SUPPLIED ) AND ( iv_target IS SUPPLIED ).
      IF ( lcl_datastore=>if_exists( iv_target(30) ) = 'X' ).
        w_tgt  = iv_target.
        w_tgttp = 'ODSO'.
      ELSEIF ( lcl_infocube=>if_exists( iv_target(30) )  = 'X' ).
        w_tgt  = iv_target.
        w_tgttp = 'CUBE'.
      ELSEIF ( lcl_infoobject=>if_exists( iv_target(30) ) = 'X' ).
        w_tgt  = iv_target(30).
        CASE iv_target+30(10).
        WHEN 'ATTRIBUTES'.
          w_tgttp = 'IOBJA'.
        WHEN 'TEXTS'.
          w_tgttp = 'IOBJT'.
        WHEN 'HIERARCHY'.
          w_tgttp = 'IOBJH'.
        ENDCASE.
      ENDIF.
*
      IF ( lcl_datasource=>if_exists( iv_source ) = 'X' ).
        w_src  = iv_source.
        w_srctp = 'DTASRC'.
      ELSEIF ( lcl_datastore=>if_exists( iv_source(30) ) = 'X' ).
        w_src  = iv_source.
        w_srctp = 'ODSO'.
      ELSEIF ( lcl_infocube=>if_exists( iv_source(30) )  = 'X' ).
        w_src  = iv_source.
        w_srctp = 'CUBE'.
      ELSEIF ( lcl_infoobject=>if_exists( iv_source(30) ) = 'X' ).
        w_src  = iv_source(30).
        CASE iv_source+30(10).
        WHEN 'ATTRIBUTES'.
          w_srctp = 'IOBJA'.
        WHEN 'TEXTS'.
          w_srctp = 'IOBJT'.
        WHEN 'HIERARCHY'.
          w_srctp = 'IOBJH'.
        ENDCASE.
      ENDIF.
*
      SELECT SINGLE dtp
      INTO w_dtp
      FROM rsbkdtp
      WHERE dtp     = iv_dtp
        AND src     = w_src
        AND srctp   = w_srctp
        AND tgt     = w_tgt
        AND tgttp   = w_tgttp
        AND objvers = 'A'.
      CASE sy-subrc.
      WHEN 0.
        rv_exists = 'X'.
      WHEN OTHERS.
        rv_exists = SPACE.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_filter_text
*---------------------------------------------------------------------*
  METHOD get_filter_text.
  DATA: local_exception TYPE REF TO lcl_local_exception.
  DATA: dtp TYPE REF TO cl_rsbk_dtp.
  DATA: ws_range TYPE rsbk_s_range,
        wt_range TYPE rsbk_th_range.

    TRY.
      lcl_dtp=>check_authorization( iv_dtp ).
      CATCH lcl_local_exception INTO local_exception.
        RAISE EXCEPTION TYPE lcl_local_exception
          EXPORTING TEXT = local_exception->local_text.
    ENDTRY.

    dtp = cl_rsbk_dtp=>factory( iv_dtp ).
    wt_range = dtp->if_rsbk_dtp_display~get_th_range( 'X' ).
    LOOP AT wt_range INTO ws_range.
      CONDENSE ws_range-low.
      CONCATENATE rv_text
      ws_range-fieldnm '='
      ws_range-low
      INTO rv_text
      SEPARATED BY SPACE.
      CONDENSE rv_text.
      CHECK ws_range-option <> 'EQ'.
      IF NOT ws_range-high IS INITIAL.
        CONDENSE ws_range-high.
        CONCATENATE rv_text '-'
        ws_range-high
        INTO rv_text
        SEPARATED BY SPACE.
        CONDENSE rv_text.
      ENDIF.
    ENDLOOP.
    SHIFT rv_text LEFT DELETING LEADING SPACE.
    IF sy-subrc <> 0.
      rv_text = 'keine'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_datastore  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_datastore IMPLEMENTATION.
*---------------------------------------------------------------------*
* constructor
*---------------------------------------------------------------------*
  METHOD constructor.

    _datastore = iv_datastore.
    _target    = iv_target.

  ENDMETHOD.

*---------------------------------------------------------------------*
* lif_request~get_list
*---------------------------------------------------------------------*
  METHOD lif_request~get_list.
  DATA: wa_request TYPE s_request.
  DATA: w_logdpid TYPE rslogdpid.
  DATA: w_odsrnr    TYPE rsrequid,
        w_odsrnrsid TYPE numc10,
        w_partnr    TYPE numc10,
        w_requid    TYPE rsbkrequid.
  DATA: ws_status TYPE rsstatmanstatus,
        wt_status LIKE TABLE OF ws_status.
  DATA: request TYPE REF TO if_rsbk_request_general.
  DATA: wa_requid_tgt TYPE LINE OF rsbk_th_requid,
        wt_requid_tgt TYPE rsbk_th_requid.
  DATA: w_target TYPE rsawbnobjnm.
  DATA: ws_dtp         TYPE s_dtp,
        ws_infopackage TYPE s_infopackage.

    SELECT odsrnrsid odsrnr partnr
           logdpid insert_recs
    INTO (w_odsrnrsid, w_odsrnr, w_partnr,
          w_logdpid, wa_request-rcrdextr)
    FROM rsstatmanpart
    WHERE dta      = _datastore
      AND dta_type = 'ODSO'.
*     Request Number
      wa_request-rnr = w_odsrnrsid.
*     Status
      CALL FUNCTION 'RSSTATMAN_GET_STATUS'
           EXPORTING
                i_dta      = _datastore
                i_dta_type = 'ODSO'
                i_method   = 'GET_STATUS'
                i_rnr      = w_odsrnr
                i_process  = 'ODSACTIVAT'
           IMPORTING
                e_status   = wa_request-request_status
           TABLES
                e_t_status = wt_status.
*     Date & Time
      READ TABLE wt_status INTO ws_status INDEX 1.
      IF sy-subrc <> 0.
        CLEAR ws_status.
      ENDIF.
      CALL FUNCTION 'RSSM_GET_TIME'
           EXPORTING
                i_timestamps = ws_status-ts_proc_ended
           IMPORTING
                e_datum_loc  = wa_request-date
                e_uzeit_loc  = wa_request-time.
*     Datamart status
      wa_request-datamart_status = icon_action_fault.
      w_requid = w_partnr.
      request = cl_rsbk_request_general=>create_from_db_general(
                                                       w_requid ).
      wt_requid_tgt = request->get_th_requid_tgt( ).
      IF NOT wt_requid_tgt[] IS INITIAL.
        SELECT tabname
        INTO w_target
        FROM rsreqicods UP TO 1 ROWS
        FOR ALL ENTRIES IN wt_requid_tgt
        WHERE rnsidlast = wt_requid_tgt-requid
          AND tabname   = _target
          AND typ       = 'I'.
        ENDSELECT.
        IF ( sy-subrc = 0 ) AND ( wa_request-rcrdextr > 0 ).
          wa_request-datamart_status = icon_action_success.
        ENDIF.
      ENDIF.
      IF wa_request-rcrdextr = 0.
        wa_request-datamart_status = icon_failure.
      ENDIF.
*     DTP/InfoPackage description
      IF lcl_dtp=>if_exists( iv_dtp = w_logdpid ) = 'X'.
        CALL METHOD lcl_dtp=>dtp_get_details
             EXPORTING iv_dtp = w_logdpid
             IMPORTING es_dtp = ws_dtp.
        wa_request-text = ws_dtp-text.
      ENDIF.
      IF lcl_infopackage=>if_exists( iv_infopackage = w_logdpid ) = 'X'.
        CALL METHOD lcl_infopackage=>get_details
             EXPORTING iv_infopackage = w_logdpid
             IMPORTING ev_infopackage = ws_infopackage.
        wa_request-text = ws_infopackage-text.
      ENDIF.
*
      COLLECT wa_request INTO et_request.
    ENDSELECT.
  ENDMETHOD.

*---------------------------------------------------------------------*
* lif_psa~get_psa_tech_name
*---------------------------------------------------------------------*
  METHOD lif_psa~get_psa_tech_name.
  DATA: w_chglog TYPE rsodsname.
  DATA: w_odsobject TYPE rsdodsobject.

    w_odsobject = _datastore.
    CALL METHOD cl_rsd_odso=>get_tablnm
      EXPORTING  i_odsobject   = w_odsobject
      IMPORTING  e_chnglognm   = w_chglog.
    SELECT SINGLE odsname_tech
    INTO rv_psa_tech_name
    FROM rstsods
    WHERE odsname = w_chglog
      AND version = ( SELECT MAX( version )
                      FROM rstsods
                      WHERE odsname = w_chglog ).

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_exists
*---------------------------------------------------------------------*
  METHOD if_exists.
  DATA: w_odsobject TYPE rsdodsobject.

    SELECT SINGLE odsobject
    INTO w_odsobject
    FROM rsdodso
    WHERE odsobject = iv_datastore
      AND objvers   = 'A'.
    CASE sy-subrc.
    WHEN '0'.
      rv_exists = 'X'.
    WHEN OTHERS.
      rv_exists = SPACE.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_requid_in
*---------------------------------------------------------------------*
  METHOD if_requid_in.
  DATA: w_rnr       TYPE rsrequid,
        w_chavl     TYPE rschavl,
        w_odsrnrsid TYPE numc10.

    IF iv_requid IS SUPPLIED.
      CALL FUNCTION 'RRSI_SID_VAL_SINGLE_CONVERT'
           EXPORTING
                i_iobjnm = '0REQUID'
                i_sid    = iv_requid
           IMPORTING
                e_chavl  = w_chavl.
      w_rnr = w_chavl.
      SELECT SINGLE rnr
      INTO w_rnr
      FROM rsstatmanpart
      WHERE dta      = iv_datastore
        AND dta_type = 'ODSO'
        AND rnr      = w_rnr.
      CASE sy-subrc.
      WHEN 0.
        rv_requid_in = 'X'.
      WHEN OTHERS.
        rv_requid_in = SPACE.
      ENDCASE.
    ELSEIF iv_odsrnrsid IS SUPPLIED.
      SELECT odsrnrsid
      INTO w_odsrnrsid
      FROM rsstatmanpart UP TO 1 ROWS
      WHERE dta       = iv_datastore
        AND dta_type  = 'ODSO'
        AND odsrnrsid = iv_odsrnrsid.
      ENDSELECT.
      CASE sy-subrc.
      WHEN 0.
        rv_requid_in = 'X'.
      WHEN OTHERS.
        rv_requid_in = SPACE.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_infocube  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_infocube IMPLEMENTATION.
*---------------------------------------------------------------------*
* constructor
*---------------------------------------------------------------------*
  METHOD constructor.

    _infocube = iv_infocube.
    _target   = iv_target.

  ENDMETHOD.

*---------------------------------------------------------------------*
* lif_request~get_list
*---------------------------------------------------------------------*
  METHOD lif_request~get_list.
  DATA: wa_request TYPE s_request.
  DATA: w_partnr         TYPE numc10,
        w_logdpid        TYPE rslogdpid,
        w_odsrnrsid      TYPE numc10,
        w_timestamp_verb TYPE rstimestmp.
  DATA: w_requid    TYPE rsbkrequid.
  DATA: request TYPE REF TO if_rsbk_request_general.
  DATA: wa_requid_tgt TYPE LINE OF rsbk_th_requid,
        wt_requid_tgt TYPE rsbk_th_requid.
  DATA: w_target TYPE rsawbnobjnm.
  DATA: ws_dtp         TYPE s_dtp,
        ws_infopackage TYPE s_infopackage.

    SELECT status partnr
           timestamp_verb logdpid insert_recs
    INTO (wa_request-request_status, w_partnr,
          w_timestamp_verb, w_logdpid, wa_request-rcrdextr)
    FROM rsstatmanpart
    WHERE dta      = _infocube
      AND dta_type = 'CUBE'.
*     Request Number
      wa_request-rnr = w_partnr.
*     Date & Time
      CALL FUNCTION 'RSSM_GET_TIME'
           EXPORTING
                i_timestamps = w_timestamp_verb
           IMPORTING
                e_datum_loc  = wa_request-date
                e_uzeit_loc  = wa_request-time.
*     Datamart status
      wa_request-datamart_status = icon_action_fault.
      w_requid = w_partnr.
      request = cl_rsbk_request_general=>create_from_db_general(
                                                       w_requid ).
      wt_requid_tgt = request->get_th_requid_tgt( ).
      IF NOT wt_requid_tgt[] IS INITIAL.
        SELECT tabname
        INTO w_target
        FROM rsreqicods UP TO 1 ROWS
        FOR ALL ENTRIES IN wt_requid_tgt
        WHERE rnsidlast = wt_requid_tgt-requid
          AND tabname   = _target
          AND typ       = 'I'.
        ENDSELECT.
        IF ( sy-subrc = 0 ) AND ( wa_request-rcrdextr > 0 ).
          wa_request-datamart_status = icon_action_success.
        ENDIF.
      ENDIF.
      IF wa_request-rcrdextr = 0.
        wa_request-datamart_status = icon_failure.
      ENDIF.
*     DTP/InfoPackage description
      IF lcl_dtp=>if_exists( iv_dtp = w_logdpid ) = 'X'.
        CALL METHOD lcl_dtp=>dtp_get_details
             EXPORTING iv_dtp = w_logdpid
             IMPORTING es_dtp = ws_dtp.
        wa_request-text = ws_dtp-text.
      ENDIF.
      IF lcl_infopackage=>if_exists( iv_infopackage = w_logdpid ) = 'X'.
        CALL METHOD lcl_infopackage=>get_details
             EXPORTING iv_infopackage = w_logdpid
             IMPORTING ev_infopackage = ws_infopackage.
        wa_request-text = ws_infopackage-text.
      ENDIF.
*
      COLLECT wa_request INTO et_request.
    ENDSELECT.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_exists
*---------------------------------------------------------------------*
  METHOD if_exists.
  DATA: w_infocube TYPE rsinfocube.

    SELECT SINGLE infocube
    INTO w_infocube
    FROM rsdcube
    WHERE infocube = iv_infocube
      AND objvers  = 'A'.
    CASE sy-subrc.
    WHEN '0'.
      rv_exists = 'X'.
    WHEN OTHERS.
      rv_exists = SPACE.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_requid_in
*---------------------------------------------------------------------*
  METHOD if_requid_in.
  DATA: w_rnr       TYPE rsrequid,
        w_chavl     TYPE rschavl.

    CALL FUNCTION 'RRSI_SID_VAL_SINGLE_CONVERT'
         EXPORTING
              i_iobjnm = '0REQUID'
              i_sid    = iv_requid
         IMPORTING
              e_chavl  = w_chavl.
    w_rnr = w_chavl.
    SELECT SINGLE rnr
    INTO w_rnr
    FROM rsstatmanpart
    WHERE dta      = iv_infocube
      AND dta_type = 'CUBE'
      AND rnr      = w_rnr.
    CASE sy-subrc.
    WHEN 0.
      rv_requid_in = 'X'.
    WHEN OTHERS.
      rv_requid_in = SPACE.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_infoobject  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_infoobject IMPLEMENTATION.
*---------------------------------------------------------------------*
* constructor
*---------------------------------------------------------------------*
  METHOD constructor.
  DATA: ws_vobj TYPE rsd_s_viobj.

    CALL FUNCTION 'RSD_IOBJ_GET'
         EXPORTING
              i_iobjnm  = iv_infoobject
              i_objvers = 'A'
         IMPORTING
              e_s_viobj = ws_vobj.
      CASE ws_vobj-chktab.
      WHEN SPACE.
        attr_exists = SPACE.
      WHEN OTHERS.
        attr_exists = 'X'.
      ENDCASE.
      CASE ws_vobj-txttabfl.
      WHEN 1.
        text_exists = 'X'.
      WHEN 0.
        text_exists = SPACE.
      ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_exists
*---------------------------------------------------------------------*
  METHOD if_exists.
  DATA: w_iobjnm TYPE rsdiobjnm.

    SELECT SINGLE iobjnm
    INTO w_iobjnm
    FROM rsdiobj
    WHERE iobjnm  = iv_infoobject
      AND objvers = 'A'.
    CASE sy-subrc.
    WHEN '0'.
      rv_exists = 'X'.
    WHEN OTHERS.
      rv_exists = SPACE.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_requid_in
*---------------------------------------------------------------------*
  METHOD if_requid_in.
  DATA: w_dta_type TYPE rsstatmandta_type.
  DATA: w_rnr       TYPE rsrequid,
        w_chavl     TYPE rschavl.

    CASE iv_infoobject+30(10).
    WHEN 'ATTRIBUTES'.
      w_dta_type = 'FLEX_M'.
    WHEN 'TEXTS'.
      w_dta_type = 'FLEX_T'.
    ENDCASE.

    CALL FUNCTION 'RRSI_SID_VAL_SINGLE_CONVERT'
         EXPORTING
              i_iobjnm = '0REQUID'
              i_sid    = iv_requid
         IMPORTING
              e_chavl  = w_chavl.
    w_rnr = w_chavl.
    SELECT SINGLE rnr
    INTO w_rnr
    FROM rsstatmanpart
    WHERE dta      = iv_infoobject(30)
      AND dta_type = w_dta_type
      AND rnr      = w_rnr.
    CASE sy-subrc.
    WHEN 0.
      rv_requid_in = 'X'.
    WHEN OTHERS.
      rv_requid_in = SPACE.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_datasource  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_datasource IMPLEMENTATION.
*---------------------------------------------------------------------*
* constructor
*---------------------------------------------------------------------*
  METHOD constructor.

    _datasource = iv_datasource.
    _target = iv_target.
    _psa    = lcl_datasource=>get_psa( iv_datasource ).

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_psa
*---------------------------------------------------------------------*
  METHOD get_psa.
  DATA: wa_dssegt TYPE LINE OF rsds_t_rsdsseg,
        wt_dssegt TYPE rsds_t_rsdsseg.

    CALL FUNCTION 'RSDS_DATASOURCE_SINGLE_GET'
         EXPORTING
              i_datasource = iv_datasource+0(30)
              i_logsys     = iv_datasource+30(10)
         IMPORTING
              e_t_dsseg    = wt_dssegt.
    READ TABLE wt_dssegt INTO wa_dssegt INDEX 1.
    IF sy-subrc = 0.
      rv_psa = wa_dssegt-psa.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* lif_request~get_list
*---------------------------------------------------------------------*
  METHOD lif_request~get_list.
  TYPE-POOLS: icon.
  DATA: w_chavl      TYPE rschavl,
        w_rnr        TYPE rsrequnr,
        ws_rsreqdone TYPE rsreqdone.
  DATA: wa_request TYPE s_request.
  DATA: request TYPE REF TO if_rsbk_request_general.
  DATA: wa_requid_tgt TYPE LINE OF rsbk_th_requid,
        wt_requid_tgt TYPE rsbk_th_requid.
  DATA: w_target TYPE rsawbnobjnm.
  DATA: ws_infopackage TYPE s_infopackage.
  DATA: w_rqdrecord TYPE rsrqdrecrd.

    SELECT icods~rnsidlast qm~datum qm~uzeit
    INTO (wa_request-rnr, wa_request-date, wa_request-time)
    FROM rsreqicods AS icods INNER JOIN rsreqdone AS qm
                                     ON icods~rnr = qm~rnr
    WHERE tabname = _psa
      AND typ     = 'O'.
*     Date & Time
      CALL FUNCTION 'RSSM_GET_TIME'
           EXPORTING
                i_datum_utc = wa_request-date
                i_uzeit_utc = wa_request-time
           IMPORTING
                e_datum_loc = wa_request-date
                e_uzeit_loc = wa_request-time.
*     Request Status
      CALL FUNCTION 'RRSI_SID_VAL_SINGLE_CONVERT'
           EXPORTING
                i_iobjnm = '0REQUID'
                i_sid    = wa_request-rnr
           IMPORTING
                e_chavl  = w_chavl.
      w_rnr = w_chavl.
      CALL FUNCTION 'RSSM_RSREQDONE_READ'
           EXPORTING
                i_rnr         = w_rnr
           IMPORTING
                e_s_rsreqdone = ws_rsreqdone.
      IF ws_rsreqdone IS INITIAL.
        ws_rsreqdone-qmstatus = icon_red_light.
      ENDIF.
      IF   ws_rsreqdone-qmstatus(3) = icon_green_light(3) OR
         ( ws_rsreqdone-tstatus(3)  = icon_green_light(3) AND
           ws_rsreqdone-qmstatus    IS INITIAL ).
        wa_request-request_status = icon_green_light.
      ELSEIF   ws_rsreqdone-qmstatus(3) = icon_red_light(3) OR
             ( ws_rsreqdone-tstatus(3)  = icon_red_light(3)  AND
               ws_rsreqdone-qmstatus    IS INITIAL ).
        wa_request-request_status = icon_red_light.
      ELSE.
        wa_request-request_status = icon_yellow_light.
      ENDIF.
*     Datamart status
      wa_request-datamart_status = icon_action_fault.
      request = cl_rsbk_request_general=>create_from_db_general(
                                                       wa_request-rnr ).
      wt_requid_tgt = request->get_th_requid_tgt( ).
      IF NOT wt_requid_tgt[] IS INITIAL.
        SELECT tabname
        INTO w_target
        FROM rsreqicods UP TO 1 ROWS
        FOR ALL ENTRIES IN wt_requid_tgt
        WHERE rnsidlast = wt_requid_tgt-requid
          AND tabname   = _target
          AND typ       = 'I'.
        ENDSELECT.
        IF sy-subrc = 0.
          wa_request-datamart_status = icon_action_success.
        ENDIF.
      ENDIF.
*     InfoPackage Description
      CALL METHOD lcl_infopackage=>get_details
           EXPORTING iv_infopackage = ws_rsreqdone-logdpid
           IMPORTING ev_infopackage = ws_infopackage.
      wa_request-text = ws_infopackage-text.
*     Number of Records
      SELECT rqdrecord
      INTO w_rqdrecord
      FROM rsmoniptab
      WHERE rnr     = w_rnr
        AND dp_nr  <> 0
        AND rqstate = 2.
        wa_request-rcrdextr = wa_request-rcrdextr + w_rqdrecord.
      ENDSELECT.
      IF sy-subrc <> 0.
        wa_request-rcrdextr = 0.
      ENDIF.
      IF wa_request-rcrdextr = 0.
        wa_request-datamart_status = icon_failure.
      ENDIF.
*
      APPEND wa_request TO et_request.
      CLEAR: wa_request,
             w_rqdrecord.
    ENDSELECT.

  ENDMETHOD.

*---------------------------------------------------------------------*
* lif_psa~get_psa_tech_name
*---------------------------------------------------------------------*
  METHOD lif_psa~get_psa_tech_name.
  DATA: w_psa TYPE rsodsname.

    w_psa = get_psa( _datasource ).
    SELECT SINGLE odsname_tech
    INTO rv_psa_tech_name
    FROM rstsods
    WHERE odsname = w_psa
      AND version = ( SELECT MAX( version )
                      FROM rstsods
                      WHERE odsname = w_psa ).

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_exists
*---------------------------------------------------------------------*
  METHOD if_exists.
  DATA: w_datasource TYPE roosourcer,
        w_logsys     TYPE rsslogsys.
  DATA: w_new_exists TYPE rs_bool,
        w_old_exists TYPE rs_bool.

    w_datasource = iv_datasource+0(30).
    w_logsys     = iv_datasource+30(10).

    CALL FUNCTION 'RSDS_DATASOURCE_OLDNEW'
         EXPORTING
              i_datasource = w_datasource
              i_logsys     = w_logsys
         IMPORTING
              e_new_exists = w_new_exists
              e_old_exists = w_old_exists.
    IF w_new_exists = 'X' OR w_old_exists = 'X'.
      rv_exists = 'X'.
    ELSE.
      rv_exists = SPACE.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* if_requid_in
*---------------------------------------------------------------------*
  METHOD if_requid_in.
  DATA: w_rnr TYPE rsrequnr.
  DATA: w_psa TYPE rsodsname.

    rv_requid_in = SPACE.
    w_psa    = lcl_datasource=>get_psa( iv_datasource ).
    CALL FUNCTION 'RRSI_SID_VAL_SINGLE_CONVERT'
         EXPORTING
              i_iobjnm = '0REQUID'
              i_sid    = iv_requid
         IMPORTING
              e_chavl  = w_rnr.
    SELECT SINGLE rnr
    INTO w_rnr
    FROM rsreqicods
    WHERE rnr     = w_rnr
      AND tabname = w_psa
      AND typ     = 'O'.
    IF sy-subrc = 0.
      rv_requid_in = 'X'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_request  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_request IMPLEMENTATION.
*---------------------------------------------------------------------*
* if_requid_in
*---------------------------------------------------------------------*
  METHOD if_requid_in.

    IF iv_datasource IS SUPPLIED.
      rv_requid_in =
        lcl_datasource=>if_requid_in( iv_datasource = iv_datasource
                                      iv_requid     = iv_requid ).
    ENDIF.
    IF iv_datastore IS SUPPLIED.
      IF iv_requid IS SUPPLIED.
        rv_requid_in =
          lcl_datastore=>if_requid_in( iv_datastore = iv_datastore
                                       iv_requid    = iv_requid ).
      ENDIF.
      IF iv_odsrnrsid IS SUPPLIED.
        rv_requid_in =
          lcl_datastore=>if_requid_in( iv_datastore = iv_datastore
                                       iv_odsrnrsid = iv_odsrnrsid ).
      ENDIF.
    ENDIF.
    IF iv_infocube IS SUPPLIED.
      rv_requid_in =
        lcl_infocube=>if_requid_in( iv_infocube = iv_infocube
                                    iv_requid   = iv_requid ).
    ENDIF.
    IF iv_infoobject IS SUPPLIED.
      rv_requid_in =
        lcl_infoobject=>if_requid_in( iv_infoobject = iv_infoobject
                                      iv_requid     = iv_requid ).
    ENDIF.



  ENDMETHOD.

*---------------------------------------------------------------------*
* convert_2_src_requid
*---------------------------------------------------------------------*
  METHOD convert_2_src_requid.
  DATA: request TYPE REF TO cl_rsbk_request.
  DATA: wa_requid TYPE LINE OF t_requid.
  DATA: ws_range TYPE LINE OF rsbk_th_range,
        wt_range TYPE rsbk_th_range.

    request = cl_rsbk_request=>create_from_db( iv_requid ).
    wt_range = request->get_th_range( ).
    wa_requid-sign   = 'I'.
    wa_requid-option = 'EQ'.
    LOOP AT wt_range INTO ws_range WHERE fieldnm = 'REQUID'.
      wa_requid-low = ws_range-low.
      APPEND wa_requid TO rt_requid.
    ENDLOOP.

  ENDMETHOD.

*---------------------------------------------------------------------*
* convert_2_requid
*---------------------------------------------------------------------*
  METHOD convert_2_requid.
  DATA: w_odsrnrsid TYPE numc10.
  DATA: w_rnr TYPE rsrequnr.

    SELECT rnr
    INTO w_rnr
    FROM rsstatmanpart UP TO 1 ROWS
    WHERE dta       = iv_datastore
      AND dta_type  = 'ODSO'
      AND odsrnrsid = iv_odsrnrsid.
    ENDSELECT.
    CASE sy-subrc.
    WHEN 0.
      CALL FUNCTION 'RRSI_VAL_SID_SINGLE_CONVERT'
           EXPORTING
                i_iobjnm = '0REQUID'
                i_chavl  = w_rnr
           IMPORTING
                e_sid    = rv_requid.
    WHEN OTHERS.
      CLEAR: rv_requid.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* convert_2_odsrnrsid
*---------------------------------------------------------------------*
  METHOD convert_2_odsrnrsid.
  DATA: w_odsrnrsid TYPE numc10.
  DATA: w_rnr TYPE rsrequnr.

    CALL FUNCTION 'RRSI_SID_VAL_SINGLE_CONVERT'
         EXPORTING
              i_iobjnm = '0REQUID'
              i_sid    = iv_requid
         IMPORTING
              e_chavl  = w_rnr.
    SELECT SINGLE odsrnrsid
    INTO w_odsrnrsid
    FROM rsstatmanpart
    WHERE dta       = iv_datastore
      AND dta_type  = 'ODSO'
      AND rnr = w_rnr.
    CASE sy-subrc.
    WHEN 0.
      rv_odsrnrsid = w_odsrnrsid.
    WHEN OTHERS.
      CLEAR: rv_odsrnrsid.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* optimize_selection
*---------------------------------------------------------------------*
  METHOD optimize_selection.
  DATA: wa_range TYPE rsbk_s_range,
        wt_range TYPE TABLE OF rsbk_s_range.
  DATA: w_request   TYPE rsrequnr,
        w_datapakid TYPE rsdatapid.
  DATA: wt_where TYPE rsdmd_t_where.
  DATA: w_sid TYPE rsd_sid.

    wt_where = build_where( iv_psa_tech_name = iv_psa_tech_name
                            it_range         = it_range ).
    wt_range[] = it_range[].
    SELECT DISTINCT request datapakid
    INTO (w_request, w_datapakid)
    FROM (iv_psa_tech_name)
    WHERE (wt_where).
      CALL FUNCTION 'RRSI_VAL_SID_SINGLE_CONVERT'
           EXPORTING
                i_iobjnm                  = '0REQUID'
                i_chavl                   = w_request
           IMPORTING
                e_sid                     = w_sid.
      wa_range-sign   = 'I'.
      wa_range-option = 'EQ'.
      wa_range-low    = w_sid.
      SHIFT wa_range-low LEFT DELETING LEADING SPACE.
      wa_range-fieldnm = 'REQUID'.
      APPEND wa_range TO wt_range.
      IF iv_limit_datapakid = 'X'.
        wa_range-fieldnm = 'DATAPAKID'.
        wa_range-low    = w_datapakid.
        APPEND wa_range TO wt_range.
      ENDIF.
    ENDSELECT.
    IF sy-subrc = 0.
      SORT wt_range BY fieldnm low.
      DELETE ADJACENT DUPLICATES FROM wt_range.
      LOOP AT wt_range INTO wa_range.
        wa_range-posit = sy-tabix.
        MODIFY wt_range FROM wa_range TRANSPORTING posit.
      ENDLOOP.
      rt_range[] = wt_range[].
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------------------*
* build_where
*---------------------------------------------------------------------*
  METHOD build_where.
  DATA: wa_range TYPE rsbk_s_range.
  DATA: w_intlen   TYPE i.
  DATA: w_not_begin TYPE rs_bool VALUE rs_c_false.
  DATA  rs_where   TYPE REF TO cl_rs_where.
  DATA: w_name TYPE ddobjname.
  DATA: wa_dd03p TYPE dd03p,
        wt_dd03p TYPE TABLE OF dd03p.

    w_name = iv_psa_tech_name.
    CALL FUNCTION 'DDIF_TABL_GET'
         EXPORTING
              name          = w_name
         TABLES
              dd03p_tab     = wt_dd03p.
    CREATE OBJECT rs_where.
    LOOP AT it_range INTO wa_range.
      READ TABLE wt_dd03p INTO wa_dd03p
        WITH KEY fieldname = wa_range-fieldnm.
      IF sy-subrc <> 0.
        CLEAR: wa_dd03p.
      ENDIF.
      w_intlen = wa_dd03p-intlen.
      IF w_not_begin = rs_c_true.
        CALL METHOD rs_where->add_and.
      ENDIF.
      IF wa_range-option = 'EQ'.
        CALL METHOD rs_where->add_field
          EXPORTING
            i_fieldnm               = wa_range-fieldnm
            i_operator              = '='
            i_intlen                = w_intlen
            i_datatp                = wa_dd03p-datatype
            i_init_field_with_value = rs_c_true
            i_value                 = wa_range-low.
      ELSE.
        CALL METHOD rs_where->add_field_between_2values
          EXPORTING
            i_fieldnm                 = wa_range-fieldnm
            i_intlen                  = w_intlen
            i_datatp                  = wa_dd03p-datatype
            i_init_fields_with_values = rs_c_true
            i_value_low               = wa_range-low
            i_value_high              = wa_range-high.
      ENDIF.
      w_not_begin = rs_c_true.
    ENDLOOP.
    CALL METHOD rs_where->check_validity
      IMPORTING
        e_t_where = rt_where.

  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_infopackage  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_infopackage IMPLEMENTATION.
*---------------------------------------------------------------------*
* if_exists
*---------------------------------------------------------------------*
  METHOD if_exists.
  DATA: w_logdpid TYPE rslogdpid.

    SELECT SINGLE logdpid
    INTO w_logdpid
    FROM rsldpio
    WHERE logdpid = iv_infopackage
      AND objvers = 'A'.
    CASE sy-subrc.
    WHEN 0.
      rv_exists = 'X'.
    WHEN OTHERS.
      rv_exists = SPACE.
    ENDCASE.

  ENDMETHOD.

*---------------------------------------------------------------------*
* get_details
*---------------------------------------------------------------------*
  METHOD get_details.

    SELECT SINGLE logdpid text
    INTO ev_infopackage
    FROM rsldpiot
    WHERE langu   = 'E'
      AND logdpid = iv_infopackage
      AND objvers = 'A'.
    IF sy-subrc <> 0.
      CLEAR: ev_infopackage.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

************************************************************************
AT SELECTION-SCREEN OUTPUT.
************************************************************************
LOOP AT SCREEN.
  CASE screen-group1.
  WHEN 'TXT'.
    screen-display_3d = '0'.
    screen-input     = '0'.
  WHEN 'CST'.
    CASE p_outcst.
    WHEN 'X'.
      screen-input     = '1'.
      screen-invisible = '0'.
      p_cmpout = 'X'.
    WHEN SPACE.
      screen-input     = '0'.
      screen-invisible = '1'.
    ENDCASE.
  WHEN 'OP'.
    IF ( lcl_datasource=>if_exists( p_source ) = 'X' ) OR
       ( lcl_datastore=>if_exists( p_source(30) ) = 'X' ).
      screen-input     = '1'.
      IF s_requid[] IS INITIAL.
        p_sel_op = 'X'.
      ENDIF.
    ELSE.
      screen-input     = '0'.
      CLEAR p_sel_op.
    ENDIF.
  WHEN OTHERS.
    CONTINUE.
  ENDCASE.
  MODIFY SCREEN.
ENDLOOP.


************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_target.
************************************************************************
CALL METHOD lcl_application=>f4_target.


************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_source.
************************************************************************
CALL METHOD lcl_application=>f4_source.


************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dtp.
************************************************************************
CALL METHOD lcl_application=>f4_dtp.


************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_requid-low.
************************************************************************
CALL METHOD lcl_application=>f4_request.


************************************************************************
AT SELECTION-SCREEN.
************************************************************************
CASE sy-ucomm.
WHEN 'SEL_STAGING_TYPE'.
  CALL METHOD lcl_application=>swap_staging_type.
*WHEN '%025'.
WHEN 'FC01'.
  CALL METHOD: lcl_application=>check_target,
               lcl_application=>check_source,
               lcl_application=>check_dtp,
               lcl_application=>display_transform.
WHEN 'FC02'.
  CALL METHOD: lcl_application=>check_target,
               lcl_application=>manage_target.
WHEN OTHERS.
  CALL METHOD: lcl_application=>update_sel_screen.
ENDCASE.


************************************************************************
AT SELECTION-SCREEN ON p_target.
************************************************************************
CHECK ( sy-ucomm <> 'SEL_STAGING_TYPE'  ) AND
      ( sy-ucomm <> 'SEL_OUTPUT' ).
CALL METHOD lcl_application=>check_target.


************************************************************************
AT SELECTION-SCREEN ON p_source.
************************************************************************
CHECK ( sy-ucomm <> 'SEL_STAGING_TYPE'  ) AND
      ( sy-ucomm <> 'SEL_OUTPUT' ).
CALL METHOD lcl_application=>check_source.


************************************************************************
AT SELECTION-SCREEN ON p_dtp.
************************************************************************
CHECK ( sy-ucomm <> 'SEL_STAGING_TYPE'  ) AND
      ( sy-ucomm <> 'SEL_OUTPUT' ).
CALL METHOD lcl_application=>check_dtp.


************************************************************************
AT SELECTION-SCREEN ON s_requid.
************************************************************************
CHECK ( sy-ucomm <> 'SEL_STAGING_TYPE'  ) AND
      ( sy-ucomm <> 'SEL_OUTPUT' ).
CALL METHOD: lcl_application=>convert_request,
             lcl_application=>check_request.


************************************************************************
AT SELECTION-SCREEN ON p_sel_op.
************************************************************************
CHECK ( sy-ucomm <> 'SEL_STAGING_TYPE'  ) AND
      ( sy-ucomm <> 'SEL_OUTPUT' ).
CALL METHOD: lcl_application=>check_select_optim.


************************************************************************
START-OF-SELECTION.
************************************************************************
CALL METHOD lcl_application=>main( ).


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100'.

  IF main_container IS INITIAL.
    CREATE OBJECT main_container
           EXPORTING
                container_name = 'CCTRL1'.
    CREATE OBJECT splitter
           EXPORTING
                parent  = main_container
                rows    = 2
                columns = 1.
    CALL METHOD splitter->set_row_sash
         EXPORTING
              id    = 1
              type  = cl_gui_splitter_container=>type_movable
              value = cl_gui_splitter_container=>true.
    container_top    = splitter->get_container( row = 1 column = 1 ).
    container_bottom = splitter->get_container( row = 2 column = 1 ).
    CREATE OBJECT alv_grid_top
           EXPORTING
                i_parent = container_top.
    CREATE OBJECT alv_grid_bottom
           EXPORTING
                i_parent = container_bottom.

    IF p_bffr_x = SPACE.
      CALL METHOD container_top->set_visible
           EXPORTING
                visible = p_bffr_x.
      CALL METHOD splitter->set_row_height
           EXPORTING
                id = 1
                height = 0.
    ENDIF.
    IF p_bffr_t = SPACE.
      CALL METHOD container_bottom->set_visible
           EXPORTING
                visible = p_bffr_t.
      CALL METHOD splitter->set_row_height
           EXPORTING
                id = 2
                height = 0.
    ENDIF.

    CREATE OBJECT event_receiver.
    IF p_bffr_x = 'X'.
      CALL METHOD alv_grid_top->set_table_for_first_display
           EXPORTING
                is_layout        = gs_layout_x
           CHANGING
                it_outtab        = <buffer_t_x>
                it_fieldcatalog  = gt_fcat_x.
      SET HANDLER: event_receiver->handle_user_command_x
          FOR alv_grid_top,
                   event_receiver->handle_toolbar_x
          FOR alv_grid_top.
      CALL METHOD alv_grid_top->set_toolbar_interactive.
    ENDIF.
    IF p_bffr_t = 'X'.
      CASE p_cmpout.
      WHEN 'X'.
        <buffer_t_t> = <buffer_t_t_collps>.
        gt_fcat_t    = gt_fcat_t_collps.
      WHEN SPACE.
        <buffer_t_t> = <buffer_t_t_expand>.
        gt_fcat_t    = gt_fcat_t_expand.
      ENDCASE.
      CALL METHOD alv_grid_bottom->set_table_for_first_display
           EXPORTING
                is_layout        = gs_layout_t
           CHANGING
                it_outtab        = <buffer_t_t>
                it_fieldcatalog  = gt_fcat_t.
      CREATE OBJECT event_receiver.
      SET HANDLER: event_receiver->handle_user_command_t
          FOR alv_grid_bottom,
                   event_receiver->handle_toolbar_t
          FOR alv_grid_bottom.
      CALL METHOD alv_grid_bottom->set_toolbar_interactive.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CALL METHOD: alv_grid_top->free,
               alv_grid_bottom->free,
               container_bottom->free,
               container_top->free,
               splitter->free,
               main_container->free,
               cl_gui_cfw=>flush.
  SET SCREEN 0.

ENDMODULE.                 " pai_exit  INPUT
