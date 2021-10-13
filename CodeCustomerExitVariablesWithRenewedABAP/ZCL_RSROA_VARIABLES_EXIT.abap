class ZCL_RSROA_VARIABLES_EXIT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_RSROA_VARIABLES_EXIT_BADI .
protected section.
private section.

  methods GET_KEYDATE
    returning
      value(RT_RANGE) type RSR_T_RANGESID .
  methods GET_UPDATEDATE
    returning
      value(RT_RANGE) type RSR_T_RANGESID .
  methods GET_NETDUEDATE
    importing
      !I_VNAM type RSZGLOBV-VNAM
      !I_T_VAR_RANGE type RRS0_T_VAR_RANGE
    returning
      value(RT_RANGE) type RSR_T_RANGESID .
  methods GET_NETDUEDATETEXT
    importing
      !I_VNAM type RSZGLOBV-VNAM
      !I_T_VAR_RANGE type RRS0_T_VAR_RANGE
    returning
      value(RT_RANGE) type RSR_T_RANGESID .
  methods GET_POSTINGDATE
    returning
      value(RT_RANGE) type RSR_T_RANGESID .
  methods CHECK
    importing
      !I_T_VAR_RANGE type RRS0_T_VAR_RANGE
    returning
      value(RT_RANGE) type RSR_T_RANGESID
    raising
      CX_RS_ERROR .
  methods CHECK_POSTINGDATE
    importing
      !I_T_VAR_RANGE type RRS0_T_VAR_RANGE
    returning
      value(RT_RANGE) type RSR_T_RANGESID
    raising
      CX_RS_ERROR .
ENDCLASS.



CLASS ZCL_RSROA_VARIABLES_EXIT IMPLEMENTATION.


METHOD check.

  TRY.
    check_postingdate( i_t_var_range ).
  CATCH cx_rs_error.
    RAISE EXCEPTION TYPE cx_rs_error.
  ENDTRY.
  rt_range = VALUE #( ).

ENDMETHOD.


METHOD check_postingdate.

  TRY.
    DATA(w_date_from) = CONV d( i_t_var_range[ vnam = 'POSTINGDATE' ]-low ).
    DATA(w_date_to) = CONV d( i_t_var_range[ vnam = 'POSTINGDATE' ]-high ).
    IF w_date_to - w_date_from > 365.
      CALL FUNCTION 'RRMS_MESSAGE_HANDLING'
           EXPORTING
                i_class  = 'OO'
                i_type   = 'E'
                i_number = '000'
                i_msgv1  = 'Posting Date range is more then 365 days'.
      RAISE EXCEPTION TYPE cx_rs_error.
    ENDIF.
  CATCH cx_sy_itab_line_not_found.
  ENDTRY.

ENDMETHOD.


METHOD get_keydate.

  rt_range = VALUE #( ( sign = 'I'
                        opt  = 'EQ'
                        low  = sy-datum ) ).

ENDMETHOD.


METHOD get_netduedate.

  TRY.
    rt_range =
      SWITCH #( i_vnam
                WHEN 'NETDUEDATE0'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'GE'
                                low  = i_t_var_range[ vnam = 'KEYDATE' ]-low ) )
                WHEN 'NETDUEDATE1'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'BT'
                                low  = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE1' ]-low )
                                high = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - 1 ) ) )
                WHEN 'NETDUEDATE2'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'BT'
                                low  = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE2' ]-low )
                                high = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE1' ]-low - 1 ) ) )
                WHEN 'NETDUEDATE3'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'BT'
                                low  = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE3' ]-low )
                                high = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE2' ]-low - 1 ) ) )
                WHEN 'NETDUEDATE4'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'BT'
                                low  = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE4' ]-low )
                                high = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE3' ]-low - 1 ) ) )
                WHEN 'NETDUEDATE5'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'BT'
                                low  = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE5' ]-low )
                                high = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE4' ]-low - 1 ) ) )
                WHEN 'NETDUEDATE6'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'LE'
                                low  = CONV d( CONV d( i_t_var_range[ vnam = 'KEYDATE' ]-low ) - i_t_var_range[ vnam = 'DAYSDUE5' ]-low - 1 ) ) ) ).
  CATCH cx_sy_itab_line_not_found.
  ENDTRY.

ENDMETHOD.


METHOD get_netduedatetext.

  TRY.
    rt_range =
      SWITCH #( i_vnam
                WHEN 'NETDUEDATETEXT1'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'EQ'
                                low  = |1..{ CONV i( i_t_var_range[ vnam = 'DAYSDUE1' ]-low ) }| ) )
                WHEN 'NETDUEDATETEXT2'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'EQ'
                                low  = |{ CONV i( i_t_var_range[ vnam = 'DAYSDUE1' ]-low ) + 1 }..{ CONV i( i_t_var_range[ vnam = 'DAYSDUE2' ]-low ) }| ) )
                WHEN 'NETDUEDATETEXT3'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'EQ'
                                low  = |{ CONV i( i_t_var_range[ vnam = 'DAYSDUE2' ]-low ) + 1 }..{ CONV i( i_t_var_range[ vnam = 'DAYSDUE3' ]-low ) }| ) )
                WHEN 'NETDUEDATETEXT4'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'EQ'
                                low  = |{ CONV i( i_t_var_range[ vnam = 'DAYSDUE3' ]-low ) + 1 }..{ CONV i( i_t_var_range[ vnam = 'DAYSDUE4' ]-low ) }| ) )
                WHEN 'NETDUEDATETEXT5'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'EQ'
                                low  = |{ CONV i( i_t_var_range[ vnam = 'DAYSDUE4' ]-low ) + 1 }..{ CONV i( i_t_var_range[ vnam = 'DAYSDUE5' ]-low ) }| ) )
                WHEN 'NETDUEDATETEXT6'
                THEN VALUE #( ( sign = 'I'
                                opt  = 'EQ'
                                low  = |{ CONV i( i_t_var_range[ vnam = 'DAYSDUE5' ]-low ) + 1 }..| ) ) ).
  CATCH cx_sy_itab_line_not_found.
  ENDTRY.

ENDMETHOD.


METHOD get_postingdate.

  rt_range = VALUE #( ( sign = 'I'
                        opt  = 'BT'
                        low  = cl_hrpad_date_computations=>add_months_to_date(
                                 start_date = sy-datum
                                 months     = -12 )
                        high = sy-datum ) ).

ENDMETHOD.


METHOD get_updatedate.

  DATA(w_date) = CONV d( CONV d( |{ sy-datum+0(6) }01| ) - 1 ).
  rt_range = VALUE #( ( sign = 'I'
                        opt  = 'BT'
                        low  = CONV d( |{ w_date+0(6) }01| )
                        high = w_date ) ).

ENDMETHOD.


METHOD if_rsroa_variables_exit_badi~process.

  TRY.
  c_t_range =
    COND #( WHEN NOT c_t_range[] IS INITIAL
            THEN c_t_range
            WHEN i_step = 1
            THEN COND #( WHEN i_vnam = 'KEYDATE'
                         THEN get_keydate( )
                         WHEN i_vnam = 'UPDATEDATE'
                         THEN get_updatedate( )
                         WHEN i_vnam = 'POSTINGDATE' 
                         THEN get_postingdate( ) )    
            WHEN i_step = 2
            THEN COND #( WHEN matches( val = i_vnam regex = '^NETDUEDATE[0-6]$' )
                         THEN get_netduedate( i_vnam        = i_vnam
                                              i_t_var_range = i_t_var_range )
                         WHEN matches( val = i_vnam regex = '^NETDUEDATETEXT[0-6]$' )
                         THEN get_netduedatetext( i_vnam        = i_vnam
                                                  i_t_var_range = i_t_var_range ) )
            WHEN i_step = 3
            THEN check( i_t_var_range ) ).  
  CATCH cx_rs_error.
    RAISE EXCEPTION TYPE cx_rs_error.
  ENDTRY.

ENDMETHOD.
ENDCLASS.