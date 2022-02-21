*"* use this source file for your ABAP unit test classes
CLASS ltcl_diff DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: o_diff TYPE REF TO zcl_timem_diff.
    METHODS:
      setup,
      changed_line FOR TESTING RAISING cx_static_check,
      ignore_case FOR TESTING RAISING cx_static_check,
      ignore_indentation FOR TESTING RAISING cx_static_check,
      empty_old FOR TESTING RAISING cx_static_check,
      empty_both FOR TESTING RAISING cx_static_check,
      empty_new FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_diff IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT o_diff.
  ENDMETHOD.


  METHOD changed_line.
    DATA temp3 TYPE ztimem_line_t.
    DATA temp4 LIKE LINE OF temp3.
    DATA temp5 LIKE LINE OF t_blame.
    DATA temp1 TYPE ztimem_line_t.
    DATA temp2 LIKE LINE OF temp1.
    DATA t_blame TYPE ztimem_line_t.
    temp4-source = 'AaA'.
    temp4-author = 'A'.
    APPEND temp4 TO temp3.


    temp2-source = 'aAa'.
    temp2-author = 'B'.
    APPEND temp2 TO temp1.

    t_blame = o_diff->compute( lines_old   = temp3
                                     lines_new   = temp1 ).

    READ TABLE t_blame INDEX 1 INTO temp5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = temp5-author
                                        exp = 'B' ).
  ENDMETHOD.


  METHOD ignore_case.
    DATA temp6 TYPE ztimem_line_t.
    DATA temp7 LIKE LINE OF temp6.
    DATA temp3 TYPE ztimem_line_t.
    DATA temp4 LIKE LINE OF temp3.
    DATA t_blame TYPE ztimem_line_t.
    DATA temp8 LIKE LINE OF t_blame.
    zcl_timem_options=>get_instance( )->set( ignore_case = abap_true ).


    temp7-source = 'AaA'.
    temp7-author = 'A'.
    APPEND temp7 TO temp6.


    temp4-source = 'aAa'.
    temp4-author = 'B'.
    APPEND temp4 TO temp3.

    t_blame = o_diff->compute( lines_old   = temp6
                                     lines_new   = temp3 ).

    READ TABLE t_blame INDEX 1 INTO temp8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = temp8-author
                                        exp = 'A' ).
  ENDMETHOD.


  METHOD ignore_indentation.
    DATA temp9 TYPE ztimem_line_t.
    DATA temp10 LIKE LINE OF temp9.
    DATA temp5 TYPE ztimem_line_t.
    DATA temp6 LIKE LINE OF temp5.
    DATA t_blame TYPE ztimem_line_t.
    DATA temp11 LIKE LINE OF t_blame.
    zcl_timem_options=>get_instance( )->set( ignore_indentation = abap_true ).


    temp10-source = '  AaA'.
    temp10-author = 'A'.
    APPEND temp10 TO temp9.


    temp6-source = '    AaA'.
    temp6-author = 'B'.
    APPEND temp6 TO temp5.

    t_blame = o_diff->compute( lines_old   = temp9
                                     lines_new   = temp5 ).

    READ TABLE t_blame INDEX 1 INTO temp11.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = temp11-author
                                        exp = 'A' ).
  ENDMETHOD.


  METHOD empty_old.
    DATA temp12 TYPE ztimem_line_t.
    DATA temp7 TYPE ztimem_line_t.
    DATA temp8 LIKE LINE OF temp7.
    DATA temp13 LIKE LINE OF t_blame.
    DATA t_blame TYPE ztimem_line_t.
    temp8-source = 'bbb'.
    temp8-author = 'B'.
    APPEND temp8 TO temp7.
    temp8-source = 'bbb'.
    temp8-author = 'B'.
    APPEND temp8 TO temp7.

    t_blame = o_diff->compute( lines_old   = temp12
                                     lines_new   = temp7 ).

    READ TABLE t_blame INDEX 1 INTO temp13.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals( act = temp13-author
                                        exp = 'B' ).
    cl_abap_unit_assert=>assert_equals( act = lines( t_blame )
                                        exp = 2 ).
  ENDMETHOD.


  METHOD empty_new.
    DATA temp14 TYPE ztimem_line_t.
    DATA temp15 LIKE LINE OF temp14.
    DATA temp9 TYPE ztimem_line_t.
    DATA t_blame TYPE ztimem_line_t.
    temp15-source = 'bbb'.
    temp15-author = 'B'.
    APPEND temp15 TO temp14.
    temp15-source = 'bbb'.
    temp15-author = 'B'.
    APPEND temp15 TO temp14.


    t_blame = o_diff->compute( lines_old   = temp14
                                     lines_new   = temp9 ).
    cl_abap_unit_assert=>assert_equals( act = lines( t_blame )
                                        exp = 0 ).
  ENDMETHOD.


  METHOD empty_both.
    DATA temp16 TYPE ztimem_line_t.
    DATA temp10 TYPE ztimem_line_t.
    DATA t_blame TYPE ztimem_line_t.
    t_blame = o_diff->compute( lines_old   = temp16
                                     lines_new   = temp10 ).
    cl_abap_unit_assert=>assert_equals( act = lines( t_blame )
                                        exp = 0 ).
  ENDMETHOD.
ENDCLASS.
