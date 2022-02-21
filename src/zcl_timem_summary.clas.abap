CLASS zcl_timem_summary DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !fieldname TYPE name_feld .

    METHODS build
      IMPORTING
        lines         TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ztimem_summary
      RAISING
        zcx_timem .

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_value_request,
        value   TYPE string,
        request TYPE verskorrno,
      END OF ty_s_value_request,
      ty_t_value_request TYPE SORTED TABLE OF ty_s_value_request WITH UNIQUE KEY value request.

    DATA fieldname TYPE name_feld.

    METHODS build_lines
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ztimem_summary_line_t
      RAISING
        zcx_timem .

    METHODS total_lines_count
      IMPORTING
        summary_lines TYPE ztimem_summary_line_t
      RETURNING
        VALUE(result) TYPE int2.

    METHODS build_value_request_list
      IMPORTING
        !lines        TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ty_t_value_request.

    METHODS add_line_to_summary_lines
      IMPORTING
        !line          TYPE ztimem_line
      CHANGING
        summary_lines  TYPE ztimem_summary_line_t.

    METHODS calc_missing_data
      IMPORTING
        lines          TYPE ztimem_line_t
      CHANGING
        summary_lines  TYPE ztimem_summary_line_t
      RAISING
        zcx_timem.

    METHODS request_count_for_value
      IMPORTING
                value          TYPE string
                value_requests TYPE ty_t_value_request
      RETURNING VALUE(result)  TYPE i.
ENDCLASS.



CLASS ZCL_TIMEM_SUMMARY IMPLEMENTATION.


  METHOD add_line_to_summary_lines.
    ASSIGN COMPONENT fieldname OF STRUCTURE line TO FIELD-SYMBOL(<field>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Insert if it doesn't yet exist
    IF NOT line_exists( summary_lines[ value = <field> ] ).
      DATA temp1 TYPE ztimem_summary_line.
      temp1-value = <field>.
      INSERT temp1 INTO TABLE summary_lines. "#EC CI_SUBRC
    ENDIF.

    FIELD-SYMBOLS <summary_line> TYPE ztimem_summary_line.
    READ TABLE summary_lines ASSIGNING <summary_line> WITH KEY value = <field>.
    IF sy-subrc = 0.
      <summary_line>-line_count = <summary_line>-line_count + 1.
    ENDIF.
  ENDMETHOD.


  METHOD build.
    DATA temp2 TYPE ztimem_summary.
    temp2-fieldname = fieldname.
    temp2-lines = build_lines( lines ).
    temp2-title = |{ fieldname } list|.
    temp2-value_title = fieldname.
    result = temp2.
    DATA temp3 TYPE REF TO zcl_timem_userexits.
    CREATE OBJECT temp3 TYPE zcl_timem_userexits.
    temp3->modify_summary( CHANGING summary = result ).
  ENDMETHOD.


  METHOD build_lines.
    DATA line LIKE LINE OF lines.
    LOOP AT lines INTO line.
      add_line_to_summary_lines(
        EXPORTING
          line = line
        CHANGING
          summary_lines = result ).
    ENDLOOP.

    " If we only have one line and no value was found... this summary is pointless
    DATA temp4 LIKE LINE OF result.
    READ TABLE result INDEX 1 INTO temp4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    IF lines( result ) = 1 AND temp4-value IS INITIAL.
      CLEAR result.
    ENDIF.

    calc_missing_data(
      EXPORTING
        lines = lines
      CHANGING
        summary_lines = result ).
  ENDMETHOD.


  METHOD build_value_request_list.
    DATA line LIKE LINE OF lines.
    LOOP AT lines INTO line.
      ASSIGN COMPONENT fieldname OF STRUCTURE line TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        " Store fieldname+request (it will not have duplicates because the table has a UNIQUE KEY
        DATA temp5 TYPE zcl_timem_summary=>ty_s_value_request.
        temp5-value = <field>.
        temp5-request = line-request.
        INSERT temp5 INTO TABLE result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD calc_missing_data.
    DATA total_lines TYPE int2.
    total_lines = total_lines_count( summary_lines ).
    DATA value_requests TYPE zcl_timem_summary=>ty_t_value_request.
    value_requests = build_value_request_list( lines ).
    FIELD-SYMBOLS <summary_line> LIKE LINE OF summary_lines.
    LOOP AT summary_lines ASSIGNING <summary_line>.
      <summary_line>-percentage = <summary_line>-line_count / total_lines.
      DATA temp6 TYPE string.
      temp6 = <summary_line>-value.
      <summary_line>-request_count = request_count_for_value(
        value = temp6
        value_requests = value_requests ).
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    me->fieldname = fieldname.
  ENDMETHOD.


  METHOD request_count_for_value.
    DATA temp7 TYPE int2.
    DATA x TYPE i.
    x = 0.
    DATA value_request LIKE LINE OF value_requests.
    LOOP AT value_requests INTO value_request.
      x = x + 1.
    ENDLOOP.
    temp7 = x.
    result = temp7.
  ENDMETHOD.


  METHOD total_lines_count.
    DATA temp8 TYPE int2.
    DATA x TYPE i.
    x = 0.
    DATA summary_line LIKE LINE OF summary_lines.
    LOOP AT summary_lines INTO summary_line.
      x = x + summary_line-line_count.
    ENDLOOP.
    temp8 = x.
    result = temp8.
  ENDMETHOD.
ENDCLASS.
