CLASS zcl_timem_userexit_default DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_timem_userexit .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_return_code,
        warning TYPE sysubrc VALUE 4,
      END OF c_return_code.

    METHODS modify_summary_author
      CHANGING
        !summary TYPE ztimem_summary
      RAISING
        zcx_timem.

    METHODS modify_summary_request
      CHANGING
        !summary TYPE ztimem_summary
      RAISING
        zcx_timem.

    METHODS build_request_imported_systems
      IMPORTING
        request       TYPE REF TO zcl_timem_request
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.



CLASS ZCL_TIMEM_USEREXIT_DEFAULT IMPLEMENTATION.


  METHOD build_request_imported_systems.
    DATA one_system TYPE string.
    DATA temp1 TYPE zcl_timem_request=>ty_t_system.
    DATA system LIKE LINE OF temp1.
    temp1 = request->get_imported_systems( ).

    LOOP AT temp1 INTO system WHERE sysid <> sy-sysid.

      one_system = |<a style="text-decoration:none" href="" title="| &&
        |Return code { system-subrc } @ { system-date DATE = USER } { system-time TIME = USER }">{ system-sysid }</a>|.
      IF system-subrc > c_return_code-warning.
        one_system = |<strike>{ one_system }</strike>|.
      ENDIF.
      result = |{ result } { one_system }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_summary_author.
    DATA line TYPE REF TO ztimem_summary_line.
    DATA temp2 TYPE syuname.
    DATA temp1 TYPE REF TO zcl_timem_author.
    summary-title = 'Contributors' ##NO_TEXT.
    summary-value_title = 'Username' ##NO_TEXT.
    summary-text1_title = 'Name' ##NO_TEXT.

    LOOP AT summary-lines REFERENCE INTO line.

      temp2 = line->value.

      CREATE OBJECT temp1 TYPE zcl_timem_author.
      line->text1 = temp1->get_name( temp2 ).
      line->value = |<a href="SAPEVENT:author?{ line->value }">{ line->value }</a>|.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_summary_request.
    DATA line TYPE REF TO ztimem_summary_line.
    DATA temp3 TYPE trkorr.
    DATA request TYPE REF TO zcl_timem_request.
    summary-title = 'Requests' ##NO_TEXT.
    summary-value_title = 'Request' ##NO_TEXT.
    summary-text1_title = 'Description' ##NO_TEXT.
    summary-text2_title = 'Systems' ##NO_TEXT.

    LOOP AT summary-lines REFERENCE INTO line.

      temp3 = line->value.

      CREATE OBJECT request TYPE zcl_timem_request EXPORTING id = temp3.
      line->text1 = request->description.
      line->text2 = build_request_imported_systems( request ).
      line->value = |<a href="SAPEVENT:request?{ line->value }">{ line->value }</a>|.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_timem_userexit~before_rendering.
    RETURN.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_asset_content.
    RETURN.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_parts.
    RETURN.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_part_list.
    RETURN.
  ENDMETHOD.


  METHOD zif_timem_userexit~modify_summary.
    CASE summary-fieldname.
      WHEN zcl_timem_consts=>fieldname-author.
        modify_summary_author( CHANGING summary = summary ).
      WHEN zcl_timem_consts=>fieldname-request.
        modify_summary_request( CHANGING summary = summary ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_timem_userexit~on_sapevent.
    RETURN.
  ENDMETHOD.
ENDCLASS.
