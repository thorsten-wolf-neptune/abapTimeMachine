"! Takes an object type and name, calculates the blame information for the source
"! code of all its parts and displays it as HTML.
REPORT ztimemachine.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-sel.
PARAMETERS p_otype TYPE ztimem_object_type OBLIGATORY DEFAULT 'PROG' AS LISTBOX VISIBLE LENGTH 25.
PARAMETERS p_name TYPE sobj_name OBLIGATORY. " TODO MATCHCODE OBJECT ztimem_object_name.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK filters WITH FRAME TITLE TEXT-flt.
PARAMETERS p_date TYPE datum OBLIGATORY DEFAULT sy-datum.
PARAMETERS p_time TYPE uzeit OBLIGATORY DEFAULT '235959'.
SELECTION-SCREEN END OF BLOCK filters.

SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE TEXT-mde.
PARAMETERS p_mtimem RADIOBUTTON GROUP mode USER-COMMAND mode.
PARAMETERS p_mblame RADIOBUTTON GROUP mode.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE TEXT-opt.
PARAMETERS p_iunre AS CHECKBOX.
PARAMETERS p_icase AS CHECKBOX MODIF ID bla.
PARAMETERS p_iinde AS CHECKBOX MODIF ID bla.
SELECTION-SCREEN END OF BLOCK options.
SELECTION-SCREEN END OF BLOCK mode.

SELECTION-SCREEN COMMENT /1(83) link.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
" dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.

*SELECTION-SCREEN BEGIN OF BLOCK output WITH FRAME TITLE TEXT-out.
*PARAMETERS p_theme TYPE zblame_theme AS LISTBOX VISIBLE LENGTH 15 DEFAULT 'LIGHT'.
*SELECTION-SCREEN END OF BLOCK output.

INITIALIZATION.
  IF sy-tcode CS 'BLAME'.
    p_mtimem = abap_false.
    p_mblame = abap_true.
  ENDIF.
  link = 'More details at https://github.com/abapinho/abapTimeMachine' ##NO_TEXT.
  DATA temp1 TYPE REF TO zcl_timem_dynpro.
  CREATE OBJECT temp1 TYPE zcl_timem_dynpro EXPORTING dynnr = '1001'.
  temp1->remove_toolbar( ).

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'BLA' AND p_mblame IS INITIAL.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  DATA temp2 TYPE REF TO zcl_timem_dynpro.
  CREATE OBJECT temp2 TYPE zcl_timem_dynpro EXPORTING dynnr = '1001'.
  temp2->hide_buttons( ).

START-OF-SELECTION.
  " Convert radio button to mode
  DATA(mode) = SWITCH zcl_timem_options=>ty_mode(
    p_mblame
    WHEN abap_true THEN zcl_timem_consts=>mode-blame
    ELSE zcl_timem_consts=>mode-time_machine ).

  TRY.
      DATA temp3 TYPE timestamp.
      temp3 = |{ p_date }{ p_time }|.
      zcl_timem_options=>get_instance( )->set( mode               = mode
                                               ignore_case        = p_icase
                                               ignore_indentation = p_iinde
                                               timestamp = temp3
                                               ignore_unreleased  = p_iunre ).

      DATA temp4 TYPE REF TO zcl_timem_run.
      CREATE OBJECT temp4 TYPE zcl_timem_run.
      temp4->go( object_type = p_otype
                                object_name = p_name ).

      CALL SELECTION-SCREEN 1001.
      LEAVE SCREEN.

      DATA exc TYPE REF TO zcx_timem.
    CATCH zcx_timem INTO exc.
      DATA text TYPE string.
      text = exc->get_text( ).
      MESSAGE text TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
