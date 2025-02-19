CLASS zcl_timem_consts DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CONSTANTS version TYPE char10 VALUE '0.33b' ##NO_TEXT.

    CONSTANTS:
      BEGIN OF mode,
        time_machine TYPE zcl_timem_options=>ty_mode VALUE 'T',
        blame        TYPE zcl_timem_options=>ty_mode VALUE 'B',
      END OF mode .

    CONSTANTS:
      BEGIN OF fieldname,
        request TYPE name_feld VALUE 'REQUEST',
        author  TYPE name_feld VALUE 'AUTHOR',
        custom1 TYPE name_feld VALUE 'CUSTOM1',
        custom2 TYPE name_feld VALUE 'CUSTOM2',
        custom3 TYPE name_feld VALUE 'CUSTOM3',
      END OF fieldname.

    CONSTANTS:
      BEGIN OF asset_type,
        css  TYPE string VALUE 'CSS',
        html TYPE string VALUE 'HTML',
      END OF asset_type .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_CONSTS IMPLEMENTATION.
ENDCLASS.
