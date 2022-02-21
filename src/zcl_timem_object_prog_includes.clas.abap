"! Representation of a program/include object. This object consists of a single part which
"! this class  will be able to create and return.
CLASS zcl_timem_object_prog_includes DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_timem_object .

    "! Constructor for the program/include object.
    "! @parameter i_name | Program/include name
    METHODS constructor
      IMPORTING
        !name TYPE sobj_name .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_t_rpy_repo TYPE STANDARD TABLE OF rpy_repo WITH KEY inclname.

    DATA name TYPE sobj_name .

    METHODS get_includes
      RETURNING VALUE(result) TYPE ty_t_rpy_repo.
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_PROG_INCLUDES IMPLEMENTATION.


  METHOD constructor.
    me->name = name.
  ENDMETHOD.


  METHOD get_includes.
    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = name
        only_texts       = abap_true
      TABLES
        include_tab      = result
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    SELECT SINGLE name INTO name
      FROM trdir
      WHERE name   = name.
    result = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = name.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
    DATA temp1 TYPE ztimem_part_t.
    DATA temp2 LIKE LINE OF temp1.
    DATA includes TYPE zcl_timem_object_prog_includes=>ty_t_rpy_repo.
    DATA temp5 TYPE undefined.
    DATA temp6 TYPE undefined.
    DATA temp3 TYPE ztimem_part_t.
    DATA include LIKE LINE OF includes.
    DATA temp4 LIKE LINE OF temp3.
    temp5 = name.
    temp2-name = temp5.

    temp6 = name.
    temp2-object_name = temp6.
    temp2-type = 'REPS'.
    APPEND temp2 TO temp1.
    result = temp1.


    includes = get_includes( ).



    LOOP AT includes INTO include.

      temp4-name = include-title.
      temp4-object_name = include-inclname.
      temp4-type = 'REPS'.
      APPEND temp4 TO temp3.
    ENDLOOP.
    result = temp3.
  ENDMETHOD.
ENDCLASS.
