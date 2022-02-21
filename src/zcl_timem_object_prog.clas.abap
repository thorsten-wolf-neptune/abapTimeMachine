"! Representation of a program/include object. This object consists of a single part which
"! this class  will be able to create and return.
CLASS zcl_timem_object_prog DEFINITION
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

    DATA name TYPE sobj_name .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_PROG IMPLEMENTATION.


  METHOD constructor.
    me->name = name.
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
    DATA temp3 TYPE undefined.
    DATA temp4 TYPE undefined.
    temp3 = name.
    temp2-name = temp3.

    temp4 = name.
    temp2-object_name = temp4.
    temp2-type = 'REPS'.
    APPEND temp2 TO temp1.
    result = temp1.
  ENDMETHOD.
ENDCLASS.
