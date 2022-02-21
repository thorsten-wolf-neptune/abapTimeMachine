"! Factory class for the object family:
"! - Class
"! - Function group
"! - Function module
"! - Program
CLASS zcl_timem_object_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_object_type,
        program           TYPE ztimem_object_type VALUE 'PROG',
        program_includes  TYPE ztimem_object_type VALUE 'PRGI',
        class             TYPE ztimem_object_type VALUE 'CLAS',
        function_group    TYPE ztimem_object_type VALUE 'FUGR',
        function          TYPE ztimem_object_type VALUE 'FUNC',
        transport_request TYPE ztimem_object_type VALUE 'TR',
      END OF gc_object_type .

    "! Creates and returns an instance to the requested object
    "! @parameter i_object_type | Object type
    "! @parameter i_object_name | Object name
    METHODS get_instance
      IMPORTING
        !object_type  TYPE ztimem_object_type
        !object_name  TYPE sobj_name
      RETURNING
        VALUE(result) TYPE REF TO zif_timem_object
      RAISING
        zcx_timem .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_FACTORY IMPLEMENTATION.


  METHOD get_instance.
    DATA temp1 TYPE seoclsname.
    temp1 = object_name.
    DATA temp2 TYPE undefined.
    temp2 = object_name.
    DATA temp3 TYPE undefined.
    temp3 = object_name.
    DATA temp4 TYPE undefined.
    temp4 = object_name.
    DATA temp5 TYPE REF TO zcl_timem_object_prog.
    CREATE OBJECT temp5 TYPE zcl_timem_object_prog EXPORTING NAME = object_name.
    DATA temp6 TYPE REF TO zcl_timem_object_prog_includes.
    CREATE OBJECT temp6 TYPE zcl_timem_object_prog_includes EXPORTING NAME = object_name.
    DATA temp7 TYPE REF TO zcl_timem_object_clas.
    CREATE OBJECT temp7 TYPE zcl_timem_object_clas EXPORTING NAME = temp1.
    DATA temp8 TYPE REF TO zcl_timem_object_fugr.
    CREATE OBJECT temp8 TYPE zcl_timem_object_fugr EXPORTING NAME = temp2.
    DATA temp9 TYPE REF TO zcl_timem_object_func.
    CREATE OBJECT temp9 TYPE zcl_timem_object_func EXPORTING NAME = temp3.
    DATA temp10 TYPE REF TO zcl_timem_object_tr.
    CREATE OBJECT temp10 TYPE zcl_timem_object_tr ClassDefinitionNotFound.
    result = SWITCH #(
      object_type
      WHEN gc_object_type-program THEN temp5
      WHEN gc_object_type-program_includes THEN temp6
      WHEN gc_object_type-class THEN temp7
      WHEN gc_object_type-function_group THEN temp8
      WHEN gc_object_type-function THEN temp9
      WHEN gc_object_type-transport_request THEN temp10 ).
    IF result IS NOT BOUND OR NOT result->check_exists( ).
      RAISE EXCEPTION TYPE zcx_timem
        EXPORTING
          textid = zcx_timem=>object_not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
