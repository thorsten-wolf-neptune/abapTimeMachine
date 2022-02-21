"! Representation of a function module object. It will be able to create and
"! return a list of all the parts the function module is made of.
CLASS zcl_timem_object_func DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_timem_object .

    "! Constructor for the function module object.
    "! @parameter i_name | Function module name
    METHODS constructor
      IMPORTING
        !name TYPE rs38l_fnam .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA name TYPE rs38l_fnam .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_FUNC IMPLEMENTATION.


  METHOD constructor.
    me->name = name.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = name
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
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
    temp2-type = 'FUNC'.
    APPEND temp2 TO temp1.
    result = temp1.
  ENDMETHOD.
ENDCLASS.
