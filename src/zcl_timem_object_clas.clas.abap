"! Representation of a class object. It will be able to create and return a list
"! of all the parts the class is made of.
CLASS zcl_timem_object_clas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_timem_object .

    "! Constructor for the class object.
    "! @parameter i_name | Class name
    METHODS constructor
    IMPORTING
      !name TYPE seoclsname
    RAISING
      zcx_timem .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA name TYPE seoclsname .
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_CLAS IMPLEMENTATION.


  METHOD constructor.
    me->name = name.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = name
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).
    IF sy-subrc = 0.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = name.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
    " All sort of includes
    DATA temp1 TYPE ztimem_part_t.
    DATA temp2 LIKE LINE OF temp1.
    DATA temp5 TYPE undefined.
    DATA temp6 TYPE undefined.
    DATA temp7 TYPE undefined.
    DATA temp8 TYPE undefined.
    DATA temp3 TYPE ztimem_part_t.
    DATA method_include LIKE LINE OF temp9.
    DATA temp4 LIKE LINE OF temp3.
    temp2-name = 'Class pool'.

    temp5 = name.
    temp2-object_name = temp5.
    temp2-type = 'CLSD'.
    APPEND temp2 TO temp1.
    temp2-name = 'Public section'.

    temp6 = name.
    temp2-object_name = temp6.
    temp2-type = 'CPUB'.
    APPEND temp2 TO temp1.
    temp2-name = 'Protected section'.

    temp7 = name.
    temp2-object_name = temp7.
    temp2-type = 'CPRO'.
    APPEND temp2 TO temp1.
    temp2-name = 'Private section'.

    temp8 = name.
    temp2-object_name = temp8.
    temp2-type = 'CPRI'.
    APPEND temp2 TO temp1.
    temp2-name = 'Local class definition'.
    temp2-object_name = cl_oo_classname_service=>get_ccdef_name( name ).
    temp2-type = 'CDEF'.
    APPEND temp2 TO temp1.
    temp2-name = 'Local class implementation'.
    temp2-object_name = cl_oo_classname_service=>get_ccimp_name( name ).
    temp2-type = 'CINC'.
    APPEND temp2 TO temp1.
    temp2-name = 'Local macros'.
    temp2-object_name = cl_oo_classname_service=>get_ccmac_name( name ).
    temp2-type = 'CINC'.
    APPEND temp2 TO temp1.
    temp2-name = 'Local types'.
    temp2-object_name = cl_oo_classname_service=>get_cl_name( name ).
    temp2-type = 'REPS'.
    APPEND temp2 TO temp1.
    temp2-name = 'Local test classes'.
    temp2-object_name = cl_oo_classname_service=>get_ccau_name( name ).
    temp2-type = 'CINC'.
    APPEND temp2 TO temp1.
    result = temp1.

    " Class methods

    data(temp9) = cl_oo_classname_service=>get_all_method_includes( name ).

    LOOP AT temp9 INTO method_include.

      temp4-name = |{ to_lower( method_name ) }()|.
      temp4-object_name = |{ name WIDTH = 30 }{ method_name }|.
      temp4-type = 'METH'.
      APPEND temp4 TO temp3.
    ENDLOOP.
    result = temp3.
  ENDMETHOD.
ENDCLASS.
