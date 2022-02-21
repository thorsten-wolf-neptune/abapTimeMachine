"! Representation of a program/include object. This object consists of a single part which
"! this class  will be able to create and return.
CLASS zcl_timem_object_tr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_timem_object.

    METHODS constructor
      IMPORTING
        !id TYPE  trkorr.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA id TYPE trkorr.

    METHODS get_object_keys
      RETURNING
        VALUE(result) TYPE trwbo_t_e071
      RAISING
        zcx_timem.

    METHODS get_object
      IMPORTING
        object_key    TYPE trwbo_s_e071
      RETURNING
        VALUE(result) TYPE REF TO zif_timem_object
      RAISING
        zcx_timem.
ENDCLASS.



CLASS ZCL_TIMEM_OBJECT_TR IMPLEMENTATION.


  METHOD constructor.
    me->id = id.
  ENDMETHOD.


  METHOD get_object.
    DATA temp1 TYPE undefined.
    DATA temp2 TYPE undefined.
    temp1 = object_key-obj_name.

    temp2 = object_key-obj_name.
    DATA temp3 TYPE undefined.
    IF object_key-pgmid = 'R3TR' AND ( object_key-object = 'CLAS' OR object_key-object = 'FUGR' ) OR object_key-pgmid = 'LIMU' AND object_key-object = 'FUNC'.
      DATA uniqueErrorSpag TYPE REF TO zcl_timem_object_factory.
      CREATE OBJECT uniqueErrorSpag TYPE zcl_timem_object_factory.
      temp3 = uniqueErrorSpag->get_instance( object_type = object_key-object
                                             object_name = temp1 ).
    IF object_key-pgmid = 'LIMU' AND object_key-object = 'REPS'.
      DATA uniqueErrorSpag TYPE REF TO zcl_timem_object_factory.
      CREATE OBJECT uniqueErrorSpag TYPE zcl_timem_object_factory.
      temp3 = uniqueErrorSpag->get_instance( object_type = 'PROG'
                                             object_name = temp2 ).
    ENDIF.
    result = temp3.
  ENDMETHOD.


  METHOD get_object_keys.
    DATA request_data TYPE trwbo_request.

    request_data-h-trkorr = id.
    CALL FUNCTION 'TRINT_READ_REQUEST'
      EXPORTING
        iv_read_objs  = abap_true
      CHANGING
        cs_request    = request_data
      EXCEPTIONS
        error_occured = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_timem.
    ENDIF.
    result = request_data-objects.
    SORT result BY pgmid ASCENDING object ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM result COMPARING pgmid object obj_name.
  ENDMETHOD.


  METHOD zif_timem_object~check_exists.
    DATA temp2 TYPE REF TO zcl_timem_request.
    TRY.

        CREATE OBJECT temp2 TYPE zcl_timem_request EXPORTING id = me->id.
        temp2.
        result = abap_true.
      CATCH zcx_timem.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_timem_object~get_name.
    result = id.
  ENDMETHOD.


  METHOD zif_timem_object~get_part_list.
      DATA object TYPE REF TO zif_timem_object.
    DATA object_key LIKE LINE OF temp3.
    data(temp3) = get_object_keys( ).

    LOOP AT temp3 INTO object_key.

      object = get_object( object_key ).
      IF object IS BOUND.
        DATA temp4 TYPE ztimem_part_t.
        temp4 = object->get_part_list( ).
        DATA part LIKE LINE OF temp4.
        LOOP AT temp4 INTO part.
          part-name = |({ object->get_name( ) }) { part-name }|.
          INSERT part INTO TABLE result.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
