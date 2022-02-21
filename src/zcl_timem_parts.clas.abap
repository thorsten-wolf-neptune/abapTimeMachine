"! List of parts of an object.
CLASS zcl_timem_parts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Constructor for an object parts
    "! @parameter i_object_type | Object type
    "! @parameter i_object_name | Object name
    METHODS constructor
      IMPORTING
        !object_type TYPE ztimem_object_type
        !object_name TYPE sobj_name
      RAISING
        zcx_timem .

    "! Returns a deep structure containing all the details for all the parts.
    METHODS get_data
      RETURNING
        VALUE(result) TYPE ztimem_data
      RAISING
        zcx_timem .

    METHODS revert
      IMPORTING
        ts TYPE timestamp
      RAISING
        zcx_timem.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA object_type TYPE ztimem_object_type .
    DATA object_name TYPE sobj_name .
    DATA parts TYPE zif_timem_object=>ty_t_part_ref .
    DATA userexits TYPE REF TO zcl_timem_userexits.
    DATA options TYPE REF TO zcl_timem_options.

    "! Load all the data, creating the actual parts
    "! which will load all the versions
    "! @parameter io_counter | To keep track of progress
    METHODS load
      RAISING
        zcx_timem .

    METHODS get_stats
      IMPORTING
                !parts        TYPE ztimem_part_source_t
      RETURNING
                VALUE(result) TYPE ztimem_stats
      RAISING   zcx_timem.

    METHODS get_timestamps
      RETURNING
        VALUE(result) TYPE ztimem_timestamp_t .

    METHODS get_lines
      IMPORTING
        !parts        TYPE ztimem_part_source_t
      RETURNING
        VALUE(result) TYPE ztimem_line_t.

    METHODS get_summaries
      IMPORTING
        lines         TYPE ztimem_line_t
      RETURNING
        VALUE(result) TYPE ztimem_summary_t
      RAISING
        zcx_timem.
ENDCLASS.



CLASS ZCL_TIMEM_PARTS IMPLEMENTATION.


  METHOD constructor.
    me->object_type = object_type.
    me->object_name = object_name.
    CREATE OBJECT me->userexits.
    me->options = zcl_timem_options=>get_instance( ).
    load( ).
  ENDMETHOD.


  METHOD get_data.
    DATA temp1 TYPE ztimem_part_source_t.
    DATA temp2 LIKE LINE OF temp1.
    DATA part LIKE LINE OF parts.
    DATA t_part LIKE temp1.
    DATA temp3 TYPE ztimem_data.
    LOOP AT parts INTO part.

      temp2-name = part->name.
      temp2-type = part->vrsd_type.
      temp2-object_name = part->vrsd_name.
      temp2-lines = part->get_source( ).
      APPEND temp2 TO temp1.
    ENDLOOP.

    t_part = temp1.
    DELETE t_part WHERE lines IS INITIAL.

    " The custom fields and anything else related to the parts must be edited at this point
    " because it can affect the aggregated results (timestamps, stats and summaries)
    userexits->modify_parts( CHANGING parts = t_part ).


    temp3-name = object_name.
    temp3-type = object_type.
    temp3-version = zcl_timem_consts=>version.
    temp3-parts = t_part.
    temp3-timestamps = get_timestamps( ).
    temp3-stats = get_stats( t_part ).
    temp3-timestamp = options->timestamp.
    temp3-summaries = get_summaries( get_lines( t_part ) ).
    temp3-ignore_case = options->ignore_case.
    temp3-ignore_indentation = options->ignore_indentation.
    result = temp3.
  ENDMETHOD.


  METHOD get_lines.
    DATA temp4 TYPE ztimem_line_t.
    DATA part LIKE LINE OF parts.
    LOOP AT parts INTO part.
      APPEND line TO temp4.
    ENDLOOP.
    result = temp4.
  ENDMETHOD.


  METHOD get_stats.
    DATA temp6 TYPE REF TO zcl_timem_stats.
    CREATE OBJECT temp6 TYPE zcl_timem_stats EXPORTING lines = get_lines( parts ).
    result = temp6->stats.
  ENDMETHOD.


  METHOD get_summaries.
    DATA temp7 TYPE ztimem_summary_t.
    DATA temp1 TYPE REF TO zcl_timem_summary.
    DATA temp2 TYPE REF TO zcl_timem_summary.
    DATA temp3 TYPE REF TO zcl_timem_summary.
    DATA temp4 TYPE REF TO zcl_timem_summary.
    DATA temp5 TYPE REF TO zcl_timem_summary.
    CREATE OBJECT temp1 TYPE zcl_timem_summary EXPORTING fieldname = 'AUTHOR'.
    APPEND temp1->build( lines ) TO temp7.

    CREATE OBJECT temp2 TYPE zcl_timem_summary EXPORTING fieldname = 'REQUEST'.
    APPEND temp2->build( lines ) TO temp7.

    CREATE OBJECT temp3 TYPE zcl_timem_summary EXPORTING fieldname = 'CUSTOM1'.
    APPEND temp3->build( lines ) TO temp7.

    CREATE OBJECT temp4 TYPE zcl_timem_summary EXPORTING fieldname = 'CUSTOM2'.
    APPEND temp4->build( lines ) TO temp7.

    CREATE OBJECT temp5 TYPE zcl_timem_summary EXPORTING fieldname = 'CUSTOM3'.
    APPEND temp5->build( lines ) TO temp7.
    result = temp7.
  ENDMETHOD.


  METHOD get_timestamps.
    " Gather timestamps from all parts
    DATA part LIKE LINE OF parts.
    DATA t_timestamp_part TYPE ztimem_timestamp_t.
    DATA ts LIKE LINE OF t_timestamp_part.
    LOOP AT parts INTO part.

      t_timestamp_part = part->get_timestamps( ).

      LOOP AT t_timestamp_part INTO ts.
        COLLECT ts INTO result.
      ENDLOOP.
    ENDLOOP.
    SORT result BY table_line DESCENDING.
  ENDMETHOD.


  METHOD load.
    DATA object TYPE REF TO zif_timem_object.
    DATA temp6 TYPE REF TO zcl_timem_object_factory.
    DATA part_list TYPE ztimem_part_t.
    DATA s_part TYPE REF TO ztimem_part.
    DATA part TYPE REF TO zcl_timem_part.
    CREATE OBJECT temp6 TYPE zcl_timem_object_factory.
    object = temp6->get_instance( object_type = object_type
                                                                  object_name = object_name ).



    part_list = object->get_part_list( ).

    userexits->modify_part_list( CHANGING part_list = part_list ).


    LOOP AT part_list REFERENCE INTO s_part.
      TRY.

          CREATE OBJECT part TYPE zcl_timem_part EXPORTING name = s_part->name
                                                           vrsd_name = s_part->object_name
                                                           vrsd_type = s_part->type.
          INSERT part INTO TABLE parts.
        CATCH zcx_timem.
          " Doesn't exist? Carry on
          ASSERT 1 = 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD revert.
    DATA part LIKE LINE OF parts.
    LOOP AT parts INTO part.
      part->revert( ts ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
