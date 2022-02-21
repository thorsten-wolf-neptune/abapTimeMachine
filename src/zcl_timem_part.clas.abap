"! Represents a part of an object, including all the versions of that part that
"! exist in the system.
CLASS zcl_timem_part DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! Object name
    DATA name TYPE string READ-ONLY .
    "! Part type
    DATA vrsd_type TYPE versobjtyp READ-ONLY .
    "! Part name
    DATA vrsd_name TYPE versobjnam READ-ONLY .

    "! Constructs a new part
    "! @parameter name | Object name
    "! @parameter vrsd_type | Part type
    "! @parameter vrsd_name | Part object
    METHODS constructor
      IMPORTING
        !name      TYPE string
        !vrsd_type TYPE versobjtyp
        !vrsd_name TYPE versobjnam
      RAISING
        zcx_timem .

    METHODS get_timestamps
      RETURNING
        VALUE(result) TYPE ztimem_timestamp_t .

    METHODS get_source
      RETURNING
        VALUE(result) TYPE ztimem_line_t
      RAISING
        zcx_timem .

    METHODS revert
      IMPORTING
                ts TYPE timestamp
      RAISING   zcx_timem.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_t_version TYPE STANDARD TABLE OF REF TO zcl_timem_version WITH KEY table_line .

    DATA versions TYPE ty_t_version .

    METHODS load_versions
      IMPORTING
        !vrsd_type TYPE versobjtyp
        !vrsd_name TYPE versobjnam
      RAISING
        zcx_timem .

    METHODS get_version_at_timestamp
      IMPORTING
        ts            TYPE timestamp
      RETURNING
        VALUE(result) TYPE REF TO zcl_timem_version .

    METHODS get_versions_until_timestamp
      IMPORTING
        ts            TYPE timestamp
      RETURNING
        VALUE(result) TYPE ty_t_version .

    "! Calculates and returns a list of the diffed source already filled with blame
    "! details.
    METHODS get_diffed_source_with_blame
      IMPORTING
        ts            TYPE timestamp
      RETURNING
        VALUE(result) TYPE ztimem_line_t
      RAISING
        zcx_timem .

    METHODS get_source_at_timestamp
      IMPORTING
        ts            TYPE timestamp
      RETURNING
        VALUE(result) TYPE ztimem_line_t
      RAISING
        zcx_timem .
ENDCLASS.



CLASS ZCL_TIMEM_PART IMPLEMENTATION.


  METHOD  constructor.
    me->name = name.
    me->vrsd_type = vrsd_type.
    me->vrsd_name = vrsd_name.
    load_versions( vrsd_type = vrsd_type
                   vrsd_name = vrsd_name ).
  ENDMETHOD.


  METHOD get_diffed_source_with_blame.
    DATA diff TYPE REF TO zcl_timem_diff.
    DATA temp1 TYPE zcl_timem_part=>ty_t_version.
    DATA version LIKE LINE OF temp1.
    CREATE OBJECT diff TYPE zcl_timem_diff.

    temp1 = get_versions_until_timestamp( ts ).

    LOOP AT temp1 INTO version.
      result = diff->compute( lines_old = result
                              lines_new =  version->get_source( ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_source.
    DATA options TYPE REF TO zcl_timem_options.
    options = zcl_timem_options=>get_instance( ).
    result = SWITCH #(
      options->get_instance( )->mode
      WHEN zcl_timem_consts=>mode-blame THEN get_diffed_source_with_blame( options->timestamp )
      WHEN zcl_timem_consts=>mode-time_machine THEN get_source_at_timestamp( options->timestamp ) ).
  ENDMETHOD.


  METHOD get_source_at_timestamp.
    DATA version TYPE REF TO zcl_timem_version.
    version = get_version_at_timestamp( ts ).
    IF version IS BOUND.
      result = version->get_source( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_timestamps.
    DATA ts LIKE LINE OF result.
    DATA version LIKE LINE OF versions.
    LOOP AT versions INTO version.
      ts = |{ version->date }{ version->time }|.
      COLLECT ts INTO result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_versions_until_timestamp.
    DATA date TYPE d.
    DATA temp2 TYPE zcl_timem_part=>ty_t_version.
    DATA time TYPE t.
    DATA version LIKE LINE OF versions.
    CONVERT TIME STAMP ts TIME ZONE space INTO DATE date TIME time.


    LOOP AT versions INTO version.
      APPEND version TO temp2.
    ENDLOOP.
    result = temp2.
  ENDMETHOD.


  METHOD get_version_at_timestamp.
    DATA date TYPE d.
    DATA time TYPE t.
    CONVERT TIME STAMP ts TIME ZONE space INTO DATE date TIME time.
    " The last one should be the one we want
    LOOP AT versions INTO result WHERE
          table_line->date < date OR
          ( table_line->date = date AND
            table_line->time <= time ).
    ENDLOOP.
  ENDMETHOD.


  METHOD load_versions.
    DATA vrsd TYPE REF TO zcl_timem_vrsd.
    DATA temp4 TYPE zcl_timem_part=>ty_t_version.
    DATA s_vrsd LIKE LINE OF vrsd->vrsd_list.
    DATA temp1 TYPE REF TO zcl_timem_version.
    CREATE OBJECT vrsd TYPE zcl_timem_vrsd EXPORTING type = vrsd_type
                                                     name = vrsd_name.



    LOOP AT vrsd->vrsd_list INTO s_vrsd.

      CREATE OBJECT temp1 TYPE zcl_timem_version EXPORTING vrsd = s_vrsd.
      APPEND temp1 TO temp4.
    ENDLOOP.
    versions = temp4.
  ENDMETHOD.


  METHOD revert.
    DATA version TYPE REF TO zcl_timem_version.
    version = get_version_at_timestamp( ts ).
    IF version IS BOUND.
      version->retrieve( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
