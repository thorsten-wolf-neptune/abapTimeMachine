CLASS zcl_timem_asset_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS create_instance
    IMPORTING
      !asset_type TYPE string
      !data TYPE ztimem_data
    RETURNING
      VALUE(result) TYPE REF TO zif_timem_asset .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TIMEM_ASSET_FACTORY IMPLEMENTATION.


  METHOD create_instance.
    DATA options TYPE REF TO zcl_timem_options.
    DATA temp1 TYPE REF TO zcl_timem_asset_css.
    DATA temp2 TYPE REF TO zcl_timem_asset_html_blame.
    DATA temp3 TYPE REF TO zcl_timem_asset_html_tmachine.
    options = zcl_timem_options=>get_instance( ).

    CREATE OBJECT temp1 TYPE zcl_timem_asset_css EXPORTING theme = options->theme.

    CREATE OBJECT temp2 TYPE zcl_timem_asset_html_blame EXPORTING data = data.

    CREATE OBJECT temp3 TYPE zcl_timem_asset_html_tmachine EXPORTING data = data.
    result = SWITCH #(
      asset_type
      WHEN 'CSS' THEN temp1
      WHEN 'HTML' THEN
        SWITCH #(
          options->mode
          WHEN zcl_timem_consts=>mode-blame THEN temp2
          WHEN zcl_timem_consts=>mode-time_machine THEN temp3 ) ).
  ENDMETHOD.
ENDCLASS.
