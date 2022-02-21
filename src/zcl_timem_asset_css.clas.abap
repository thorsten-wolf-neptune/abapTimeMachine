"! Renders a CSS asset which is generated from a Transformation based on
"! the provided theme name
CLASS zcl_timem_asset_css DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_timem_asset_factory .

  PUBLIC SECTION.

    INTERFACES zif_timem_asset .

    "! Constructor for a CSS asset
    "! @parameter theme | Theme name which will determine which transformation to use
    "! to generate the CSS asset
    METHODS constructor
    IMPORTING
      !theme TYPE ztimem_theme .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA theme_transformation TYPE char30.
ENDCLASS.



CLASS ZCL_TIMEM_ASSET_CSS IMPLEMENTATION.


  METHOD constructor.
    theme_transformation = |ZTIMEM_CSS_THEME_{ theme }|.
  ENDMETHOD.


  METHOD zif_timem_asset~get_content.
    DATA theme_css TYPE string.
    DATA css TYPE string.
    css = |<css/>|.

    CALL TRANSFORMATION ztimem_css_main
    SOURCE XML css
    RESULT XML result.

    CALL TRANSFORMATION (theme_transformation)
    SOURCE XML css
    RESULT XML theme_css.

    result = |{ result }{ theme_css }|.
  ENDMETHOD.


  METHOD zif_timem_asset~get_subtype.
    result = 'css'.
  ENDMETHOD.


  METHOD zif_timem_asset~get_url.
    result = 'abaptimemachine.css'.
  ENDMETHOD.
ENDCLASS.
