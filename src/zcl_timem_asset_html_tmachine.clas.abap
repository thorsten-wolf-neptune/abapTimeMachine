"! Renders the main HTML page from a Transformation which receives the deep
"! structure with all the required data
CLASS zcl_timem_asset_html_tmachine DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_timem_asset_factory .

  PUBLIC SECTION.

    INTERFACES zif_timem_asset .

    "! Constructor for the main HTML asset
    "! @parameter is_parts | Deep structure containing all the information that
    "! is to be rendered as HTML.
    METHODS constructor
    IMPORTING
      !data TYPE ztimem_data .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA data TYPE ztimem_data .
ENDCLASS.



CLASS ZCL_TIMEM_ASSET_HTML_TMACHINE IMPLEMENTATION.


  METHOD constructor.
    me->data = data.
  ENDMETHOD.


  METHOD zif_timem_asset~get_content.
    CALL TRANSFORMATION ztimem_html_timemachine
    SOURCE data = data
    RESULT XML result
    OPTIONS xml_header = 'NO'.
  ENDMETHOD.


  METHOD zif_timem_asset~get_subtype.
    result = 'html'.
  ENDMETHOD.


  METHOD zif_timem_asset~get_url.
    result = 'timemachine.html'.
  ENDMETHOD.
ENDCLASS.
