<?php
/**
 * @file
 * rdf_link_field.features.inc
 */

/**
 * Implements hook_ctools_plugin_api().
 */
function rdf_link_field_ctools_plugin_api() {
  list($module, $api) = func_get_args();
  if ($module == "ds" && $api == "ds") {
    return array("version" => "1");
  }
}
