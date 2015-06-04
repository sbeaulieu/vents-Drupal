<?php
/**
 * @file
 * rdf_link_field.ds.inc
 */

/**
 * Implements hook_ds_custom_fields_info().
 */
function rdf_link_field_ds_custom_fields_info() {
  $export = array();

  $ds_field = new stdClass();
  $ds_field->api_version = 1;
  $ds_field->field = 'rdf_link';
  $ds_field->label = 'RDF link';
  $ds_field->field_type = 5;
  $ds_field->entities = array(
    'node' => 'node',
  );
  $ds_field->ui_limit = '';
  $ds_field->properties = array(
    'code' => array(
      'value' => '<?php return l(\'View RDF\', \'node/\'.$entity->nid.\'.rdf\'); ?>',
      'format' => 'ds_code',
    ),
    'use_token' => 0,
  );
  $export['rdf_link'] = $ds_field;

  return $export;
}
