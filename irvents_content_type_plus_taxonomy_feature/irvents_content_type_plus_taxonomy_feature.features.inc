<?php
/**
 * @file
 * irvents_content_type_plus_taxonomy_feature.features.inc
 */

/**
 * Implements hook_node_info().
 */
function irvents_content_type_plus_taxonomy_feature_node_info() {
  $items = array(
    'ventfield' => array(
      'name' => t('Vent Field'),
      'base' => 'node_content',
      'description' => t('Hydrothermal vent field'),
      'has_title' => '1',
      'title_label' => t('Vent Field Name ID'),
      'help' => t('Please enter a unique Name ID for the Vent Field.
The "Description" can include information on the general description of the Vent Field and information relevant to entries under other descriptive headers below. Please provide full citations in "Discovery References" and "Other References" below.'),
    ),
  );
  return $items;
}
