<?php
/**
 * @file
 * irvents_views_features.views_default.inc
 */

/**
 * Implements hook_views_default_views().
 */
function irvents_views_features_views_default_views() {
  $export = array();

  $view = new view();
  $view->name = 'export';
  $view->description = '';
  $view->tag = '';
  $view->base_table = 'node';
  $view->human_name = '';
  $view->core = 0;
  $view->api_version = '3.0';
  $view->disabled = FALSE; /* Edit this to true to make a default view disabled initially */

  /* Display: Defaults */
  $handler = $view->new_display('default', 'Defaults', 'default');
  $handler->display->display_options['use_more_always'] = FALSE;
  $handler->display->display_options['access']['type'] = 'none';
  $handler->display->display_options['cache']['type'] = 'none';
  $handler->display->display_options['query']['type'] = 'views_query';
  $handler->display->display_options['exposed_form']['type'] = 'basic';
  $handler->display->display_options['pager']['type'] = 'some';
  $handler->display->display_options['pager']['options']['items_per_page'] = 1000;
  $handler->display->display_options['style_plugin'] = 'table';
  /* Header: Global: Text area */
  $handler->display->display_options['header']['text']['id'] = 'text';
  $handler->display->display_options['header']['text']['table'] = 'views';
  $handler->display->display_options['header']['text']['field'] = 'area';
  $handler->display->display_options['header']['text']['content'] = 'We used node export in Ver. 2.0 while the database was in Drupal 6.';
  $handler->display->display_options['header']['text']['format'] = '1';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  /* Field: Content: Type */
  $handler->display->display_options['fields']['type']['id'] = 'type';
  $handler->display->display_options['fields']['type']['table'] = 'node';
  $handler->display->display_options['fields']['type']['field'] = 'type';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['export_node']['id'] = 'export_node';
  $handler->display->display_options['fields']['export_node']['table'] = 'node';
  $handler->display->display_options['fields']['export_node']['field'] = 'export_node';
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'ventfield' => 'ventfield',
  );
  $handler->display->display_options['filters']['type']['group'] = '0';
  $handler->display->display_options['filters']['type']['expose']['operator'] = FALSE;

  /* Display: Page */
  $handler = $view->new_display('page', 'Page', 'page_1');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['path'] = 'was_used_with_node_export';
  $export['export'] = $view;

  $view = new view();
  $view->name = 'migrate_20100326';
  $view->description = 'migrate_20100326';
  $view->tag = 'tw';
  $view->base_table = 'migrate_20100326';
  $view->human_name = '';
  $view->core = 0;
  $view->api_version = '3.0';
  $view->disabled = FALSE; /* Edit this to true to make a default view disabled initially */

  /* Display: migrate_20100326 */
  $handler = $view->new_display('default', 'migrate_20100326', 'default');
  $handler->display->display_options['title'] = 'Contents of migrate_20100326';
  $handler->display->display_options['use_more_always'] = FALSE;
  $handler->display->display_options['access']['type'] = 'perm';
  $handler->display->display_options['access']['perm'] = 'table wizard administration';
  $handler->display->display_options['cache']['type'] = 'none';
  $handler->display->display_options['query']['type'] = 'views_query';
  $handler->display->display_options['exposed_form']['type'] = 'basic';
  $handler->display->display_options['pager']['type'] = 'full';
  $handler->display->display_options['pager']['options']['items_per_page'] = 25;
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['grouping'] = '';
  $handler->display->display_options['style_options']['columns'] = array(
    'nameID' => 'nameID',
    'ventsites' => 'ventsites',
    'Activity' => 'Activity',
    'Ocean' => 'Ocean',
    'Region' => 'Region',
    'natjuris' => 'natjuris',
    'longitude' => 'longitude',
    'latitude' => 'latitude',
    'sitedescription' => 'sitedescription',
    'deposittype' => 'deposittype',
    'hostrock' => 'hostrock',
    'maxtemp' => 'maxtemp',
    'spreadrate' => 'spreadrate',
    'tectonicsetting' => 'tectonicsetting',
    'Biology' => 'Biology',
    'yearmeansdisc' => 'yearmeansdisc',
    'disccitations' => 'disccitations',
    'othercitations' => 'othercitations',
    'maxdepth' => 'maxdepth',
    'mindepth' => 'mindepth',
    'targetdepth' => 'targetdepth',
    'tempcat_computed' => 'tempcat_computed',
    'dec3_featureId' => 'dec3_featureId',
  );
  $handler->display->display_options['style_options']['default'] = -1;
  $handler->display->display_options['style_options']['info'] = array(
    'nameID' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'ventsites' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'Activity' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'Ocean' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'Region' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'natjuris' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'longitude' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'latitude' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'sitedescription' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'deposittype' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'hostrock' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'maxtemp' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'spreadrate' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'tectonicsetting' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'Biology' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'yearmeansdisc' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'disccitations' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'othercitations' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'maxdepth' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'mindepth' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'targetdepth' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'tempcat_computed' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'dec3_featureId' => array(
      'sortable' => 1,
      'separator' => '',
    ),
  );
  $handler->display->display_options['style_options']['sticky'] = TRUE;
  /* Header: Global: Text area */
  $handler->display->display_options['header']['text']['id'] = 'text';
  $handler->display->display_options['header']['text']['table'] = 'views';
  $handler->display->display_options['header']['text']['field'] = 'area';
  $handler->display->display_options['header']['text']['content'] = 'Note this is a remnant from the original migration from CSV file into Drupal 6 for Ver. 2.0';
  $handler->display->display_options['header']['text']['format'] = '1';
  /* No results behavior: Global: Text area */
  $handler->display->display_options['empty']['text']['id'] = 'area';
  $handler->display->display_options['empty']['text']['table'] = 'views';
  $handler->display->display_options['empty']['text']['field'] = 'area';
  $handler->display->display_options['empty']['text']['content'] = 'There are no rows in this table.';
  $handler->display->display_options['empty']['text']['format'] = '1';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['nameID']['id'] = 'nameID';
  $handler->display->display_options['fields']['nameID']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['nameID']['field'] = 'nameID';
  $handler->display->display_options['fields']['nameID']['label'] = 'nameID';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['ventsites']['id'] = 'ventsites';
  $handler->display->display_options['fields']['ventsites']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['ventsites']['field'] = 'ventsites';
  $handler->display->display_options['fields']['ventsites']['label'] = 'ventsites';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['Activity']['id'] = 'Activity';
  $handler->display->display_options['fields']['Activity']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['Activity']['field'] = 'Activity';
  $handler->display->display_options['fields']['Activity']['label'] = 'Activity';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['Ocean']['id'] = 'Ocean';
  $handler->display->display_options['fields']['Ocean']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['Ocean']['field'] = 'Ocean';
  $handler->display->display_options['fields']['Ocean']['label'] = 'Ocean';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['Region']['id'] = 'Region';
  $handler->display->display_options['fields']['Region']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['Region']['field'] = 'Region';
  $handler->display->display_options['fields']['Region']['label'] = 'Region';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['natjuris']['id'] = 'natjuris';
  $handler->display->display_options['fields']['natjuris']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['natjuris']['field'] = 'natjuris';
  $handler->display->display_options['fields']['natjuris']['label'] = 'natjuris';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['longitude']['id'] = 'longitude';
  $handler->display->display_options['fields']['longitude']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['longitude']['field'] = 'longitude';
  $handler->display->display_options['fields']['longitude']['label'] = 'longitude';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['latitude']['id'] = 'latitude';
  $handler->display->display_options['fields']['latitude']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['latitude']['field'] = 'latitude';
  $handler->display->display_options['fields']['latitude']['label'] = 'latitude';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['sitedescription']['id'] = 'sitedescription';
  $handler->display->display_options['fields']['sitedescription']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['sitedescription']['field'] = 'sitedescription';
  $handler->display->display_options['fields']['sitedescription']['label'] = 'sitedescription';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['deposittype']['id'] = 'deposittype';
  $handler->display->display_options['fields']['deposittype']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['deposittype']['field'] = 'deposittype';
  $handler->display->display_options['fields']['deposittype']['label'] = 'deposittype';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['hostrock']['id'] = 'hostrock';
  $handler->display->display_options['fields']['hostrock']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['hostrock']['field'] = 'hostrock';
  $handler->display->display_options['fields']['hostrock']['label'] = 'hostrock';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['maxtemp']['id'] = 'maxtemp';
  $handler->display->display_options['fields']['maxtemp']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['maxtemp']['field'] = 'maxtemp';
  $handler->display->display_options['fields']['maxtemp']['label'] = 'maxtemp';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['spreadrate']['id'] = 'spreadrate';
  $handler->display->display_options['fields']['spreadrate']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['spreadrate']['field'] = 'spreadrate';
  $handler->display->display_options['fields']['spreadrate']['label'] = 'spreadrate';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['tectonicsetting']['id'] = 'tectonicsetting';
  $handler->display->display_options['fields']['tectonicsetting']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['tectonicsetting']['field'] = 'tectonicsetting';
  $handler->display->display_options['fields']['tectonicsetting']['label'] = 'tectonicsetting';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['Biology']['id'] = 'Biology';
  $handler->display->display_options['fields']['Biology']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['Biology']['field'] = 'Biology';
  $handler->display->display_options['fields']['Biology']['label'] = 'Biology';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['yearmeansdisc']['id'] = 'yearmeansdisc';
  $handler->display->display_options['fields']['yearmeansdisc']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['yearmeansdisc']['field'] = 'yearmeansdisc';
  $handler->display->display_options['fields']['yearmeansdisc']['label'] = 'yearmeansdisc';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['disccitations']['id'] = 'disccitations';
  $handler->display->display_options['fields']['disccitations']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['disccitations']['field'] = 'disccitations';
  $handler->display->display_options['fields']['disccitations']['label'] = 'disccitations';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['othercitations']['id'] = 'othercitations';
  $handler->display->display_options['fields']['othercitations']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['othercitations']['field'] = 'othercitations';
  $handler->display->display_options['fields']['othercitations']['label'] = 'othercitations';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['maxdepth']['id'] = 'maxdepth';
  $handler->display->display_options['fields']['maxdepth']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['maxdepth']['field'] = 'maxdepth';
  $handler->display->display_options['fields']['maxdepth']['label'] = 'maxdepth';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['mindepth']['id'] = 'mindepth';
  $handler->display->display_options['fields']['mindepth']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['mindepth']['field'] = 'mindepth';
  $handler->display->display_options['fields']['mindepth']['label'] = 'mindepth';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['targetdepth']['id'] = 'targetdepth';
  $handler->display->display_options['fields']['targetdepth']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['targetdepth']['field'] = 'targetdepth';
  $handler->display->display_options['fields']['targetdepth']['label'] = 'targetdepth';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['tempcat_computed']['id'] = 'tempcat_computed';
  $handler->display->display_options['fields']['tempcat_computed']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['tempcat_computed']['field'] = 'tempcat_computed';
  $handler->display->display_options['fields']['tempcat_computed']['label'] = 'tempcat_computed';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['dec3_featureId']['id'] = 'dec3_featureId';
  $handler->display->display_options['fields']['dec3_featureId']['table'] = 'migrate_20100326';
  $handler->display->display_options['fields']['dec3_featureId']['field'] = 'dec3_featureId';
  $handler->display->display_options['fields']['dec3_featureId']['label'] = 'dec3_featureId';

  /* Display: Page */
  $handler = $view->new_display('page', 'Page', 'page_1');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['path'] = 'admin/content/tw/view/migrate_20100326';
  $export['migrate_20100326'] = $view;

  $view = new view();
  $view->name = 'taxonomy_term_old';
  $view->description = 'A view to emulate Drupal core\'s handling of taxonomy/term; it also emulates Views 1\'s handling by having two possible feeds.';
  $view->tag = 'default';
  $view->base_table = 'node';
  $view->human_name = '';
  $view->core = 0;
  $view->api_version = '3.0';
  $view->disabled = FALSE; /* Edit this to true to make a default view disabled initially */

  /* Display: Defaults */
  $handler = $view->new_display('default', 'Defaults', 'default');
  $handler->display->display_options['items_per_page'] = 0;
  $handler->display->display_options['use_more_always'] = FALSE;
  $handler->display->display_options['access']['type'] = 'none';
  $handler->display->display_options['cache']['type'] = 'none';
  $handler->display->display_options['query']['type'] = 'views_query';
  $handler->display->display_options['query']['options']['distinct'] = TRUE;
  $handler->display->display_options['exposed_form']['type'] = 'basic';
  $handler->display->display_options['pager']['type'] = 'full';
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['grouping'] = '';
  $handler->display->display_options['style_options']['columns'] = array(
    'title' => 'title',
    'name' => 'name',
    'name_1' => 'name_1',
    'name_2' => 'name_2',
    'field_latitude4_value' => 'field_latitude4_value',
    'field_longitude4_value' => 'field_longitude4_value',
    'field_latitude_value' => 'field_latitude_value',
    'field_longitude_value' => 'field_longitude_value',
    'field_depth_value' => 'field_depth_value',
    'field_yr_mns_discovered_value' => 'field_yr_mns_discovered_value',
  );
  $handler->display->display_options['style_options']['default'] = 'title';
  $handler->display->display_options['style_options']['info'] = array(
    'title' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'name' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'name_1' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'name_2' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_latitude4_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_longitude4_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_latitude_value' => array(
      'separator' => '',
    ),
    'field_longitude_value' => array(
      'separator' => '',
    ),
    'field_depth_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_yr_mns_discovered_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
  );
  /* Header: Global: Text area */
  $handler->display->display_options['header']['text']['id'] = 'area';
  $handler->display->display_options['header']['text']['table'] = 'views';
  $handler->display->display_options['header']['text']['field'] = 'area';
  $handler->display->display_options['header']['text']['content'] = 'Below is the <strong>subset</strong> of Vent Fields that contain the Activity, Tectonic Setting, Region, Ocean, or National Jurisdiction listed above.
You can sort the list below by clicking on a column header.';
  $handler->display->display_options['header']['text']['format'] = '1';
  /* Relationship: Broken/missing handler */
  $handler->display->display_options['relationships']['field_activity_value']['id'] = 'field_activity_value';
  $handler->display->display_options['relationships']['field_activity_value']['table'] = 'node_data_field_activity';
  $handler->display->display_options['relationships']['field_activity_value']['field'] = 'field_activity_value';
  $handler->display->display_options['relationships']['field_activity_value']['label'] = 'Activity';
  /* Relationship: Broken/missing handler */
  $handler->display->display_options['relationships']['field_region_value']['id'] = 'field_region_value';
  $handler->display->display_options['relationships']['field_region_value']['table'] = 'node_data_field_region';
  $handler->display->display_options['relationships']['field_region_value']['field'] = 'field_region_value';
  $handler->display->display_options['relationships']['field_region_value']['label'] = 'Region';
  /* Relationship: Broken/missing handler */
  $handler->display->display_options['relationships']['field_tectonic_setting_value']['id'] = 'field_tectonic_setting_value';
  $handler->display->display_options['relationships']['field_tectonic_setting_value']['table'] = 'node_data_field_tectonic_setting';
  $handler->display->display_options['relationships']['field_tectonic_setting_value']['field'] = 'field_tectonic_setting_value';
  $handler->display->display_options['relationships']['field_tectonic_setting_value']['label'] = 'Tectonic Setting';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Vent Field Name ID';
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'field_activity_value';
  $handler->display->display_options['fields']['name']['label'] = 'Activity';
  $handler->display->display_options['fields']['name']['link_to_taxonomy'] = TRUE;
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name_1']['id'] = 'name_1';
  $handler->display->display_options['fields']['name_1']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name_1']['field'] = 'name';
  $handler->display->display_options['fields']['name_1']['relationship'] = 'field_tectonic_setting_value';
  $handler->display->display_options['fields']['name_1']['label'] = 'Tectonic Setting';
  $handler->display->display_options['fields']['name_1']['link_to_taxonomy'] = TRUE;
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name_2']['id'] = 'name_2';
  $handler->display->display_options['fields']['name_2']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name_2']['field'] = 'name';
  $handler->display->display_options['fields']['name_2']['relationship'] = 'field_region_value';
  $handler->display->display_options['fields']['name_2']['label'] = 'Region';
  $handler->display->display_options['fields']['name_2']['link_to_taxonomy'] = TRUE;
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['field_latitude4_value']['id'] = 'field_latitude4_value';
  $handler->display->display_options['fields']['field_latitude4_value']['table'] = 'node_data_field_latitude4';
  $handler->display->display_options['fields']['field_latitude4_value']['field'] = 'field_latitude4_value';
  $handler->display->display_options['fields']['field_latitude4_value']['label'] = 'Latitude';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['field_longitude4_value']['id'] = 'field_longitude4_value';
  $handler->display->display_options['fields']['field_longitude4_value']['table'] = 'node_data_field_longitude4';
  $handler->display->display_options['fields']['field_longitude4_value']['field'] = 'field_longitude4_value';
  $handler->display->display_options['fields']['field_longitude4_value']['label'] = 'Longitude';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['field_depth_value']['id'] = 'field_depth_value';
  $handler->display->display_options['fields']['field_depth_value']['table'] = 'node_data_field_depth';
  $handler->display->display_options['fields']['field_depth_value']['field'] = 'field_depth_value';
  $handler->display->display_options['fields']['field_depth_value']['label'] = 'Maximum or Single Reported Depth';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['field_yr_mns_discovered_value']['id'] = 'field_yr_mns_discovered_value';
  $handler->display->display_options['fields']['field_yr_mns_discovered_value']['table'] = 'node_data_field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered_value']['field'] = 'field_yr_mns_discovered_value';
  $handler->display->display_options['fields']['field_yr_mns_discovered_value']['label'] = 'Year and How Discovered';
  /* Sort criterion: Content: Sticky */
  $handler->display->display_options['sorts']['sticky']['id'] = 'sticky';
  $handler->display->display_options['sorts']['sticky']['table'] = 'node';
  $handler->display->display_options['sorts']['sticky']['field'] = 'sticky';
  $handler->display->display_options['sorts']['sticky']['order'] = 'DESC';
  /* Sort criterion: Content: Post date */
  $handler->display->display_options['sorts']['created']['id'] = 'created';
  $handler->display->display_options['sorts']['created']['table'] = 'node';
  $handler->display->display_options['sorts']['created']['field'] = 'created';
  $handler->display->display_options['sorts']['created']['order'] = 'DESC';
  /* Contextual filter: Content: Has taxonomy term ID (with depth) */
  $handler->display->display_options['arguments']['term_node_tid_depth']['id'] = 'term_node_tid_depth';
  $handler->display->display_options['arguments']['term_node_tid_depth']['table'] = 'node';
  $handler->display->display_options['arguments']['term_node_tid_depth']['field'] = 'term_node_tid_depth';
  $handler->display->display_options['arguments']['term_node_tid_depth']['default_action'] = 'not found';
  $handler->display->display_options['arguments']['term_node_tid_depth']['exception']['title_enable'] = TRUE;
  $handler->display->display_options['arguments']['term_node_tid_depth']['title_enable'] = TRUE;
  $handler->display->display_options['arguments']['term_node_tid_depth']['title'] = '%1';
  $handler->display->display_options['arguments']['term_node_tid_depth']['default_argument_type'] = 'fixed';
  $handler->display->display_options['arguments']['term_node_tid_depth']['summary']['format'] = 'default_summary';
  $handler->display->display_options['arguments']['term_node_tid_depth']['specify_validation'] = TRUE;
  $handler->display->display_options['arguments']['term_node_tid_depth']['validate']['type'] = 'taxonomy_term';
  $handler->display->display_options['arguments']['term_node_tid_depth']['depth'] = '0';
  $handler->display->display_options['arguments']['term_node_tid_depth']['break_phrase'] = TRUE;
  /* Contextual filter: Content: Has taxonomy term ID depth modifier */
  $handler->display->display_options['arguments']['term_node_tid_depth_modifier']['id'] = 'term_node_tid_depth_modifier';
  $handler->display->display_options['arguments']['term_node_tid_depth_modifier']['table'] = 'node';
  $handler->display->display_options['arguments']['term_node_tid_depth_modifier']['field'] = 'term_node_tid_depth_modifier';
  $handler->display->display_options['arguments']['term_node_tid_depth_modifier']['exception']['title_enable'] = TRUE;
  $handler->display->display_options['arguments']['term_node_tid_depth_modifier']['default_argument_type'] = 'fixed';
  $handler->display->display_options['arguments']['term_node_tid_depth_modifier']['summary']['format'] = 'default_summary';
  $handler->display->display_options['arguments']['term_node_tid_depth_modifier']['specify_validation'] = TRUE;
  /* Filter criterion: Content: Published or admin */
  $handler->display->display_options['filters']['status_extra']['id'] = 'status_extra';
  $handler->display->display_options['filters']['status_extra']['table'] = 'node';
  $handler->display->display_options['filters']['status_extra']['field'] = 'status_extra';
  $handler->display->display_options['filters']['status_extra']['group'] = 0;
  $handler->display->display_options['filters']['status_extra']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';

  /* Display: Page */
  $handler = $view->new_display('page', 'Page', 'page');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['path'] = 'taxonomy_old/term/%';

  /* Display: Core feed */
  $handler = $view->new_display('feed', 'Core feed', 'feed');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['pager']['type'] = 'full';
  $handler->display->display_options['pager']['options']['items_per_page'] = 15;
  $handler->display->display_options['style_plugin'] = 'rss';
  $handler->display->display_options['row_plugin'] = 'node_rss';
  $handler->display->display_options['path'] = 'taxonomy/term/%/%/feed';
  $handler->display->display_options['displays'] = array(
    'page' => 'page',
    'default' => 0,
  );

  /* Display: Views 1 feed */
  $handler = $view->new_display('feed', 'Views 1 feed', 'feed_1');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['pager']['type'] = 'full';
  $handler->display->display_options['pager']['options']['items_per_page'] = 15;
  $handler->display->display_options['style_plugin'] = 'rss';
  $handler->display->display_options['row_plugin'] = 'node_rss';
  $handler->display->display_options['path'] = 'taxonomy/term/%/feed';
  $export['taxonomy_term_old'] = $view;

  $view = new view();
  $view->name = 'vent_fields';
  $view->description = 'Vent Fields';
  $view->tag = '';
  $view->base_table = 'node';
  $view->human_name = '';
  $view->core = 0;
  $view->api_version = '3.0';
  $view->disabled = FALSE; /* Edit this to true to make a default view disabled initially */

  /* Display: Defaults */
  $handler = $view->new_display('default', 'Defaults', 'default');
  $handler->display->display_options['title'] = 'Not being used in Ver. 3.0';
  $handler->display->display_options['use_more_always'] = FALSE;
  $handler->display->display_options['access']['type'] = 'none';
  $handler->display->display_options['cache']['type'] = 'none';
  $handler->display->display_options['query']['type'] = 'views_query';
  $handler->display->display_options['query']['options']['distinct'] = TRUE;
  $handler->display->display_options['exposed_form']['type'] = 'basic';
  $handler->display->display_options['pager']['type'] = 'full';
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['grouping'] = '';
  $handler->display->display_options['style_options']['columns'] = array(
    'title' => 'title',
    'field_ventsite_txt_value' => 'field_ventsite_txt_value',
    'field_activity_value' => 'field_activity_value',
    'field_max_depth_value' => 'field_max_depth_value',
    'field_longitude_value' => 'field_longitude_value',
    'field_latitude_value' => 'field_latitude_value',
    'field_max_temp_cat_value' => 'field_max_temp_cat_value',
    'field_max_temp_value' => 'field_max_temp_value',
  );
  $handler->display->display_options['style_options']['default'] = 'title';
  $handler->display->display_options['style_options']['info'] = array(
    'title' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_ventsite_txt_value' => array(
      'separator' => '',
    ),
    'field_activity_value' => array(
      'separator' => '',
    ),
    'field_max_depth_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_longitude_value' => array(
      'sortable' => 0,
      'separator' => '',
    ),
    'field_latitude_value' => array(
      'sortable' => 0,
      'separator' => '',
    ),
    'field_max_temp_cat_value' => array(
      'separator' => '',
    ),
    'field_max_temp_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
  );
  $handler->display->display_options['style_options']['override'] = FALSE;
  /* Header: Global: Text area */
  $handler->display->display_options['header']['text']['id'] = 'area';
  $handler->display->display_options['header']['text']['table'] = 'views';
  $handler->display->display_options['header']['text']['field'] = 'area';
  $handler->display->display_options['header']['text']['content'] = 'Welcome to the InterRidge Global Database of Active Submarine Hydrothermal Vent Fields (the <strong>“InterRidge Vents Database”</strong>).
To sort the list of Vent Fields, click on a column header.
To view details for a specific Vent Field, click on the Name ID.
To view the subset of Vent Fields that contain a specific category, click on the category (for example, "active" in the "Activity" column).';
  $handler->display->display_options['header']['text']['format'] = '1';
  /* Footer: Global: Text area */
  $handler->display->display_options['footer']['text']['id'] = 'area';
  $handler->display->display_options['footer']['text']['table'] = 'views';
  $handler->display->display_options['footer']['text']['field'] = 'area';
  $handler->display->display_options['footer']['text']['content'] = 'To display the full list of Vent Fields, and for export to a CSV file, please click <a href="/ventfields_list_all">HERE</a>.';
  $handler->display->display_options['footer']['text']['format'] = '1';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name ID';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['field_name_alias_value']['id'] = 'field_name_alias_value';
  $handler->display->display_options['fields']['field_name_alias_value']['table'] = 'node_data_field_name_alias';
  $handler->display->display_options['fields']['field_name_alias_value']['field'] = 'field_name_alias_value';
  $handler->display->display_options['fields']['field_name_alias_value']['label'] = 'Name Alias(es)';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['field_ventsite_txt_value']['id'] = 'field_ventsite_txt_value';
  $handler->display->display_options['fields']['field_ventsite_txt_value']['table'] = 'node_data_field_ventsite_txt';
  $handler->display->display_options['fields']['field_ventsite_txt_value']['field'] = 'field_ventsite_txt_value';
  $handler->display->display_options['fields']['field_ventsite_txt_value']['label'] = 'Vent Sites (text)';
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'ventfield' => 'ventfield',
  );
  $handler->display->display_options['filters']['type']['group'] = '0';
  $handler->display->display_options['filters']['type']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = '1';
  $handler->display->display_options['filters']['status']['group'] = '0';
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;

  /* Display: Page */
  $handler = $view->new_display('page', 'Page', 'page_1');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['items_per_page'] = 0;
  $handler->display->display_options['defaults']['style_plugin'] = FALSE;
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['grouping'] = '';
  $handler->display->display_options['style_options']['columns'] = array(
    'title' => 'title',
    'name' => 'name',
    'name_6' => 'name_6',
    'name_3' => 'name_3',
    'field_latitude4_value' => 'field_latitude4_value',
    'field_latitude_value' => 'field_latitude_value',
    'field_longitude4_value' => 'field_longitude4_value',
    'field_longitude_value' => 'field_longitude_value',
    'field_depth_value' => 'field_depth_value',
    'field_yr_mns_discovered_value' => 'field_yr_mns_discovered_value',
  );
  $handler->display->display_options['style_options']['default'] = 'title';
  $handler->display->display_options['style_options']['info'] = array(
    'title' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'name' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'name_6' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'name_3' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_latitude4_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_latitude_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_longitude4_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_longitude_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_depth_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_yr_mns_discovered_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
  );
  $handler->display->display_options['defaults']['style_options'] = FALSE;
  $handler->display->display_options['defaults']['row_plugin'] = FALSE;
  $handler->display->display_options['defaults']['row_options'] = FALSE;
  $handler->display->display_options['defaults']['header'] = FALSE;
  /* Header: Global: Text area */
  $handler->display->display_options['header']['text']['id'] = 'area';
  $handler->display->display_options['header']['text']['table'] = 'views';
  $handler->display->display_options['header']['text']['field'] = 'area';
  $handler->display->display_options['header']['text']['content'] = 'This table can be copied/pasted into a spreadsheet application to supplement the downloaded full list <a href="/ventfields_list_all">CSV file</a> (which only lists the first name alias and first vent site due to a software bug).
To view details for a specific Vent Field, click on the Name ID.';
  $handler->display->display_options['header']['text']['format'] = '1';
  $handler->display->display_options['defaults']['footer'] = FALSE;
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name ID';
  $handler->display->display_options['path'] = 'not_being_used';
  $handler->display->display_options['menu']['title'] = 'VentFields';
  $handler->display->display_options['menu']['description'] = 'VentFields - full listing';
  $handler->display->display_options['menu']['weight'] = '0';
  $export['vent_fields'] = $view;

  $view = new view();
  $view->name = 'vent_fields_clone';
  $view->description = 'Vent Fields';
  $view->tag = '';
  $view->base_table = 'node';
  $view->human_name = '';
  $view->core = 0;
  $view->api_version = '3.0';
  $view->disabled = FALSE; /* Edit this to true to make a default view disabled initially */

  /* Display: Defaults */
  $handler = $view->new_display('default', 'Defaults', 'default');
  $handler->display->display_options['title'] = 'Vent Fields';
  $handler->display->display_options['use_more_always'] = FALSE;
  $handler->display->display_options['access']['type'] = 'none';
  $handler->display->display_options['cache']['type'] = 'none';
  $handler->display->display_options['query']['type'] = 'views_query';
  $handler->display->display_options['query']['options']['distinct'] = TRUE;
  $handler->display->display_options['exposed_form']['type'] = 'basic';
  $handler->display->display_options['pager']['type'] = 'full';
  $handler->display->display_options['pager']['options']['items_per_page'] = '30';
  $handler->display->display_options['pager']['options']['offset'] = '0';
  $handler->display->display_options['pager']['options']['id'] = '0';
  $handler->display->display_options['pager']['options']['quantity'] = '9';
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['grouping'] = '';
  $handler->display->display_options['style_options']['columns'] = array(
    'title' => 'title',
    'field_ventsite_txt_value' => 'field_ventsite_txt_value',
    'field_activity_value' => 'field_activity_value',
    'field_max_depth_value' => 'field_max_depth_value',
    'field_longitude_value' => 'field_longitude_value',
    'field_latitude_value' => 'field_latitude_value',
    'field_max_temp_cat_value' => 'field_max_temp_cat_value',
    'field_max_temp_value' => 'field_max_temp_value',
  );
  $handler->display->display_options['style_options']['default'] = 'title';
  $handler->display->display_options['style_options']['info'] = array(
    'title' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_ventsite_txt_value' => array(
      'separator' => '',
    ),
    'field_activity_value' => array(
      'separator' => '',
    ),
    'field_max_depth_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_longitude_value' => array(
      'sortable' => 0,
      'separator' => '',
    ),
    'field_latitude_value' => array(
      'sortable' => 0,
      'separator' => '',
    ),
    'field_max_temp_cat_value' => array(
      'separator' => '',
    ),
    'field_max_temp_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
  );
  $handler->display->display_options['style_options']['override'] = FALSE;
  /* Header: Global: Text area */
  $handler->display->display_options['header']['text']['id'] = 'area';
  $handler->display->display_options['header']['text']['table'] = 'views';
  $handler->display->display_options['header']['text']['field'] = 'area';
  $handler->display->display_options['header']['text']['content'] = 'Welcome to the InterRidge Global Database of Active Submarine Hydrothermal Vent Fields (the <strong>“InterRidge Vents Database”</strong>).
To sort the list of Vent Fields, click on a column header.
To view details for a specific Vent Field, click on the Name ID.
To view the subset of Vent Fields that contain a specific category, click on the category (for example, "active, confirmed" in the "Activity" column).';
  $handler->display->display_options['header']['text']['format'] = '1';
  /* Footer: Global: Text area */
  $handler->display->display_options['footer']['text']['id'] = 'area';
  $handler->display->display_options['footer']['text']['table'] = 'views';
  $handler->display->display_options['footer']['text']['field'] = 'area';
  $handler->display->display_options['footer']['text']['content'] = 'To display the full list of Vent Fields, and for export to a CSV file, please click <a href="/irvents/ventfields_list_all">HERE</a>.';
  $handler->display->display_options['footer']['text']['format'] = '1';
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid']['id'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid']['label'] = 'Activity';
  $handler->display->display_options['relationships']['term_node_tid']['vocabularies'] = array(
    'vocabulary_5' => 'vocabulary_5',
  );
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid_1']['id'] = 'term_node_tid_1';
  $handler->display->display_options['relationships']['term_node_tid_1']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid_1']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid_1']['label'] = 'Tectonic Setting';
  $handler->display->display_options['relationships']['term_node_tid_1']['vocabularies'] = array(
    'vocabulary_8' => 'vocabulary_8',
  );
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid_2']['id'] = 'term_node_tid_2';
  $handler->display->display_options['relationships']['term_node_tid_2']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid_2']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid_2']['label'] = 'Region';
  $handler->display->display_options['relationships']['term_node_tid_2']['vocabularies'] = array(
    'vocabulary_14' => 'vocabulary_14',
  );
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name ID';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['field_ventsite_txt_value']['id'] = 'field_ventsite_txt_value';
  $handler->display->display_options['fields']['field_ventsite_txt_value']['table'] = 'node_data_field_ventsite_txt';
  $handler->display->display_options['fields']['field_ventsite_txt_value']['field'] = 'field_ventsite_txt_value';
  $handler->display->display_options['fields']['field_ventsite_txt_value']['label'] = 'Vent Sites (text)';
  /* Field: Broken/missing handler */
  $handler->display->display_options['fields']['field_max_temp_value']['id'] = 'field_max_temp_value';
  $handler->display->display_options['fields']['field_max_temp_value']['table'] = 'node_data_field_max_temp';
  $handler->display->display_options['fields']['field_max_temp_value']['field'] = 'field_max_temp_value';
  $handler->display->display_options['fields']['field_max_temp_value']['label'] = 'Maximum Temperature';
  $handler->display->display_options['fields']['field_max_temp_value']['empty_zero'] = TRUE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'ventfield' => 'ventfield',
  );
  $handler->display->display_options['filters']['type']['group'] = '0';
  $handler->display->display_options['filters']['type']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = '1';
  $handler->display->display_options['filters']['status']['group'] = '0';
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;

  /* Display: Page */
  $handler = $view->new_display('page', 'Page', 'page_1');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['defaults']['style_plugin'] = FALSE;
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['columns'] = array(
    'title' => 'title',
    'name' => 'name',
    'name_1' => 'name_1',
    'name_3' => 'name_3',
    'field_latitude4' => 'field_latitude4',
    'field_longitude4' => 'field_longitude4',
    'field_depth' => 'field_depth',
    'field_yr_mns_discovered' => 'field_yr_mns_discovered',
  );
  $handler->display->display_options['style_options']['default'] = 'title';
  $handler->display->display_options['style_options']['info'] = array(
    'title' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'name' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'name_1' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'name_3' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_latitude4' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_longitude4' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_depth' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_yr_mns_discovered' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
  );
  $handler->display->display_options['defaults']['style_options'] = FALSE;
  $handler->display->display_options['defaults']['row_plugin'] = FALSE;
  $handler->display->display_options['defaults']['row_options'] = FALSE;
  $handler->display->display_options['defaults']['footer'] = FALSE;
  /* Footer: Global: Text area */
  $handler->display->display_options['footer']['text']['id'] = 'area';
  $handler->display->display_options['footer']['text']['table'] = 'views';
  $handler->display->display_options['footer']['text']['field'] = 'area';
  $handler->display->display_options['footer']['text']['content'] = 'To display the full list of Vent Fields, and for export to a CSV file, please click <a href="/ventfields_list_all">HERE</a>.';
  $handler->display->display_options['footer']['text']['format'] = '1';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Vent Field Name ID';
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'term_node_tid';
  $handler->display->display_options['fields']['name']['label'] = 'Activity';
  $handler->display->display_options['fields']['name']['link_to_taxonomy'] = TRUE;
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name_1']['id'] = 'name_1';
  $handler->display->display_options['fields']['name_1']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name_1']['field'] = 'name';
  $handler->display->display_options['fields']['name_1']['relationship'] = 'term_node_tid_1';
  $handler->display->display_options['fields']['name_1']['label'] = 'Tectonic Setting';
  $handler->display->display_options['fields']['name_1']['link_to_taxonomy'] = TRUE;
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name_3']['id'] = 'name_3';
  $handler->display->display_options['fields']['name_3']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name_3']['field'] = 'name';
  $handler->display->display_options['fields']['name_3']['relationship'] = 'term_node_tid_2';
  $handler->display->display_options['fields']['name_3']['label'] = 'Region';
  $handler->display->display_options['fields']['name_3']['link_to_taxonomy'] = TRUE;
  /* Field: Content: Latitude */
  $handler->display->display_options['fields']['field_latitude4']['id'] = 'field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['table'] = 'field_data_field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['field'] = 'field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_latitude4']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '4',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Longitude */
  $handler->display->display_options['fields']['field_longitude4']['id'] = 'field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['table'] = 'field_data_field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['field'] = 'field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_longitude4']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '4',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Maximum or Single Reported Depth (mbsl) */
  $handler->display->display_options['fields']['field_depth']['id'] = 'field_depth';
  $handler->display->display_options['fields']['field_depth']['table'] = 'field_data_field_depth';
  $handler->display->display_options['fields']['field_depth']['field'] = 'field_depth';
  $handler->display->display_options['fields']['field_depth']['label'] = 'Maximum or Single Reported Depth';
  $handler->display->display_options['fields']['field_depth']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_depth']['settings'] = array(
    'thousand_separator' => '',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Year and How Discovered (if active, visual confirmation is listed first) */
  $handler->display->display_options['fields']['field_yr_mns_discovered']['id'] = 'field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['table'] = 'field_data_field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['field'] = 'field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['label'] = 'Year and How Discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['element_label_colon'] = FALSE;
  $handler->display->display_options['path'] = 'ventfields';
  $handler->display->display_options['menu']['type'] = 'tab';
  $handler->display->display_options['menu']['title'] = 'Vent Fields';
  $handler->display->display_options['menu']['description'] = 'Vent Fields - full listing';
  $handler->display->display_options['menu']['weight'] = '10';
  $export['vent_fields_clone'] = $view;

  $view = new view();
  $view->name = 'vent_fields_full';
  $view->description = 'Vent Fields full list';
  $view->tag = '';
  $view->base_table = 'node';
  $view->human_name = '';
  $view->core = 0;
  $view->api_version = '3.0';
  $view->disabled = FALSE; /* Edit this to true to make a default view disabled initially */

  /* Display: Defaults */
  $handler = $view->new_display('default', 'Defaults', 'default');
  $handler->display->display_options['title'] = 'Vent Fields list all';
  $handler->display->display_options['items_per_page'] = 0;
  $handler->display->display_options['use_more_always'] = FALSE;
  $handler->display->display_options['access']['type'] = 'none';
  $handler->display->display_options['cache']['type'] = 'none';
  $handler->display->display_options['query']['type'] = 'views_query';
  $handler->display->display_options['query']['options']['distinct'] = TRUE;
  $handler->display->display_options['exposed_form']['type'] = 'basic';
  $handler->display->display_options['pager']['type'] = 'none';
  $handler->display->display_options['pager']['options']['offset'] = '0';
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['grouping'] = '';
  $handler->display->display_options['style_options']['columns'] = array(
    'title' => 'title',
    'field_ventsite_txt_value' => 'field_ventsite_txt_value',
    'field_activity_value' => 'field_activity_value',
    'field_max_depth_value' => 'field_max_depth_value',
    'field_longitude_value' => 'field_longitude_value',
    'field_latitude_value' => 'field_latitude_value',
    'field_max_temp_cat_value' => 'field_max_temp_cat_value',
    'field_max_temp_value' => 'field_max_temp_value',
  );
  $handler->display->display_options['style_options']['default'] = 'title';
  $handler->display->display_options['style_options']['info'] = array(
    'title' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_ventsite_txt_value' => array(
      'separator' => '',
    ),
    'field_activity_value' => array(
      'separator' => '',
    ),
    'field_max_depth_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
    'field_longitude_value' => array(
      'sortable' => 0,
      'separator' => '',
    ),
    'field_latitude_value' => array(
      'sortable' => 0,
      'separator' => '',
    ),
    'field_max_temp_cat_value' => array(
      'separator' => '',
    ),
    'field_max_temp_value' => array(
      'sortable' => 1,
      'separator' => '',
    ),
  );
  $handler->display->display_options['style_options']['override'] = FALSE;
  /* Header: Global: Text area */
  $handler->display->display_options['header']['text']['id'] = 'area';
  $handler->display->display_options['header']['text']['table'] = 'views';
  $handler->display->display_options['header']['text']['field'] = 'area';
  $handler->display->display_options['header']['text']['content'] = 'Below is the full list of all Vent Fields.
Click to <a href="/irvents/ventfields_list_all_CSV">export the list to a CSV file</a>. <em>Note: the CSV output is only including the first name alias and the first vent site listed for each vent field. We will have this fixed soon. Thanks 9-Nov-2011.</em>';
  $handler->display->display_options['header']['text']['format'] = '1';
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid']['id'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid']['label'] = 'Activity';
  $handler->display->display_options['relationships']['term_node_tid']['vocabularies'] = array(
    'vocabulary_5' => 'vocabulary_5',
  );
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid_1']['id'] = 'term_node_tid_1';
  $handler->display->display_options['relationships']['term_node_tid_1']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid_1']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid_1']['label'] = 'Tectonic Setting';
  $handler->display->display_options['relationships']['term_node_tid_1']['vocabularies'] = array(
    'vocabulary_8' => 'vocabulary_8',
  );
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid_2']['id'] = 'term_node_tid_2';
  $handler->display->display_options['relationships']['term_node_tid_2']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid_2']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid_2']['label'] = 'Region';
  $handler->display->display_options['relationships']['term_node_tid_2']['vocabularies'] = array(
    'vocabulary_14' => 'vocabulary_14',
  );
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid_3']['id'] = 'term_node_tid_3';
  $handler->display->display_options['relationships']['term_node_tid_3']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid_3']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid_3']['label'] = 'Feature ID';
  $handler->display->display_options['relationships']['term_node_tid_3']['vocabularies'] = array(
    'vocabulary_19' => 'vocabulary_19',
  );
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid_4']['id'] = 'term_node_tid_4';
  $handler->display->display_options['relationships']['term_node_tid_4']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid_4']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid_4']['label'] = 'Max temperature category';
  $handler->display->display_options['relationships']['term_node_tid_4']['vocabularies'] = array(
    'vocabulary_7' => 'vocabulary_7',
  );
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid_5']['id'] = 'term_node_tid_5';
  $handler->display->display_options['relationships']['term_node_tid_5']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid_5']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid_5']['label'] = 'Ocean';
  $handler->display->display_options['relationships']['term_node_tid_5']['vocabularies'] = array(
    'vocabulary_6' => 'vocabulary_6',
  );
  /* Relationship: Content: Taxonomy terms on node */
  $handler->display->display_options['relationships']['term_node_tid_6']['id'] = 'term_node_tid_6';
  $handler->display->display_options['relationships']['term_node_tid_6']['table'] = 'node';
  $handler->display->display_options['relationships']['term_node_tid_6']['field'] = 'term_node_tid';
  $handler->display->display_options['relationships']['term_node_tid_6']['label'] = 'National Jurisdiction';
  $handler->display->display_options['relationships']['term_node_tid_6']['vocabularies'] = array(
    'vocabulary_13' => 'vocabulary_13',
  );
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Name ID';
  /* Field: Content: Name Alias(es) */
  $handler->display->display_options['fields']['field_name_alias']['id'] = 'field_name_alias';
  $handler->display->display_options['fields']['field_name_alias']['table'] = 'field_data_field_name_alias';
  $handler->display->display_options['fields']['field_name_alias']['field'] = 'field_name_alias';
  $handler->display->display_options['fields']['field_name_alias']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_name_alias']['delta_offset'] = '0';
  /* Field: Content: MGDS_FeatureID lowest in hierarchy */
  $handler->display->display_options['fields']['taxonomy_vocabulary_19']['id'] = 'taxonomy_vocabulary_19';
  $handler->display->display_options['fields']['taxonomy_vocabulary_19']['table'] = 'field_data_taxonomy_vocabulary_19';
  $handler->display->display_options['fields']['taxonomy_vocabulary_19']['field'] = 'taxonomy_vocabulary_19';
  $handler->display->display_options['fields']['taxonomy_vocabulary_19']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['taxonomy_vocabulary_19']['delta_offset'] = '0';
  /* Field: Content: Vent Sites */
  $handler->display->display_options['fields']['field_ventsite_txt']['id'] = 'field_ventsite_txt';
  $handler->display->display_options['fields']['field_ventsite_txt']['table'] = 'field_data_field_ventsite_txt';
  $handler->display->display_options['fields']['field_ventsite_txt']['field'] = 'field_ventsite_txt';
  $handler->display->display_options['fields']['field_ventsite_txt']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_ventsite_txt']['delta_offset'] = '0';
  /* Field: Content: Activity */
  $handler->display->display_options['fields']['taxonomy_vocabulary_5']['id'] = 'taxonomy_vocabulary_5';
  $handler->display->display_options['fields']['taxonomy_vocabulary_5']['table'] = 'field_data_taxonomy_vocabulary_5';
  $handler->display->display_options['fields']['taxonomy_vocabulary_5']['field'] = 'taxonomy_vocabulary_5';
  $handler->display->display_options['fields']['taxonomy_vocabulary_5']['element_label_colon'] = FALSE;
  /* Field: Content: Maximum Temperature */
  $handler->display->display_options['fields']['field_max_temp']['id'] = 'field_max_temp';
  $handler->display->display_options['fields']['field_max_temp']['table'] = 'field_data_field_max_temp';
  $handler->display->display_options['fields']['field_max_temp']['field'] = 'field_max_temp';
  $handler->display->display_options['fields']['field_max_temp']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_max_temp']['settings'] = array(
    'thousand_separator' => ' ',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Max Temperature Category */
  $handler->display->display_options['fields']['taxonomy_vocabulary_7']['id'] = 'taxonomy_vocabulary_7';
  $handler->display->display_options['fields']['taxonomy_vocabulary_7']['table'] = 'field_data_taxonomy_vocabulary_7';
  $handler->display->display_options['fields']['taxonomy_vocabulary_7']['field'] = 'taxonomy_vocabulary_7';
  $handler->display->display_options['fields']['taxonomy_vocabulary_7']['element_label_colon'] = FALSE;
  /* Field: Content: Latitude */
  $handler->display->display_options['fields']['field_latitude4']['id'] = 'field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['table'] = 'field_data_field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['field'] = 'field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_latitude4']['settings'] = array(
    'thousand_separator' => ' ',
    'decimal_separator' => '.',
    'scale' => '4',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Longitude */
  $handler->display->display_options['fields']['field_longitude4']['id'] = 'field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['table'] = 'field_data_field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['field'] = 'field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_longitude4']['settings'] = array(
    'thousand_separator' => ' ',
    'decimal_separator' => '.',
    'scale' => '4',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Ocean */
  $handler->display->display_options['fields']['taxonomy_vocabulary_6']['id'] = 'taxonomy_vocabulary_6';
  $handler->display->display_options['fields']['taxonomy_vocabulary_6']['table'] = 'field_data_taxonomy_vocabulary_6';
  $handler->display->display_options['fields']['taxonomy_vocabulary_6']['field'] = 'taxonomy_vocabulary_6';
  $handler->display->display_options['fields']['taxonomy_vocabulary_6']['element_label_colon'] = FALSE;
  /* Field: Content: Region */
  $handler->display->display_options['fields']['taxonomy_vocabulary_14']['id'] = 'taxonomy_vocabulary_14';
  $handler->display->display_options['fields']['taxonomy_vocabulary_14']['table'] = 'field_data_taxonomy_vocabulary_14';
  $handler->display->display_options['fields']['taxonomy_vocabulary_14']['field'] = 'taxonomy_vocabulary_14';
  $handler->display->display_options['fields']['taxonomy_vocabulary_14']['element_label_colon'] = FALSE;
  /* Field: Content: National Jurisdiction */
  $handler->display->display_options['fields']['taxonomy_vocabulary_13']['id'] = 'taxonomy_vocabulary_13';
  $handler->display->display_options['fields']['taxonomy_vocabulary_13']['table'] = 'field_data_taxonomy_vocabulary_13';
  $handler->display->display_options['fields']['taxonomy_vocabulary_13']['field'] = 'taxonomy_vocabulary_13';
  $handler->display->display_options['fields']['taxonomy_vocabulary_13']['element_label_colon'] = FALSE;
  /* Field: Content: Maximum or Single Reported Depth (mbsl) */
  $handler->display->display_options['fields']['field_depth']['id'] = 'field_depth';
  $handler->display->display_options['fields']['field_depth']['table'] = 'field_data_field_depth';
  $handler->display->display_options['fields']['field_depth']['field'] = 'field_depth';
  $handler->display->display_options['fields']['field_depth']['label'] = 'Maximum or Single Reported Depth';
  $handler->display->display_options['fields']['field_depth']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_depth']['settings'] = array(
    'thousand_separator' => '',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Minimum Depth (mbsl) */
  $handler->display->display_options['fields']['field_min_depth']['id'] = 'field_min_depth';
  $handler->display->display_options['fields']['field_min_depth']['table'] = 'field_data_field_min_depth';
  $handler->display->display_options['fields']['field_min_depth']['field'] = 'field_min_depth';
  $handler->display->display_options['fields']['field_min_depth']['label'] = 'Minimum Depth';
  $handler->display->display_options['fields']['field_min_depth']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_min_depth']['settings'] = array(
    'thousand_separator' => '',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Tectonic setting */
  $handler->display->display_options['fields']['taxonomy_vocabulary_8']['id'] = 'taxonomy_vocabulary_8';
  $handler->display->display_options['fields']['taxonomy_vocabulary_8']['table'] = 'field_data_taxonomy_vocabulary_8';
  $handler->display->display_options['fields']['taxonomy_vocabulary_8']['field'] = 'taxonomy_vocabulary_8';
  $handler->display->display_options['fields']['taxonomy_vocabulary_8']['element_label_colon'] = FALSE;
  /* Field: Content: Full Spreading Rate (mm/a) */
  $handler->display->display_options['fields']['field_full_spread_rate_d']['id'] = 'field_full_spread_rate_d';
  $handler->display->display_options['fields']['field_full_spread_rate_d']['table'] = 'field_data_field_full_spread_rate_d';
  $handler->display->display_options['fields']['field_full_spread_rate_d']['field'] = 'field_full_spread_rate_d';
  $handler->display->display_options['fields']['field_full_spread_rate_d']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_full_spread_rate_d']['settings'] = array(
    'thousand_separator' => ' ',
    'decimal_separator' => '.',
    'scale' => '2',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Host Rock */
  $handler->display->display_options['fields']['field_host_rock']['id'] = 'field_host_rock';
  $handler->display->display_options['fields']['field_host_rock']['table'] = 'field_data_field_host_rock';
  $handler->display->display_options['fields']['field_host_rock']['field'] = 'field_host_rock';
  $handler->display->display_options['fields']['field_host_rock']['element_label_colon'] = FALSE;
  /* Field: Content: Deposit Type */
  $handler->display->display_options['fields']['field_deposit_type']['id'] = 'field_deposit_type';
  $handler->display->display_options['fields']['field_deposit_type']['table'] = 'field_data_field_deposit_type';
  $handler->display->display_options['fields']['field_deposit_type']['field'] = 'field_deposit_type';
  $handler->display->display_options['fields']['field_deposit_type']['element_label_colon'] = FALSE;
  /* Field: Content: Notes on Vent Field Description */
  $handler->display->display_options['fields']['field_description']['id'] = 'field_description';
  $handler->display->display_options['fields']['field_description']['table'] = 'field_data_field_description';
  $handler->display->display_options['fields']['field_description']['field'] = 'field_description';
  $handler->display->display_options['fields']['field_description']['element_label_colon'] = FALSE;
  /* Field: Content: Notes Relevant to Biology */
  $handler->display->display_options['fields']['field_organisms']['id'] = 'field_organisms';
  $handler->display->display_options['fields']['field_organisms']['table'] = 'field_data_field_organisms';
  $handler->display->display_options['fields']['field_organisms']['field'] = 'field_organisms';
  $handler->display->display_options['fields']['field_organisms']['element_label_colon'] = FALSE;
  /* Field: Content: Year and How Discovered (if active, visual confirmation is listed first) */
  $handler->display->display_options['fields']['field_yr_mns_discovered']['id'] = 'field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['table'] = 'field_data_field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['field'] = 'field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['element_label_colon'] = FALSE;
  /* Field: Content: Discovery References (text) */
  $handler->display->display_options['fields']['field_discoveryref_txt']['id'] = 'field_discoveryref_txt';
  $handler->display->display_options['fields']['field_discoveryref_txt']['table'] = 'field_data_field_discoveryref_txt';
  $handler->display->display_options['fields']['field_discoveryref_txt']['field'] = 'field_discoveryref_txt';
  $handler->display->display_options['fields']['field_discoveryref_txt']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_discoveryref_txt']['delta_offset'] = '0';
  /* Field: Content: Other References (text) */
  $handler->display->display_options['fields']['field_otherref_txt']['id'] = 'field_otherref_txt';
  $handler->display->display_options['fields']['field_otherref_txt']['table'] = 'field_data_field_otherref_txt';
  $handler->display->display_options['fields']['field_otherref_txt']['field'] = 'field_otherref_txt';
  $handler->display->display_options['fields']['field_otherref_txt']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_otherref_txt']['delta_offset'] = '0';
  /* Sort criterion: Content: Title */
  $handler->display->display_options['sorts']['title']['id'] = 'title';
  $handler->display->display_options['sorts']['title']['table'] = 'node';
  $handler->display->display_options['sorts']['title']['field'] = 'title';
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'ventfield' => 'ventfield',
  );
  $handler->display->display_options['filters']['type']['group'] = '0';
  $handler->display->display_options['filters']['type']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = '1';
  $handler->display->display_options['filters']['status']['group'] = '0';
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;

  /* Display: Page */
  $handler = $view->new_display('page', 'Page', 'page_1');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['items_per_page'] = 0;
  $handler->display->display_options['defaults']['style_plugin'] = FALSE;
  $handler->display->display_options['style_plugin'] = 'table';
  $handler->display->display_options['style_options']['columns'] = array(
    'title' => 'title',
    'name' => 'name',
    'name_6' => 'name_6',
    'name_3' => 'name_3',
    'field_latitude4' => 'field_latitude4',
    'field_longitude4' => 'field_longitude4',
    'field_depth' => 'field_depth',
    'field_yr_mns_discovered' => 'field_yr_mns_discovered',
  );
  $handler->display->display_options['style_options']['default'] = 'title';
  $handler->display->display_options['style_options']['info'] = array(
    'title' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'name' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'name_6' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'name_3' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_latitude4' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_longitude4' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_depth' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
    'field_yr_mns_discovered' => array(
      'sortable' => 1,
      'default_sort_order' => 'asc',
      'align' => '',
      'separator' => '',
      'empty_column' => 0,
    ),
  );
  $handler->display->display_options['defaults']['style_options'] = FALSE;
  $handler->display->display_options['defaults']['row_plugin'] = FALSE;
  $handler->display->display_options['defaults']['row_options'] = FALSE;
  $handler->display->display_options['defaults']['header'] = FALSE;
  /* Header: Global: Text area */
  $handler->display->display_options['header']['text']['id'] = 'text';
  $handler->display->display_options['header']['text']['table'] = 'views';
  $handler->display->display_options['header']['text']['field'] = 'area';
  $handler->display->display_options['header']['text']['content'] = 'Below is the full list of all Vent Fields.
Click to <a href="/ventfields_list_all_CSV">export the list to a CSV file</a>.';
  $handler->display->display_options['header']['text']['format'] = '1';
  $handler->display->display_options['defaults']['fields'] = FALSE;
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Vent Field Name ID';
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name']['id'] = 'name';
  $handler->display->display_options['fields']['name']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name']['field'] = 'name';
  $handler->display->display_options['fields']['name']['relationship'] = 'term_node_tid';
  $handler->display->display_options['fields']['name']['label'] = 'Activity';
  $handler->display->display_options['fields']['name']['link_to_taxonomy'] = TRUE;
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name_6']['id'] = 'name_6';
  $handler->display->display_options['fields']['name_6']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name_6']['field'] = 'name';
  $handler->display->display_options['fields']['name_6']['relationship'] = 'term_node_tid_1';
  $handler->display->display_options['fields']['name_6']['label'] = 'Tectonic Setting';
  $handler->display->display_options['fields']['name_6']['link_to_taxonomy'] = TRUE;
  /* Field: Taxonomy term: Name */
  $handler->display->display_options['fields']['name_3']['id'] = 'name_3';
  $handler->display->display_options['fields']['name_3']['table'] = 'taxonomy_term_data';
  $handler->display->display_options['fields']['name_3']['field'] = 'name';
  $handler->display->display_options['fields']['name_3']['relationship'] = 'term_node_tid_2';
  $handler->display->display_options['fields']['name_3']['label'] = 'Region';
  $handler->display->display_options['fields']['name_3']['link_to_taxonomy'] = TRUE;
  /* Field: Content: Latitude */
  $handler->display->display_options['fields']['field_latitude4']['id'] = 'field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['table'] = 'field_data_field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['field'] = 'field_latitude4';
  $handler->display->display_options['fields']['field_latitude4']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_latitude4']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '4',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Longitude */
  $handler->display->display_options['fields']['field_longitude4']['id'] = 'field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['table'] = 'field_data_field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['field'] = 'field_longitude4';
  $handler->display->display_options['fields']['field_longitude4']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_longitude4']['settings'] = array(
    'thousand_separator' => '',
    'decimal_separator' => '.',
    'scale' => '4',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Maximum or Single Reported Depth (mbsl) */
  $handler->display->display_options['fields']['field_depth']['id'] = 'field_depth';
  $handler->display->display_options['fields']['field_depth']['table'] = 'field_data_field_depth';
  $handler->display->display_options['fields']['field_depth']['field'] = 'field_depth';
  $handler->display->display_options['fields']['field_depth']['label'] = 'Maximum or Single Reported Depth';
  $handler->display->display_options['fields']['field_depth']['element_label_colon'] = FALSE;
  $handler->display->display_options['fields']['field_depth']['settings'] = array(
    'thousand_separator' => '',
    'prefix_suffix' => 1,
  );
  /* Field: Content: Year and How Discovered (if active, visual confirmation is listed first) */
  $handler->display->display_options['fields']['field_yr_mns_discovered']['id'] = 'field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['table'] = 'field_data_field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['field'] = 'field_yr_mns_discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['label'] = 'Year and How Discovered';
  $handler->display->display_options['fields']['field_yr_mns_discovered']['element_label_colon'] = FALSE;
  $handler->display->display_options['path'] = 'ventfields_list_all';
  $handler->display->display_options['menu']['title'] = 'VentFields';
  $handler->display->display_options['menu']['description'] = 'VentFields - full listing';
  $handler->display->display_options['menu']['weight'] = '0';

  /* Display: Data export */
  $handler = $view->new_display('views_data_export', 'Data export', 'views_data_export_1');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['pager']['type'] = 'none';
  $handler->display->display_options['pager']['options']['offset'] = '0';
  $handler->display->display_options['style_plugin'] = 'views_data_export_csv';
  $handler->display->display_options['style_options']['provide_file'] = 1;
  $handler->display->display_options['style_options']['filename'] = 'vent_fields_all.csv';
  $handler->display->display_options['style_options']['parent_sort'] = 0;
  $handler->display->display_options['style_options']['quote'] = 1;
  $handler->display->display_options['style_options']['trim'] = 0;
  $handler->display->display_options['style_options']['replace_newlines'] = 0;
  $handler->display->display_options['style_options']['header'] = 1;
  $handler->display->display_options['path'] = 'ventfields_list_all_CSV';
  $export['vent_fields_full'] = $view;

  $view = new view();
  $view->name = 'vent_fields_geofield_map';
  $view->description = '';
  $view->tag = 'default';
  $view->base_table = 'node';
  $view->human_name = 'vent_fields_Geofield_map';
  $view->core = 7;
  $view->api_version = '3.0';
  $view->disabled = FALSE; /* Edit this to true to make a default view disabled initially */

  /* Display: Master */
  $handler = $view->new_display('default', 'Master', 'default');
  $handler->display->display_options['title'] = 'Current Interactive Map of Vent Fields';
  $handler->display->display_options['use_more_always'] = FALSE;
  $handler->display->display_options['access']['type'] = 'perm';
  $handler->display->display_options['cache']['type'] = 'none';
  $handler->display->display_options['query']['type'] = 'views_query';
  $handler->display->display_options['exposed_form']['type'] = 'basic';
  $handler->display->display_options['pager']['type'] = 'none';
  $handler->display->display_options['style_plugin'] = 'geofield_map_map';
  $handler->display->display_options['style_options']['data_source'] = 'field_geofield_lat_and_lon';
  $handler->display->display_options['style_options']['popup_source'] = 'title';
  $handler->display->display_options['style_options']['geofield_map_height'] = '500px';
  $handler->display->display_options['style_options']['geofield_map_baselayers_physical'] = 1;
  /* Header: Global: Text area */
  $handler->display->display_options['header']['area']['id'] = 'area';
  $handler->display->display_options['header']['area']['table'] = 'views';
  $handler->display->display_options['header']['area']['field'] = 'area';
  $handler->display->display_options['header']['area']['format'] = '1';
  /* Field: Content: Title */
  $handler->display->display_options['fields']['title']['id'] = 'title';
  $handler->display->display_options['fields']['title']['table'] = 'node';
  $handler->display->display_options['fields']['title']['field'] = 'title';
  $handler->display->display_options['fields']['title']['label'] = 'Vent Field Name ID';
  /* Field: Content: Location on map */
  $handler->display->display_options['fields']['field_geofield_lat_and_lon']['id'] = 'field_geofield_lat_and_lon';
  $handler->display->display_options['fields']['field_geofield_lat_and_lon']['table'] = 'field_data_field_geofield_lat_and_lon';
  $handler->display->display_options['fields']['field_geofield_lat_and_lon']['field'] = 'field_geofield_lat_and_lon';
  $handler->display->display_options['fields']['field_geofield_lat_and_lon']['click_sort_column'] = 'wkt';
  $handler->display->display_options['fields']['field_geofield_lat_and_lon']['type'] = 'geofield_latlon';
  $handler->display->display_options['fields']['field_geofield_lat_and_lon']['settings'] = array(
    'data' => 'full',
    'format' => 'decimal_degrees',
    'labels' => 1,
  );
  /* Sort criterion: Content: Post date */
  $handler->display->display_options['sorts']['created']['id'] = 'created';
  $handler->display->display_options['sorts']['created']['table'] = 'node';
  $handler->display->display_options['sorts']['created']['field'] = 'created';
  $handler->display->display_options['sorts']['created']['order'] = 'DESC';
  /* Filter criterion: Content: Published */
  $handler->display->display_options['filters']['status']['id'] = 'status';
  $handler->display->display_options['filters']['status']['table'] = 'node';
  $handler->display->display_options['filters']['status']['field'] = 'status';
  $handler->display->display_options['filters']['status']['value'] = 1;
  $handler->display->display_options['filters']['status']['group'] = 1;
  $handler->display->display_options['filters']['status']['expose']['operator'] = FALSE;
  /* Filter criterion: Content: Type */
  $handler->display->display_options['filters']['type']['id'] = 'type';
  $handler->display->display_options['filters']['type']['table'] = 'node';
  $handler->display->display_options['filters']['type']['field'] = 'type';
  $handler->display->display_options['filters']['type']['value'] = array(
    'ventfield' => 'ventfield',
  );

  /* Display: Page */
  $handler = $view->new_display('page', 'Page', 'page');
  $handler->display->display_options['defaults']['hide_admin_links'] = FALSE;
  $handler->display->display_options['path'] = 'vent-fields-geofield-map';
  $handler->display->display_options['menu']['type'] = 'normal';
  $handler->display->display_options['menu']['title'] = 'vent_fields_Geofield_map';
  $handler->display->display_options['menu']['name'] = 'main-menu';
  $export['vent_fields_geofield_map'] = $view;

  return $export;
}
