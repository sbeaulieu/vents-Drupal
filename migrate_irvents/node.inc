<?php

/**
 * Common mappings for the Drupal 7 node migrations.
 */
abstract class IRVentMigration extends DrupalNode7Migration {
  public function __construct(array $arguments) {
    parent::__construct($arguments);

  }
}

/**
 * Image-specific mappings and handling.
 */
class VentFieldMigration extends IRVentMigration {
  public function __construct(array $arguments) {
        parent::__construct($arguments);

        // Note that we map migrated terms by the vocabulary ID.
    $this->addFieldMapping('taxonomy_vocabulary_19', 'taxonomy_vocabulary_19')
         ->sourceMigration('MGDS_FeatureID')
         ->arguments(array('source_type' => 'tid'));
    $this->addFieldMapping('taxonomy_vocabulary_14', 'taxonomy_vocabulary_14')
         ->sourceMigration('Region')
         ->arguments(array('source_type' => 'tid'));
    $this->addFieldMapping('taxonomy_vocabulary_13', 'taxonomy_vocabulary_13')
         ->sourceMigration('NationalJurisdiction')
         ->arguments(array('source_type' => 'tid'));
    $this->addFieldMapping('taxonomy_vocabulary_8', 'taxonomy_vocabulary_8')
         ->sourceMigration('TectonicSetting')
         ->arguments(array('source_type' => 'tid'));
    $this->addFieldMapping('taxonomy_vocabulary_7', 'taxonomy_vocabulary_7')
         ->sourceMigration('MaxTemperatureCategory')
         ->arguments(array('source_type' => 'tid'));
    $this->addFieldMapping('taxonomy_vocabulary_6', 'taxonomy_vocabulary_6')
         ->sourceMigration('Ocean')
         ->arguments(array('source_type' => 'tid'));
    $this->addFieldMapping('taxonomy_vocabulary_5', 'taxonomy_vocabulary_5')
         ->sourceMigration('Activity')
         ->arguments(array('source_type' => 'tid'));
         
    
    $this->addFieldMapping('upload', 'upload');
    $this->addFieldMapping('upload:display', 'upload:display');
    $this->addFieldMapping('upload:description', 'upload:description');
    $this->addFieldMapping('field_depth', 'field_depth');
    $this->addFieldMapping('field_min_depth', 'field_min_depth');
    $this->addFieldMapping('field_description', 'field_description');
    $this->addFieldMapping('field_host_rock', 'field_host_rock');
    $this->addFieldMapping('field_max_temp', 'field_max_temp');
    $this->addFieldMapping('field_longitude4', 'field_longitude4');
    $this->addFieldMapping('field_organisms', 'field_organisms');
    $this->addFieldMapping('field_ventsite_txt', 'field_ventsite_txt');
    $this->addFieldMapping('field_deposit_type', 'field_deposit_type');
    $this->addFieldMapping('field_discoveryref_txt', 'field_discoveryref_txt');
    $this->addFieldMapping('field_latitude4', 'field_latitude4');
    $this->addFieldMapping('field_yr_mns_discovered', 'field_yr_mns_discovered');
    $this->addFieldMapping('field_otherref_txt', 'field_otherref_txt');
    $this->addFieldMapping('field_full_spread_rate_d', 'field_full_spread_rate_d');
    $this->addFieldMapping('field_name_alias', 'field_name_alias');
    
    /* 
    
    'field_geofield_lat_and_lon' may require 'migrate_extras' for the GeoField Handler
    
    $this->addFieldMapping('field_geofield_lat_and_lon', 'field_geofield_lat_and_lon');
    $this->addFieldMapping('field_geofield_lat_and_lon:geo_type', 'field_geofield_lat_and_lon:geo_type');
    $this->addFieldMapping('field_geofield_lat_and_lon:lat', 'field_geofield_lat_and_lon:lat');
    $this->addFieldMapping('field_geofield_lat_and_lon:lon', 'field_geofield_lat_and_lon:lon');
    $this->addFieldMapping('field_geofield_lat_and_lon:left', 'field_geofield_lat_and_lon:left');
    $this->addFieldMapping('field_geofield_lat_and_lon:top', 'field_geofield_lat_and_lon:top');
    $this->addFieldMapping('field_geofield_lat_and_lon:right', 'field_geofield_lat_and_lon:right');
    $this->addFieldMapping('field_geofield_lat_and_lon:bottom', 'field_geofield_lat_and_lon:bottom');
    $this->addFieldMapping('field_geofield_lat_and_lon:srid', 'field_geofield_lat_and_lon:srid');
    $this->addFieldMapping('field_geofield_lat_and_lon:accuracy', 'field_geofield_lat_and_lon:accuracy');
    $this->addFieldMapping('field_geofield_lat_and_lon:source', 'field_geofield_lat_and_lon:source');
    */
  }
}

class PageMigration extends IRVentMigration {
  public function __construct(array $arguments) {
    // Add any other data we're pulling into the source row, before the parent
    // constructor.

    parent::__construct($arguments);

  }
  
  /**
   * Implementation of Migration::prepareRow().
   *
   * Here we pull in additional data from a non-core, non-CCK table.
   *
   * @param $row
   */
  public function prepareRow($row) {
    if (parent::prepareRow($row) === FALSE) {
      return FALSE;
    }

    if( $row->nid == 1 ){
      //skip it
      return FALSE;
    }
  }
}

class StoryMigration extends IRVentMigration {
  public function __construct(array $arguments) {
    // Add any other data we're pulling into the source row, before the parent
    // constructor.

    parent::__construct($arguments);

  }
}
