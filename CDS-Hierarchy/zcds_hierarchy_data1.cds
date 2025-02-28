@AbapCatalog.sqlViewName: 'zv_hier_data1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.usageType.serviceQuality: #X
@ObjectModel.usageType.sizeCategory: 'XXL'
@ObjectModel.usageType.dataClass: #MIXED
@EndUserText.Label: 'Data for Hierarchy 1'

define view zcds_hierarchy_data1
    with parameters 
    p_togltri : co_gltri

// Get the materials by plant information
as select from zcds_hierarchy_data2
                (p_togltri: $parameters.p_togltri) as parent_join
association [1..1] to marc as marc on parent_join.parent = marc.matnr
                                    and parent_join.parent_plant = marc.werks_d

{
// Previous view entries
parent_join.gltri,
parent_join.parent,
parent_join.parent_batch,
parent_join.parent_plant,
parent_join.child,
parent_join.parent_order,
parent_join.child_plant,
parent_join.child_order,
parent_join.child_batch

}
// Make sure that no unintended components
where
(
    parent_join.child_type = 'HALB'
    and parent_join.aufnr != ' '
)
or parent_join.child_type <> 'HALB'