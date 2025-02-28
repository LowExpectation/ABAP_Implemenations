@AbapCatalog.sqlViewName: 'zv_hier_start'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.usageType.serviceQuality: #X
@ObjectModel.usageType.sizeCategory: 'XXL'
@ObjectModel.usageType.dataClass: #MIXED
@EndUserText.Label: 'Start Hierarchy'

// Base level that feeds into the hierarchy data service
// All translations should be completed before this view
define view zcds_hierarchy_start
with parameters
p_togltri : co_gltri
as select from zcds_hierarchy_data1
(p_togltri: $parameters.p_togltri)

association [0..*] to zcds_hierarchy_start as _recursion on $projection.parent = _recursion.child
                                                            $projection.parent_batch = _recursion.child_batch
                                                            $projection.parent_order = _recursion.child_order

{
    parent,
    child,
    gltri,
    parent_batch,
    parent_plant,
    parent_order,
    child_order,
    child_batch,
    _recursion
}