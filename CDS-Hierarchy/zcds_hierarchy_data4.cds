@AbapCatalog.sqlViewName: 'zv_hier_data4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.usageType.serviceQuality: #X
@ObjectModel.usageType.sizeCategory: 'XXL'
@ObjectModel.usageType.dataClass: #MIXED
@EndUserText.Label: 'Data for Hierarchy 4'

define view zcds_hierarchy_data3
    with parameters 
    p_togltri : co_gltri

// Get the materials, orders, and batches per date
as select from afpo as afpo
inner join afko as afko on afpo.aufnr = afko.aufnr
inner join mara as mara on afpo.matnr = mara.matnr

{
// Distinct materials per Order, Batch, and Date 101 MIGO movement
// Later we will use these to get the components from the 261 entries within the 101
cast ( max( distinct afko.gltri ) as co_gltri) as parent_gltri,
cast ( max( distinct afko.bwtar ) as bwtar_d) as parent_batch,
cast ( max( distinct afpo.aufnr ) as aufnr) as parent_order,
afpo.matnr as parent,
afpo. dwerk as parent_plant
}
where
// This standard function can add or subtract dates on the fly so we are giving a three month range
// So that we can restict the data being pulled
// You could also use a constants table entry or AMDP to resolve the number used to subtract
( afko.gltri between ats_add_months( $parameters.p_togltri, -3, 'INITIAL ' ) and $parameters.p_togltri
    and mara.mtart = 'HALB' ) or
    ( afko.gltri = $parameters.p_togltri and (mara.mtart = 'FERT' or mara.mtart = 'HALB' ) )

group by
afpo.matnr,
afpo.dwerk