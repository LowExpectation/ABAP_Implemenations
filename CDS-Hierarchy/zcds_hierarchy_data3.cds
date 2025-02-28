@AbapCatalog.sqlViewName: 'zv_hier_data3'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.usageType.serviceQuality: #X
@ObjectModel.usageType.sizeCategory: 'XXL'
@ObjectModel.usageType.dataClass: #MIXED
@EndUserText.Label: 'Data for Hierarchy 3'

define view zcds_hierarchy_data3
    with parameters 
    p_togltri : co_gltri

// Get the materials (parents) and components (children)
// We may not have any components selected so we left join mara_child
as select from zcds_hierarchy_data4
                (p_togltri: $parameters.p_togltri) as parent_join
inner join afko as afko on parent_join.parent_order = afko.aufnr
inner join afpo as afpo on afko.aufnr = afpo.aufnr
inner join matdoc as matdoc on afko.aufnr = matdoc.aufnr
                            and afpo.bwtar = matdoc.charg_sid
left outer join afpo as component_order on component_order.bwtar = matdoc.charg_sid
                                        and component_order.matnr = matdoc.matbf                            

{
afko.gltri as gltri,
parent_join.parent,
parent_join.parent_batch,
parent_join.parent_plant,
matdoc.matbf as child,
parent_join.parent_order,
matdoc.werks as child_plant,
cast( max(component_order.aufnr) as aufnr) as child_order,
matdoc.charg_sid as child_batch
}

// Make sure that no unintended components
// During the production process 261 components are added to the 101 order during MIGO movement
where 
    matdoc.bwart = '261'
    and afko.gltri between dats_add_months( $parameters.p_togltri, -3, 'INITIAL ' ) and $parameters.p_togltri

group by 
afko.gltri,
parent_join.parent,
parent_join.parent_batch,
parent_join.parent_plant,
matdoc.matbf,
parent_join.parent_order,
matdoc.werks,
matdoc.charg_sid