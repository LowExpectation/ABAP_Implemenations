@AbapCatalog.sqlViewName: 'zv_hier_data2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.usageType.serviceQuality: #X
@ObjectModel.usageType.sizeCategory: 'XXL'
@ObjectModel.usageType.dataClass: #MIXED
@EndUserText.Label: 'Data for Hierarchy 2'

define view zcds_hierarchy_data2
    with parameters 
    p_togltri : co_gltri

// Get the materials (parents) and components (children)
// We may not have any components selected so we left join mara_child
as select from zcds_hierarchy_data3
                (p_togltri: $parameters.p_togltri) as parent_join
inner join mara as mara on parent_join.parent = mara.matnr
left outer join mara as mara_child on parent_join.child = mara_child.matnr
left outer join makt as makt on makt.matnr = mara_child.matnr
                             and spras = $session.system_language
left outer join afpo as _filter on _filter.matnr = parent_join.child
                                and (
                                    (
                                        _filter = parent_join.gltri
                                        and mara_child.mtart = 'HALB'
                                    )
                                    or mara_child <> 'HALB'
                                )

// Previous view entries
{
parent_join.gltri,
parent_join.parent,
parent_join.parent_batch,
parent_join.parent_plant,
parent_join.child,
parent_join.parent_order,
parent_join.child_plant,
parent_join.child_order,
parent_join.child_batch,
}
// Current view entries
cast( max( distinct _t.aufnr) as aufnr ) as aufnr,
cast( max( distinct _t_aufnr) as co_ltrmi) as co_ltrmi

// Make sure that no unintended components
where 
(
    mara_child.mtart = 'HLB'
    and _t.aufnr != ' '
)
or mara_child.mtart <> 'HLB'

group by
parent_join.gltri,
parent_join.parent,
parent_join.parent_batch,
parent_join.parent_plant,
parent_join.child,
parent_join.parent_order,
parent_join.child_plant,
parent_join.child_order,
parent_join.child_batch