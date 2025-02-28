@AccessControl. authorizationCheck: #NOT_REQUIRED
@EndUserText.Label: 'Define Hierarchy'

// This is the start of our CDS hierarchy loop
define hierarchy zcds_hierarchy_output
// These parameters allow us to input values to help guide the loop
 with parameters
    p_matnr : matnr,
    p_werks : werks_d,
    p_frombatch : charg_d,
    p_tobatch : charg_d,
    p_fromgltri: co_gltri,
    p_togltri : co_gltri

// This gives guidance on the hierarchy structure
// Example Parent 1 -> Child 1, Child 2
// Child 1 and Child 2 become the parents in the next loop
// Parent 2 Parent 3 and so on until the lineage stops
as parent child hierarch(

// This view/entity will be the one that drives the selection logic for populating data
    source zcds_hierarchy_start
        (p_fromgltri : p_fromgltri,
         p_togltri : p_togltri)
// Set the recursion/loop criteria
// Start with a certain material, certain date range, and or certain batch
    start where
    parent = $parameters.p_matnr
    and parent_plant = $parameters.p_werks
    and gltri between $parameters.p_fromgltri and $parameters.p_togltri
    and parent_batch between $parameters.p_frombatch and $parameters.p_tobatch

// How should we order each level
    siblings order by
    gltri descending
// Allow the value to be consumed in hierarchy
    nodetype parent_batch
//  Many parents are possible
    multiple parents allowed
// no broken hierarchies allowed
    orpahns ignore
// We want to preserve the hierarchy so children are not shown as own parents
    cycles breakup    
)

// Set the fields for output
{
    parent,
    child,
    child_plant,
    gltri
// $nodetypes are built in helper values to add
// These ones will show the rank and level of the loop 
    $nodetype.hierarchy_level,
    $nodetype.hierarchy_rank,
    child_batch,
    parent_order,
    child_order
    _recursion // make association known
}