# CDS Views

This view will allow for an example on how to recusively read the database based on a logical hierarchy. This example is one that I built from a couple of years ago so it does not use entities or associations like the newer data definitions but it should be a great start on how to get an object with these concepts moving forward.

## Logical breakdown

This is based on a Product Costing approach to identify material and component hierarchies using the order number and batch. The standard SAP process will handle this but the requirements for this were a specific reporting type which I removed from this example for fairness.

The basic flow is for a material to be created and push through the production process. It will have several MIGO movement types applied such as movement 101 for the material and 261 movement types for the components. These will be able to be hierarchically categorized based on the production order and batch associated during these MIGO movements.
