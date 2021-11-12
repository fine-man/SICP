# Multiple Representation for Abstract Data

## Intro
Previously we used data abstraction, a way of structuring systems in such a way that the programs manipulating data can be
specified independent of the choices involed in implementing the data objects themselves.

For example we created data-objects for rational numbers and structured the procedures in such a way that for procedures like
(add-rat, mul-rat, sub-rat) the underlying representation of rational numbers didn't matter. The idea in implementing all our
data-abstractions was to erect an abstraction barrier between the representation(constructors and selectors) and the usage of 
compound-data (in the above case : add-rat, mul-rat, sub-rat)

These data-abstraction are powerful tools for controlling complexity of a system but there are some downsides as well.
- There may be more than one useful representation for a data object, for example complex numbers can have two representations
rectangular and polar forms, both of which are equally useful.

The way we solve the above problem is by using *generic procedures* which are procedures that can operate on data that may be
represented in more than one way.

we implement *generic procedures* the following way:
- data objects with *type tags*, that is data objects that include explicit information for how they are to be processed
- *generic selectors* - selectors which select parts of a data-object irrespective of their representation

