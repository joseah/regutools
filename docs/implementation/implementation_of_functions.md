---
title: Implementation of `Regutools` functions in R
author: José Alquicira Hernández
---

# Get biological elements within a genomic location

- **Function name**: `getElements()`
- **Description**: This function retrieves all or some biological objects within a defined genomic physical interval
- **Parameters**:
  + **start**. Initial position in Kbs 
  + **end**. Final position in Kbs
  + **type**. Retrive only some biological elements (promoter, gene, binding site)
  + **conn**. Connection to sqlite database
- **Output**: A data frame with two columns.
  + **id**: object id
  + **name**: object name
  + **start**: start position
  + **end**: end position
  + **strand**: strand location
  + **type**: object type
- **Example**:

```r
res <- getElements(start = 100000, end = 150000, 
                   type = c("promoter","gene"), 
                   con = conn)
```

- **Validations**
 + Return error if `start` and `end` values are not numbers
 + Return error if interval is out of range (invalid genomic positions)
 + Return error if biological type elements to be retrieved are invalid and print a message of possible values
 + If no element is found, return an empty vector and print a message
 + Validate if provided variable to `con` is indeed a connection
 + Object physical position is contained in provided range at least in 1 base pair


# Get attributes and relationships of biological elements

- **Function name**: `getAttr()`
- **Description**: This function retrieves all or some attributes and relationships of one or more biological elements
- **Parameters**:
  + **attributes**. Vector of attributes to be obtained
  + **filters**. Vector of filters to make specific searches
  + **values**. Vector of values associated with each filter
  + **mart**. Type of information to be retrieved. (Gene, Promoter, TU, Operon, ...) 
  + **conn**. Connection to sqlite database

- **Output**: A data frame with n columns.
  + **columns**
      + **n columns**. Corresponding to requested attribute(s)
- **Example**:

```r
res <- getRG(attributes = c("gene_id", "gene_name", "gene_type"), 
      filters = c("gene_strand", "gene_type"),
      values = c("forward", "Pseudo Gene"), 
      mart = "gene",
      conn = con)
```
- **Validations**
  + Return error if biological type attributes to be retrieved are invalid and print a message suggesting to use `listAttributes()` function
  + Return error if provided filters are invalid. Print a message to suggest using `ListFilters` function
  + Validate if filters and values vector are the same size
  + Validate if provided variable to `conn` is indeed a connection
  + If nothing is found, return an empty data.frame and print a message

# List all available attributes in Marts
    
- **Function name**: `listAttributes()`
- **Description**: This function returns all available attributes which can be obtained from each Mart using the `getRG()` function
- **Parameters**:
  + **mart**. Type of information to be retrieved. (Gene, Promoter, TU, Operon, ...) 
  + **conn**. Connection to sqlite database

- **Output**: A data frame two columns.
  + **columns**
      + **name**
      + **description**
- **Example**:

```r
listAttributes(mart = "gene", con = conn)
```

- **Validations**
  + Return error if mart is invalid. Print all available marts
  + Validate if provided variable to `conn` is indeed a connection

# List all available filters in Marts
    
- **Function name**: `listFilters()`
- **Description**: This function returns all available filters which can be used for each Mart using the `getRG()` function
- **Parameters**:
  + **mart**. Type of information to be retrieved. (Gene, Promoter, TU, Operon, ...) 
  + **conn**. Connection to sqlite database

- **Output**: A data frame two columns.
  + **columns**
      + **name**
      + **description**
- **Example**:

```r
listFilters(mart = "gene", con = conn)
```

- **Validations**
  + Return error if mart is invalid. Print all available marts
  + Validate if provided variable to `conn` is indeed a connection