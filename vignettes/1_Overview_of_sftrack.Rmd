---
title: "1. Overview of sftrack"
output:
  pdf_document: default
  html_document: default
vignette: |
  %\VignetteIndexEntry{1. Overview of sftrack}
   %\VignetteEncoding{UTF-8}
   %\VignetteEngine{knitr::rmarkdown}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, include =FALSE}
#devtools::load_all("/home/matt/r_programs/sftrack")
library(sftrack)

```
*This vignette acts as a proto how-to manually mainly for highlighting features for anyone testing sftrack. It will eventually evolve into the full vignette.*

We'll begin with a brief overview of an `sftrack` object. An `sftrack` object is a data object that describes the movement of a subject, it has x,y coordinates (sometimes z), some measurement of time (clock time or a sequence of integers), and some grouping variable that identifies the subjects. For the spatial aspects of `sftrack` we are using the package `sf`, this powerful tool lets us quickly calculate spatial attributes and plot with ease with its full integration with `rgdal`. 

An `sftrack` object has 4 parts to it, 3 of which are required:  
 - **Geometry** This is stored as an `sfc` object from `sf`. Accepts x,y,z coordinates.  
 - **Burst** This is the grouping variables, which contains at minimum an `id` field to identify the subject. Grouping variables can be turned on at various stages based on the 'active' burst, allowing for the column to be more dynamic than a standard grouping column.  
 - **Time** Either a POSIX object or an integer.  
 - **Error** (optional). This is the field with error data in it. At present the column name is being stored as an attribute, but it does not have any real functionality yet.  

There are two different classes of objects:

#### sftrack objects
An `sftrack` object is a data.frame where the geometries are stored as a `POINT` for each row.

#### sftraj object
An sftraj object is a data.frame where the geometries are stored as a `LINESTRING` for each row. The linestring is a line built from the two points at t1 -> t2. This means that when an sftraj object is modified the steps likely need to be recalculated. Internally anytime active_bursts are changed, then the geometry is recalculated.

## Definitions

## Theory
