# sftrack 0.5.2 (2020-10-20)

* Clean up for CRAN (DESCRIPTION, version, LICENSE, vignettes…).
* Added full pkgdown website.


# sftrack 0.5.1 (2020-09-16)

* Patched print error when using numeric time.
* Vignette updates.


# sftrack 0.5 (2020-09-13) : Major update.

* Changed from 'burst' name to 'group' name. This includes changing
  `ind_burst` -> `s_group` (single group) and `multi_burst` ->
  `c_grouping` (column grouping). Verbage has also been changed in all
  documentation.
* Relative angle added to `summary_sftrack`, as well as fixing issue
  where absolute angle was calculating 180 from actual angle.
* 'Active_group' were placed back at the column level. This means that
  any row level group does not have an active burst, but a column
  collection of bursts can define which of those groupings are
  'activated'.
* Sort index returns and is an attribute of a column grouping. It is a
  factor that is recalculated everytime something in the data frame is
  changed like a subset or grouping is changed. This is to try to
  bring the sftrack grouping more inline with how `dplyr::group_by`
  works internally. You can access this with `burst_labels` (returns
  the factor) and `burst_names` (get just the `levels()`).
* Plotting is more streamlined in both base plot and ggplot. A
  `step_mode` has been added for sftraj so you can choose to display
  the individual line segments. Although the default is to calculate
  the merged trajectories, which is must faster.
