# Morning Report

MoveApps

Github repository: *github.com/movestore/MorningReport*

## Description
This App provides an interactive update of your (recent) tracking data. It gives a table of all animals with first and last timestamps, number of positions and moved distance during the past 24h and 7d (relative to a reference date). Furthermore, it allows interactive plotting of selected data attributes as well as daily properties for each individual and a time range of up to 20 weeks (about 4.5 months) before the reference date. In addition, an interactive map for the selected individual and time range is plotted.

## Documentation
This App lets the user interactively explore the input Movement data set, which is usually downloaded from Movebank and contains data from presently deployed and running tags. It gives a `Morning Report` of how the animals and tags are/were doing.

In the tab "Settings" different settings can be adjusted. The *reference timestamp* (either user-defined or by default NOW) can be adjusted. This reference timestamp is used as end timestamp for all plots and the map. For reference the last timestamp of the data set is displayed. The *migration buffer* and the *mortality buffer* can be set. And the *reference position* can be selected by clicking on the map or by default the last location will be selected.

In the tab "Visualization", on the top an overview table is displayed providing a data overview: For each animal and tag it shows the first and last timestamp (the last timestamp also in the local time zone of the last position), the number of positions and distance traveled (in km) during the last 24 hours or 7 days, respectively. If the animal was migrating, dead or had no data during the last 7 days then the appropriate event will be indicated (see the migration and mortality buffer settings below). In the left column of the table, single individuals can be selected for plotting selected properties and a map in the lower part. If no individual is selected, the first individual of the table that has locations in the selected time interval is shown.

In the sidebar one can select the column name according to which the rows in the table shall be ordered (in ascending or descending order). The time slider defines the time window for the plots and the map as the number of days before the reference timestamp (at most 140 days = 20 weeks); the resulting start and end timestamps are shown below the slider. All plots and the map react on the time slider.
Below the time slider, three drop down selection menus allow to define which data attribute or calculated daily data property shall be shown in the three plots.
The reference location or in its absence the last location of the track is used as the reference point for the Net Square Displacement and the average daily distance. The displacement per day is calculated for each single calendar day (UTC) and includes the step from the last position of the previous day.

The leaflet map on the right side shows the track of the selected animal in the selected time interval (the selected number of days before the reference timestamp). All positions are shown as blue points, connected by light blue lines. The most recent 3 positions are highlighted in red so that the present location of the animal can easily be picked out. The points and lines can be disabled and enabled. The background map can be selected as StreetMap (OpenStreetMap), TopoMap or Aerial. Note that the map can easily be zoomed in and out.

### Application scope
#### Generality of App usability

This App was developed for any taxonomic group.

#### Required data properties

The App should work for any kind of (location) data. Specially useful for live feed data.

The data must have a coordinate reference system (CRS). Projected data are transformed internally to longitude/latitude (EPSG:4326) for the calculations and maps; the output data keep their original projection.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
none

### Settings
Tab "Settings":

`Date` (date) and `Time (24h format)` (time): reference timestamp towards which all analyses are performed. Generally (and by default) this is NOW in UTC, especially if in the field and looking for one or the other animal or wanting to make sure that it is still doing fine. When analyzing older data sets, this parameter can be set to other timestamps so that the data fall into the 20 week time interval possible to explore. In this case make sure to unselect the "NOW" check box. As a reference the last timestamp of the data set is displayed. This chosen timestamp is also shown below the time slider in the sidebar of the Visualization tab. Unit: UTC, time in hours and minutes. Default: NOW.

`Always use date and time 'NOW'` (alwaysNOW): *Check* this box to use the 'NOW' date and time of each run of a scheduled workflow. When this app is part of a scheduled workflow, each time the workflow runs, it will use the date and time of moment when the app is executed. *Uncheck* the box if you want to use a fixed date and time. Default: checked.

`Migration buffer in km (last 7 days)` (mig7d_dist): user-defined distance that an animal of the respective species is expected to minimally move during up to 7 days during migration. This variable is used to define the event `migration` that is reported in the overview table: if any position of the last 7 days is further than this distance from the mean position of the last 7 days. Unit: `km`. Default: `100`.

`Mortality buffer in m (last 7 days)` (dead7d_dist): user-defined distance that an animal of the respective species is expected to minimally move during 7 days if it is alive. Take into account the data resolution, which can also miss longer displacements. This variable is used to define the event `dead` that is reported in the overview table: if all positions of the last 7 days are within this distance of their mean position. At least 2 positions in the last 7 days are needed to report `dead`. Unit: `m`. Default: `100`.

`Reference position` (click on the map; `Lat` (posLat) and `Lon` (posLon)): select reference position, by clicking on the map, to which the net square displacement and the average daily distances are calculated. Typically this is the observer position in the field, if one wants to find out if any tagged animals are in the surroundings. The coordinates can also be typed into the `Lat` and `Lon` fields below the map; both are needed. Unit: decimal degrees. Default: none, the last position of each track is used.
**NOTE:** currently when the settings are stored in a previous session, and the app is opened again, the reference position will not appear on the map, but the coordinates are still saved and shown on the bottom left below the map. Typed coordinates are also not drawn on the map.

`Reset to last position of track` (set2Na): Click to remove selected position from the map and the `Lat`/`Lon` fields, and use the last position of the selected animal as a reference point.

Tab "Visualization":

`Show Plots and Map` (C): choose which track should be shown in the plots and the map. Default: the first track of the table that has locations in the selected time interval.

`Select property by which to sort the overview table` (sort_table): choose column by which to sort the table above. Default: animal name.

`Choose how to sort the table` (sort_direc): choose if the table should be sorted ascending or descending according to the column chosen above. Default: ascending.

`Choose time window for map and plots (days before the reference time)` (days_back): length of the time window shown in the plots and the map, counted back from the reference timestamp (selected in Tab Settings). The resulting start and end timestamps are shown below the slider. As the window is stored as a number of days, a stored setting keeps the same window length when the reference time changes, e.g. with "Always use date and time 'NOW'" in a scheduled workflow. Unit: days. Default: `140` (20 weeks, the maximum).

`Select which data attribute to plot` (attr) / `Select which second data attribute to plot` (attr2): select which data attribute should be shown in the first and the second plot. All numeric attributes of the data that are not empty can be selected, apart from identifiers (time, track id, columns ending in `_id` and 64-bit integer columns such as checksums). Default: net square displacement (km2).

`Select which daily property to plot` (prop): select which calculated daily property should be shown in the third plot: number of positions per day, displacement per day (km) or average daily distance to the selected position (km). Default: number of positions per day.

### Changes in output data

The input data remains unchanged.

### Errors and null handling

**Common error:** The plots and map show a message instead of data: the selected animal has no locations in the selected time window (the selected number of days before the reference time). This is typical when the reference time is NOW (default) but the tags stopped sending data some time ago. Check the last timestamp of the animal in the overview table, and select another animal, increase the time window with the slider or adapt the reference time in the tab "Settings" (and uncheck the "NOW" box).

**Setting `Date` and `Time`:** By default the reference time is set to NOW. The present timestamp is extracted in UTC from the MoveApps server system. If the selected animal does not have any locations within the 20 weeks before the selected reference time (default NOW), a message is displayed instead of the plots and map. Check the time span of the data in the overview table in the tab "Visualization", adjust the date and uncheck the "NOW" box.

**Setting `Reference position`:** If no location is chosen, for each animal the last available position is used as reference for the calculation of the net square displacement and the average daily distances to a position.

**Setting `Migration buffer in km (last 7 days)`:** The parameter has an explicit default value. If the field is left empty, the event `migration` is not evaluated.

**Setting `Mortality buffer in m (last 7 days)`:** The parameter has an explicit default value. If the field is left empty, the event `dead` is not evaluated.

**Setting `Reference position` with typed coordinates:** If the typed latitude is not between -90 and 90 or the longitude not between -180 and 180, a message is displayed instead of the net square displacement and average daily distance plots.

**Setting `Select which data attribute to plot`:** If the selected attribute has no values for the selected animal in the selected time interval, a message is displayed instead of the plot. If a stored attribute does not exist in the data (e.g. the workflow now uses other data), a message asks to select another attribute.

**Input data:** The App stops with an error if the input data have no coordinate reference system (CRS).

### Technical details

- The App is an interactive R Shiny App. The data are processed per track. Track properties that do not depend on the settings (first and last timestamp, local time zone) are calculated once at the start. The numbers of positions, moved distances and the distances used for the events are recalculated when the reference time changes; when only a buffer changes, just the comparison with the buffers is repeated.
- Projected data are transformed with `sf::st_transform()` to longitude/latitude (EPSG:4326). All distances are calculated on the ellipsoid with `geosphere::distVincentyEllipsoid()`, the displacement during the last 24h and 7d with `move2::mt_distance()`.
- Events: the mean position of the last 7 days is calculated with `geosphere::geomean()` (correct across the 180° meridian). `migration` is reported if any position of the last 7 days is further than the migration buffer from this mean, `dead` if all of them (at least 2) are within the mortality buffer, `no data` if there are no positions in the last 7 days, otherwise `-`.
- The local time zone of the last position is looked up with `lutz::tz_lookup_coords()` (method "accurate"); the local time is calculated with `lubridate::with_tz()` for the exact moment of the last position (including daylight saving time) and shown with the time zone abbreviation.
- All timestamps, the reference time and the calendar days of the daily properties are in UTC.
- Locations within a track are assumed to be ordered by time: the last position of a track is the last location in the data.
- Selected settings are stored with the "Store settings" button (Shiny bookmark) and restored in the next run of the workflow.
