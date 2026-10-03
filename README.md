# Morning Report
MoveApps

Github repository: *github.com/movestore/MorningReport*

## Description
This App provides an interactive update of your (recent) tracking data. It gives a table of all animals with first and last timestamps, number of positions and moved distance during the past 24h and 7d (relative to a reference date). Furthermore, it allows interactive plotting of selected data attributes as well as daily properties for each individual and a time range of up to 20 weeks (about 4.5 months) before the reference date. In addition, an interactive map for the selected individual and time range is plotted. 

## Documentation
This App lets the user interactively explore the input Movement data set, which is usually downloaded from Movebank and contains data from presently deployed and running tags. It gives a `Morning Report` of how the animals and tags are/were doing.

In the tab "Settings" different settings can be adjusted. The *reference timestamp* (either user-defined or by default NOW) can be adjusted. This reference timestamp is used as end timestamp for all plots and the map. For reference the last timestamp of the data set is displayed. The *migration buffer* and the *mortality buffer* can be set. And the *reference position* can be selected by clicking on the map or by default the last location will be selected.

In the tab "Visualization", on the top an overview table is displayed providing a data overview: For each animal and tag it shows the first and last timestamp (in UTC and in the local time zone of the last position), the number of positions and distance traveled (in km) during the last 24 hours or 7 days, respectively. If the animal was migrating, dead or had no data during the last 7 days then the appropriate event will be indicated (see the migration and mortality buffer settings below). In the left column of the table, single individuals can be selected for plotting selected properties and a map in the lower part. If no individual is selected, the first individual of the table that has locations in the selected time interval is shown.

In the sidebar one can select the column name according to which the rows in the table shall be ordered (in ascending or descending order). The time slider allows the selection of the start timestamp which has been defined to be at the most 20 weeks before the reference. All plots and the map react on the time slider. 
Below the time slider, three drop down selection menus allow to define which data attribute or calculated daily data property shall be shown in the three plots.
The reference location or in its absence the last location of the track is used as the reference point for the Net Square Displacement and the average daily distance. The displacement per day is calculated for each single calendar day.

The leaflet map on the right side shows the track of the selected animal in the selected time interval (from time slider selected time to reference timestamp). All positions are shown as blue points, connected by light blue lines. The most recent 3 positions are highlighted in red so that the present location of the animal can easily be picked out. The points and lines can be disabled and enabled. The background map can be selected as StreetMap (OpenStreetMap), TopoMap or Aerial. Note that the map can easily be zoomed in and out.

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

  - `Date` and `Time`: reference timestamp towards which all analyses are performed. Generally (and by default) this is NOW in UTC, especially if in the field and looking for one or the other animal or wanting to make sure that it is still doing fine. When analyzing older data sets, this parameter can be set to other timestamps so that the data fall into the 20 week time interval possible to explore. In this case make sure to unselect the "NOW" check box. As a reference the last timestamp of the data set is displayed. This chosen timestamp is also noted in the time slider in the sidebar of the Visualization tab.

  - `Always use date and time 'NOW'`: *Check* this box to use the 'NOW' date and time of each run of a scheduled workflow. When this app is part of a scheduled workflow, each time the workflow runs, it will use the date and time of moment when the app is executed. *Uncheck* the box if you want to use a fixed date and time. Box is checked by default.

  - `Migration buffer in km (last 7 days)`: user-defined distance (in km) that an animal of the respective species is expected to minimally move during up to 7 days during migration. This variable is used to define the event `migration` that is reported in the overview table: if any position of the last 7 days is further than this distance from the mean position of the last 7 days. The default value is presently set to 100 km. Units km.

  - `Mortality buffer in m (last 7 days)`: user-defined distance (in m) that an animal of the respective species is expected to minimally move during 7 days if it is alive. Take into account the data resolution, which can also miss longer displacements. This variable is used to define the event `dead` that is reported in the overview table: if all positions of the last 7 days are within this distance of their mean position. At least 2 positions in the last 7 days are needed to report `dead`. The default value is presently set to 100 m. Units m.

  - `Reference position`: select reference position, by clicking on the map, to which average daily distances are to be calculated. Typically this is the observer position in the field, if one wants to find out if any tagged animals are in the surroundings. The coordinates can also be typed into the `Lat` and `Lon` fields below the map; both are needed. If no location is selected, the last position of each track is used. 
**NOTE:** currently when the settings are stored in a previous session, and the app is opened again, the reference position will not appear on the map, but the coordinates are still saved and shown on the bottom left below the map.

  - `Reset to last position of track`: Click to remove selected position from the map and the `Lat`/`Lon` fields, and use the last position of the selected animal as a reference point.

Tab "Visualization":
  - `Show Plots and Map`: choose which track should be shown on the plot.
  
  - `Select property by which to sort the overview table`: choose column by which to sort the table above.
  
  - `Choose how to sort the table`: choose if the table should be sorted ascending or descending according to the column chosen above.
  
  - `Choose start time for map and plots (reference: yyyy-mm-dd HH:MM:SS)`: selection of the start timestamp which has been defined to be at the most 20 weeks before the reference (selected in Tab Settings).
  
  - `Select which data attribute to plot`/`Select which second data attribute to plot`: select which data attribute should be shown in the first and the second plot. All numeric attributes of the data that are not empty can be selected, apart from identifiers (time, track id, columns ending in `_id` and 64-bit integer columns such as checksums).
  
  - `Select which daily property to plot`: select which calculated daily property should be shown in the third plot.

### Changes in output data

The input data remains unchanged.

### Most common errors

- **The plots and map show a message instead of data:** the selected animal has no locations between the selected start time and the reference time. This is typical when the reference time is NOW (default) but the tags stopped sending data some time ago. Check the last timestamp of the animal in the overview table, and select another animal, move the start time slider or adapt the reference time in the tab "Settings" (and uncheck the "NOW" box).

### Null or error handling
**Setting `Date` and `Time`:** By default the reference time is set to NOW. The present timestamp is extracted in UTC from the MoveApps server system. If the selected animal does not have any locations within the 20 weeks before the selected reference time (default NOW), a message is displayed instead of the plots and map. Check the time span of the data in the overview table in the tab "Visualization", adjust the date and uncheck the "NOW" box.

**Setting `Reference position`:** If no location is chosen, for each animal the last available position is used as reference for the calculation of the net square displacement and the average daily distances to a position.

**Setting `Migration buffer in km (last 7 days)`:** The parameter has an explicit default value. If the field is left empty, the event `migration` is not evaluated.

**Setting `Mortality buffer in m (last 7 days)`:** The parameter has an explicit default value. If the field is left empty, the event `dead` is not evaluated.

**Data without coordinate reference system:** The App stops with an error if the input data have no coordinate reference system (CRS).
