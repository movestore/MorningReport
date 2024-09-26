# Morning Report
MoveApps

Github repository: *github.com/movestore/MorningReport*

## Description
This App provides an interactive update of your (recent) tracking data. It gives a table of all animals with first and last timestamps, number of positions and moved distance during the past 24h and 7d (relative to a reference date). Furthermore, it allows interactive plotting of selected data attributes as well as daily properties for each individual and a time range of up to 20 weeks (5 months) before the reference date. In addition, an interactive map for the selected individual and time range is plotted. 

## Documentation
This App lets the user interactively explore the input Movement data set, which is usually downloaded from Movebank and contains data from presently deployed and running tags. It gives a `Morning Report` of how the animals and tags are/were doing.

In the tab "Settings" different settings can be adjusted. The *reference timestamp* (either user-defined or by default NOW) can be adjusted. This reference timestamp is used as end timestamp for all plots and the map. For reference the last timestamp of the data set is displayed. The *migration buffer* and the *mortality buffer* can be set. And the *reference position* can be selected by clicking on the map or by default the last location will be selected.

In the tab "Visualization", on the top an overview table is displayed providing a data overview: For each animal and tag it shows the first and last timestamp, the number of positions and distance traveled during the last 24 hours or 7 days, respectively. If the animal was migrating, dead or had no data during the last 7 days then the appropriate event will be indicated. In the left column of the table, single individuals can be selected for plotting selected properties and a map in the lower part.

In the sidebar one can select the column name according to which the rows in the table shall be ordered (in ascending or descending order). The time slider allows the selection of the start timestamp which has been defined to be at the most 5 months before the reference. All plots and the map react on the time slider. 
Below the time slider, three drop down selection menus allow to define which data attribute or calculated daily data property shall be shown in the three plots.

The leaflet map on the right side shows the track of the selected animal in the selected time interval (from time slider selected time to reference timestamp). All positions are shown as blue points, connected by light blue lines. The most recent 5 positions are highlighted in red so that the present location of the animal can easily be picked out. The points and lines can be disabled and enabled. The openstreetmap background map can be selected as StreetMap or Aerial. Note that the map can easily be zoomed in and out.

### Application scope
#### Generality of App usability

This App was developed for any taxonomic group. 

#### Required data properties

The App should work for any kind of (location) data. Specially useful for live feed data.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
none

### Settings 
Tab "Settings":

  - `Date` and `Time`: reference timestamp towards which all analyses are performed. Generally (and by default) this is NOW in UTC, especially if in the field and looking for one or the other animal or wanting to make sure that it is still doing fine. When analyzing older data sets, this parameter can be set to other timestamps so that the data fall into the 5 month time interval possible to explore. In this case make sure to unselect the "NOW" check box. As a reference the last timestamp of the data set is displayed. This chosen timestamp is also noted in the time slider in the sidebar of the Visualization tab.

  - `Always use date and time 'NOW'`: *Check* this box to use the 'NOW' date and time of each run of a scheduled workflow. When this app is part of a scheduled workflow, each time the workflow runs, it will use the date and time of moment when the app is executed. *Unckeck* the box if you want to use a fixed date and time. Box is checked by default.

  - `Migration buffer in km (last 7 days)`: user-defined distance (in km) that an animal of the respective species is expected to minimally move during up to 7 days during migration. This variable is used to define the event `migration` that is reported in the overview table. The default value is presently set to 100 km. Units km.

  - `Mortality buffer in m (last 7 days)`: user-defined distance (in m) that an animal of the respective species is expected to minimally move during 7 days if it is alive. Take into account the data resolution, which can also miss longer displacements. This variable is used to define the event `dead` that is reported in the overview table. The default value is presently set to 100 m. Units m.

  - `Reference position`: select reference position, by clicking on the map, to which average daily distances are to be calculated. Typically this is the observer position in the field, if one wants to find out if any tagged animals are in the surroundings. If no location is selected, the last position of each track is used. 
**NOTE:** currently when the settings are stored in a previous session, and the app is opened again, the reference position will not appear on the map, but the coordinates are still saved and shown on the bottom left below the map.

  - `Reset to last position of track`: Click to remove selected position, and use the last position of the selected animal as a reference point. *Bug*: currently the "selected location" remains displaying on the map, but the has no effect.

Tab "Visualization":
  - `Show Plots and Map`: choose which track should be shown on the plot.
  
  - `Select property by which to sort the overview table`: choose column by which to sort the table above.
  
  - `Choose how to sort the table`: choose if the table should be sortet ascending or descending acording to the column chosen above.
  
  - `Choose start time for map and plots (reference: yyyy-mm-dd HH:MM:SS)`: selection of the start timestamp which has been defined to be at the most 5 months before the reference (selected in Tab Settings).
  
  - `Select which data attribute to plot`/`Select which second data attribute to plot`: select which data attribute should be shown in the first and the second plot.
  
  - `Select which daily property to plot`: select which calculated daily property should be shown in the third plot.

### Changes in output data

The input data remains unchanged.

### Most common errors


### Null or error handling
*Setting `Date` and `Time`:** By default the reference time is set to NOW. The present timestamp is extracted in UTC from the MoveApps server system. An error will be displayed if the data set does not contain any locations of the previous 5 months before the selected reference time (default NOW). Check the time span of the data in the overview table in the tab "Visualization" and adjust the date and unckeck the "NOW" box.

**Setting `Reference position`:** If no location is chosen, for each animal the longitude of the last available position is used as reference for the calculation of average daily distances to a position.

**Setting `Migration buffer in km (last 7 days)`:** The parameter has a explicit default value, so NULL or non-numeric values are not possible and will give an error.

**Setting `Mortality buffer in m (last 7 days)`:** The parameter has a explicit default value, so NULL or non-numeric values are not possible and will give an error.