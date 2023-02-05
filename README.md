# First Passage Time Segmentation

MoveApps

Github repository: *github.com/movestore/First-Passage-Time-Segm*

## Description
This App calculated the first passage time of all individual tracks. Depending on a time threshold, a map overview indicates for each animal which locations lie below (fast movement) and above (slow movement/resting) such a time.

## Documentation
Given one radius distance, this App calculated for each location of each animal the time it needs to cross this radius, i.e. the first passage time (FPT). This App uses the fpt() function from the adehabitatLT package.

For each animals it is possible to plot the FPT for each location with a time threshold line. This time threshold has to be initially defined and can be adapted in the User Interface (Shiny UI).

Depending on the time threshold, this App annotates each location with NA (not possible to calculate FPT), fast movement (low FPT) and slow movement/resting (high FPT). On a map the track of a selected animals is plotted together with the locations in their respective colour (NA is omitted).

For each location, the fpt value and behaviour (fast/slow movement) are annotated and returned in the output rds data set in the columns named "fpt_value" and "fpt_behaviour" respectively. That way they can be used in subsequent Apps, including csv file creating (rds2csv App) or shapfile creation (Write Shapefile).

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format
Shiny user interface (UI)

### Artefacts
none

### Parameters 
`Radius parameter for First Passage Time`: Define the radius for which you want to calculate the first passage times (when does an animal pass the radius). Unit: `metres`. Default 30000 m = 30 km.

`Threshold time for migration/resting`: Define the time threshold with which you will split your tracks into fast and slow movement locations. This is the minimum time your animals need to pass a given radius (above) if resting. Select your time unit below. Default 10 (days).

`Unit of the threshold time`: Select the time unit of your selected threshold time. Possible values `seconds`, `hours`, and `days`. Default `days`.

`Start/Update calculation`: after customizing or changing any of the parameters above, hit this button to do the calculation using the new input.

### Null or error handling:
**Parameter `Radius parameter for First Passage Time`:** A default of 30 km is provided, but can be changed to NULL, which will lead to an error. Take care to relate this parameter to the extent of your data set. If the radius is too large for the data set then all FPT values will be NA. If the radius is too small then FPTs will be extremely short. Negative values will not be tolerated and lead to an error.

**Parameter `Threshold time for migration/resting`:** The time threshold has to be positive and not NULL, a default of 10 days is provided. By shifting the slider in the UI, this parameter can be changed, but only to integer values. Initial values can be double. Negative values are not tolerated and lead to an error.

**Parameter `Unit of the threshold time`:** Radiobuttons with three possible values and default (see above). No null or error possibilities.

**Data:** The data are not manipulated in this App, but interactively explored. So that a possible Workflow can be continued after this App, the input data set is returned with the columns "fpt_value" and "fpt_behaviour" appended.
