# First Passage Time Segmentation

MoveApps

Github repository: *github.com/movestore/First-Passage-Time-Segm*

## Description
This App calculated the first passage time of all individual tracks. Depending on a time threshold, a map overview indicates for each animal which locations lie below (migration) and above (resting) such a time.

## Documentation
Given one radius distance, this App calculated for each location of each animal the time it needs to cross this radius, i.e. the first passage time (FPT). This App uses the fpt() function from the adehabitatLT package.

For each animals it is possible to plot the FPT for each location with a time threshold line. This time threshold has to be initially defined and can be adapted in the User Interface (Shiny UI).

Depending on the time threshold, this App annotates each location with NA (not possible to calculate FPT), fast movement (low FPT) and slow movement/resting (high FPT). On a map the track of a selected animals is plotted together with the locations in their respective colour (NA is omitted).

For each location, the fpt value and behaviour (fast/slow movement) are annotated and returned in the output rds data set. That way they can be used in subsequent Apps, including csv file creating (rds2csv App) or shapfile creation (Write Shapefile).

### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format
Shiny user interface (UI)

### Artefacts
none

### Parameters 
`radius`: Defined radius the animal has to cross for first passage. Unit: `metres`. Default 30000 m = 30 km.

`time_thr`: Time threshold of the FPT to define migration/resting. This value can be adapted interactively. Selected unit below. Default 10 (days).

`thr_uni`: Threshold time unit. Possible values `seconds`, `hours`, and `days`. Default `days`.

### Null or error handling:
**Parameter `radius`:** A default of 30 km is provided, but can be changed to NULL, which will lead to an error. Take care to relate this parameter to the extent of your data set. If the radius is too large for the data set then all FPT values will be NA. If the radius is too small then FPTs will be extremely short. Negative values will not be tolerated and lead to an error.

**Parameter `time_thr`:** The time threshold has to be positive and not NULL, a default of 10 days is provided. By shifting the slider in the UI, this parameter can be changed, but only to integer values. Initial values can be double. Negative values are not tolerated and lead to an error.

**Parameter `thr_uni`:** Radiobuttons with three possible values and default (see above). No null or error possibilities.

**Data:** The data are not manipulated in this App, but interactively explored. So that a possible Workflow can be continued after this App, the input data set is returned.
