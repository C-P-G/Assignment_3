# Assignment_3
Quellendashboard

A shiny dashboard for the Assignment 3 in Geovisulization. 
## Data overview

The dashboard is set up on data from the Nationalpark Berchtesgaden, which has been collected in the course of the spring monitoring. The used data stems from three datasets:

1.  Ranger monitoring database. The spring monitoring of physical and chemical parameters started in 2012. The in table 1 shown parameters are measured every two to three weeks from now 19 springs (started with 7). 

```{r echo = FALSE, results = 'asis', warning=FALSE, message=FALSE}
library(knitr)
library(dplyr)
library(kableExtra)
 NPV_Monitoring <- read.csv2("data/Quellmonitoring.csv")
kable(NPV_Monitoring[1:5,], caption = "Raw Monitoring data example") %>%
kable_styling(latex_options = c("stripped", "scale_down"))
```

2. Hourly water temperature. In 2014 the monitoring was expanded by permanently installed Thermobuttons. Through the fixed measurements, hourly (every three hours in winter) water temperature data are provided.

```{r echo = FALSE, results = 'asis', warning=FALSE, message=FALSE}
library(knitr)
 Thermobuttons <- read.csv2("data/alleThermobuttons.csv")
kable(Thermobuttons[155:160,], caption = "Raw Thermobutton data example")
```

3.  The Coordinates of the springs. The coordinates were extracted of a larger spatial dataset containing all recorded springs in the park. The coordinates were then merged with the temperature and ranger datasets.



### Objective of the Dashboard

The objective of this dashboard is to provide a tool for the Nationalpark to easily create and download custom graphs and data for specific research questions.

## Use of Shinydashboards

The choice regarding a dashboard fell on R and Shiny as these tools are available and accessible for the Nationalpark.

### Benefits of Shiny dashboards

-   Easy use when accostumed to R.
-   Customization with html and CSS possibe 

### Disadvantages of Shiny dashboards
-   Employment only possible with R, so with RStudio Cloud etc. which are then necessary. Or a more complicated solution is necessary. Compared with ArcGis Dashboards publishing the dashboards seems a bit more straightforward. 

## Summary 
The dashboards fulfills the main functions (to generate customized graphs and data) it was expected to perform. But it is to note, that user experience can still be enhanced by adding additional information. The dashboard is a work in progress and will for a while be continously be extended and maintained. Additions as further data analysis options and details to the specific springs might be added, if the dashboard will be published on an public domain and not just for internal use. 
