---
title: "Putah Creek Dashboard"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: fill
---

```{r message=FALSE, warning=FALSE, include=FALSE}
######################################################### Libraries Used #########################################################
library(heatmaply)
library(flexdashboard)
library(readxl)
library(plotly)
library(corrplot)
library(ggplot2)
library(ggdendro)
library(circlize)
library(dendextend)
library(colorspace)
library(gplots)
library(leaflet)
```

```{r, echo = FALSE}
####################################################### Reading in the Data #######################################################

Putah_Creek_CLEAN = read_excel("Putah Creek  CLEAN.xlsx")
Putah_Creek = Putah_Creek_CLEAN
```

```{r,message = FALSE, echo = FALSE}
################################################### Adding the Names of Months ###################################################

months = factor(format(Putah_Creek$Date,"%m"))
months = month.abb[months]
months = factor(months)
Putah_Creek$Month = months


years = factor(format(Putah_Creek$Date,"%Y"))
years = factor(years)
Putah_Creek$Year = years
Putah_Creek$Location = as.factor(Putah_Creek$Location)
```

```{r, echo = FALSE, results = "hide", warning = FALSE}
################################################# Reformatting Variable Structure #################################################

str(Putah_Creek)

Putah_Creek$Date = as.Date(Putah_Creek$Date, foramt = "%m/%d/%y")
Putah_Creek$ID = as.factor(Putah_Creek$ID)
Putah_Creek$Location = as.factor(Putah_Creek$Location)
Putah_Creek$Time = format(Putah_Creek$Time, format = "%H:%M:%S")
Putah_Creek$`Temp(C)` = as.numeric(Putah_Creek$`Temp(C)`, na.rm = TRUE)
Putah_Creek$`EC(us/cm)` = as.numeric(Putah_Creek$`EC(us/cm)`, na.rm = TRUE)
Putah_Creek$pH = as.numeric(Putah_Creek$pH, na.rm = TRUE)
# Refromatting either numerically, as a factor or by time

str(Putah_Creek)
# Now we can see how the variable type changed

Putah_Creek = data.frame(Putah_Creek)
# Converting it into a data frame so its easier to work with

# We will keep TN and drop the variables NH4-N, TDN, NO3-N
Putah_Creek = subset(Putah_Creek, select = -c(NH4.N.mg.L., TDN.mg.L., NO3.N.mg.L.))

# We will keep TP drop TDP, PO4-P, rest of P features 
Putah_Creek = subset(Putah_Creek, select = -c(TDP.mg.L., PO4.P.mg.L.))
```

Introduction {data-orientation=row data-navmenu="Introduction"}
=====================================  
Row
-------------------------------------
### Arboretum Waterway
![](/Users/CarlosMonsivais/Desktop/Arboretum2.jpg){ width=100px }  
  
### UC Davis Arboretum Putah Creek Waterway
This data anlysis dashboard was created to get a more insightful and data driven look at the water quality samples collected by the 
UC Davis Arboretum and Public Garden between 2016 and 2018.  My goal was to make the dashboard interactive and easy for users to play with so that there can be a data driven solution to management and maintenence in the Arboretum. This project combines my passion for data analysis, solving real world problems and bringing positivity to my community. I looked at water samples from 7 different locations throughout the UC Davis Arboretum waterway that can be seen on the Water Sample Locations Map tab above. Within the seven different locations, 9 different features from the data set that measure water quality including Temperature, Electrical Conductivity, pH, Turbidity, Total Phosphorus, Total Nitrogen, Dissolved Organic Nitrogen, Dissolved Organic Carbon, and Dissolved Organic Matter. For the study, I used summary statistics, a correlation plot, exploratory analysis visual graphs and the unsupervised machine learning method of hierarchal clustering to look for patterns between the locations in terms of water quality.  



About Features {data-orientation=rows data-navmenu="Introduction"}
=====================================  
Row 
-------------------------------------
### About Features
Below is a description of each feature that was analyzed in the study to measure water quality in the UC Davis Arboretum along the Putah Creek Waterway.  

Temperature: Shows the temperature of the water. Measured in C$^{\circ}$ (degrees Celsius).  

Electrical Conductivity: The ability of water to conduct an electrical current. Measured in us/cm (microSiemens).

pH: A measure of how acidic/basic water is. The range goes from 0 to 14, with 7 being neutral. pH less than 7 indicate acidity, whereas a pH of greater than 7 indicates a base.

Dissolved Organic Nitrogen (DON): In lakes and rivers originate from photosynthetic organisms (algae and plants) and excretion of nitrogenous waste by animals, but leachate from soil, sewage discharge, and atmospheric deposition. Measured in mg/L (milligrams per liter).    

Total Nitrogen (TN): The sum of total kjeldahl nitrogen (ammonia, organic and reduced nitrogen) and nitrate-nitrite. Measured in mg/L (milligrams per liter).

Total Phosphorus (TP): Phosphorus is a nutrient important for plant growth. Phosphorus originates from a variety of sources, many of which are related to human activities; major sources include human and animal wastes, soil erosion, detergents, septic systems and runoff from farmland or fertilized lawns. Measured in mg/L (milligrams per liter).  

Dissolved Organic Carbon (DOC):  The organic material dissolved in water. Results from decomposition of plants or animals.
Once this decomposed organic material contacts water it may partially dissolve. Measured in mg/L (milligrams per liter).   

Dissolved Organic Matter (DOM): consists of soluble organic materials derived from the partial decomposition of organic materials, including soil organic matter, plant residues, and soluble particles released by living organisms, including bacteria, algae, and plants. Measured in C:N ratio (carbon to nitrogen ratio).

Turbidity: the quality of being cloudy, opaque, or thick with suspended matter. Measured in ntu (nephelometric turbidity unit).

UC Davis Arboretum Map {data-orientation=rows data-navmenu="Introduction"}
=====================================  
### UC Davis Arboretum Map  
Below is a map of the UC Davis Arboretum waterway from which the water sample data was obtained. We can see that the locations are not ordered chronologically because for example location 7 was added to the water  
sampling data set after locations 1 through 6. Therefore, keep in mind throughout the study each location ID has a very specific site that it pertains to.

Row {data-height=700}
-------------------------------------
### Water Sample Locations Map
```{r}
Putah_Creek_Coordinates= read_excel("Putah_Creek_Coordinates.xlsx")
Putah_Creek_Coordinates = Putah_Creek_Coordinates
colnames(Putah_Creek_Coordinates) = c("Location", "Latitude", "Longitude")
Putah_Creek_Coordinates$Location = as.factor(Putah_Creek_Coordinates$Location)
leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(lng = -121.7567,lat = 38.5328, zoom = 15) %>%
  addCircleMarkers(
    lng = -121.7438, lat = 38.5401, radius = 15, color = "#FF0000",
    label = 'ID 1: Preschool',
    labelOptions = labelOptions(noHide = T, direction = 'right'))%>%
   addCircleMarkers(
    lng = -121.7415, lat = 38.5383, radius = 15, color = "#FFA500",
    label = 'ID 2: Wyatt Theater',
    labelOptions = labelOptions(noHide = T, direction = 'right'))%>%
   addCircleMarkers(
    lng = -121.7422, lat = 38.5364, radius = 15, color = "#FFFF00",
    label = 'ID 3: Mark Hall',
    labelOptions = labelOptions(noHide = T, direction = 'right'))%>%
   addCircleMarkers(
    lng = -121.7520, lat = 38.5331, radius = 15, color = "#008000",
    label = 'ID 4: Horse Barn',
    labelOptions = labelOptions(noHide = T, direction = 'right'))%>%
   addCircleMarkers(
    lng = -121.7590, lat = 38.5315, radius = 15, color = "#00FFFF",
    label = 'ID 5: Putah Creek',
    labelOptions = labelOptions(noHide = T, direction = 'right'))%>%
   addCircleMarkers(
    lng = -121.7644, lat = 38.5280, radius = 15, color = "#0000FF",
    label = 'ID 6: Outlet',
    labelOptions = labelOptions(noHide = T, direction = 'right'))%>%
   addCircleMarkers(
    lng = -121.7567, lat = 38.5328, radius = 15, color = "#800080",
    label = 'ID 7: Inlet',
    labelOptions = labelOptions(noHide = T, direction = 'right')) %>%
  addLegend("topright", colors = c("#FF0000", "#FFA500", "#FFFF00", "#008000", "#00FFFF", "#0000FF", "#800080"),
            labels= c("ID 1: Preschool", "ID 2: Wyatt Theater","ID 3: Mark Hall","ID 4: Horse Barn",
                      "ID 5: Putah Creek", "ID 6: Outlet", "ID 7: Inlet"))
```

Summary Statistics by Year{data-orientation=rows data-navmenu="Summary Statistics"}
=====================================  
Column 
-------------------------------------
### Summary Statistics by Year
```{r}
Year_all_summary_Year = na.omit(Putah_Creek) 

Year_all_summary_Year$Date <- Year_all_summary_Year$ID <- Year_all_summary_Year$Time <- Year_all_summary_Year$Month <-  Year_all_summary_Year$Location <- NULL

mtlabels = c(Temp.C. = "Temperature",
  EC.us.cm. = "Electrical Conductivity",
  pH = "pH",
  Turbidity.ntu. = "Turbidity",
  TP.mg.L. = "Total Phosphorus",
  TN.mg.L.. = "Total Nitrogen",
  DON.mg.L. = "Dissolved Organic Nitrogen",
  DOC.mg.L. = "Dissolved Organic Carbon",
  DOM.C.N.ratio. = "Dissolved Organic Matter")

library(desctable)
Year_all_summary_Year %>%
  group_by(Year) %>%
  desctable(labels = mtlabels, stats = list(
                                "Mean" = mean,
                                "SD" = sd,
                                "Median" = median,
                                "Min" = min,
                                "Max" = max)) %>%
  datatable
```


Summary Statistics by Location {data-orientation=rows data-navmenu="Summary Statistics"}
=====================================  
Column 
-------------------------------------
### Summary Statistics by Location
```{r}
Year_all_summary_Location = na.omit(Putah_Creek) 

Year_all_summary_Location$Date <- Year_all_summary_Location$ID <- Year_all_summary_Location$Time <- Year_all_summary_Location$Month <-  Year_all_summary_Location$Year <- NULL

mtlabels = c(Temp.C. = "Temperature",
  EC.us.cm. = "Electrical Conductivity",
  pH = "pH",
  Turbidity.ntu. = "Turbidity",
  TP.mg.L. = "Total Phosphorus",
  TN.mg.L.. = "Total Nitrogen",
  DON.mg.L. = "Dissolved Organic Nitrogen",
  DOC.mg.L. = "Dissolved Organic Carbon",
  DOM.C.N.ratio. = "Dissolved Organic Matter")

library(desctable)
Year_all_summary_Location %>%
  group_by(Location) %>%
  desctable(labels = mtlabels, stats = list(
                                "Mean" = mean,
                                "SD" = sd,
                                "Median" = median,
                                "Min" = min,
                                "Max" = max)) %>%
  datatable
```


Correlation Plot 
=====================================  
Column {data-width=100}
-------------------------------------
### Correlation Plot 
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Correlogram #########################################################

col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

cor_Putah_Creek = Putah_Creek[,5:13]
colnames(cor_Putah_Creek) = c("Temperature", "Electrical Conductivity", "pH", "Turbidity", "Total Phosphorus", "Total Nitrogen", "Dissolved Organic Nitrogen", "Dissolved Organic Carbon", "Dissolved Organic Matter")

cor_Putah_Creek = data.matrix(cor_Putah_Creek)
cor_Putah_Creek = cor(cor_Putah_Creek, use = "complete.obs")

corrplot(cor_Putah_Creek, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .55,
         addCoef.col = "black",
         tl.col = "black", tl.srt = 90, 
         sig.level = 0.01, insig = "blank", 
         diag = FALSE,
         title = "Correlation Plot for Water Quality Variables",
         tl.cex = 0.8,
         mar=c(0,0,1,0))
```


Interactive Graph Preface {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================  
Row 
-------------------------------------
### Interactive Graph Preface
I plotted scatter plots, density plots and line plots of the data. There is a description of the interactive capabilities of each graph type below, please read the descriptions before looking at the graphs to make the most of your experience using the visualization tools.  
    
      
      
Scatter Plots:  
The scatter plots are the features (such as Temperature, Electrical Conductivity, etc..) plotted over time to see how these features have changed in the Arboretum between 2016 and 2018. With this in mind, there will be color coded points that correspond to a location. For example, yellow corresponds to the Water Outlet location in the scatterplots, therefore every time you see a yellow point, that is the value of the feature for the Water Outlet at a specific time. You can hover over the points to see the exact values such as the location it belongs to, the value of the feature and the date the point was recorded. The graphs are interactive so if you double click the dots on the legend, such as the yellow dot the graph will make it so that you only look at the points for Water Outlet which corresponds to the yellow dots. To get out of this view double click on the graph again. You can also zoom into the graph to look at specific points by right clicking with the cursor on the graph and creating a window for the area you want to look at. Again, to get out of this view double click on the graph. There are icons on the top right part of the scatter plot that you can read and play around with to zoom into the graph or even download the plot as a png file.  
    
     
     
Density Plots:  
The density plots are the features (such as Temperature, Electrical Conductivity, etc..) plotted by location to see the different density of each by the location name. You can hover over the points to see the exact values such as the density and the temperature value it belongs to. You can also zoom into the graph to look at specific densities by right clicking with the cursor on the graph and creating a window for the area you want to look at. To get out of this view double click on the graph. There are icons on the top right part of the scatter plot that you can read and play around with to zoom into the graph or even download the plot as a png file.  
    
      
      
Line Plots (Median Values):  
The line plots for the median value of each location ID was used because overall, the data is skewed and in order to minimize this skewness I used the median. With this in mind, there are two lines on this plot. The orange line which is the median of the data is the overall median of the seven locations combined and the blue line is the median of each location for the selected feature. You can hover over the lines at each location ID to see the exact median values of each feature. The graphs are interactive so if you double click the dots on the legend, such as the blue line the graph will make it so that you only look at the blue line which is the line for the median at each individual location. To get out of this view double click on the graph again. You can also zoom into the graph to look at specific points by right clicking with the cursor on the graph and creating a window for the area you want to look at. Again, to get out of this view double click on the graph. There are icons on the top right part of the scatter plot that you can read and play around with to zoom into the graph or even download the plot as a png file.



Temperature {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### Temperature: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
#################################################### Subsetting by Location ID ####################################################

Location1 = subset(Putah_Creek, ID == 1)
Location2 = subset(Putah_Creek, ID == 2)
Location3 = subset(Putah_Creek, ID == 3)
Location4 = subset(Putah_Creek, ID == 4)
Location5 = subset(Putah_Creek, ID == 5)
Location6 = subset(Putah_Creek, ID == 6)
Location7 = subset(Putah_Creek, ID == 7)

############################################ Median of Data and Median by Location ID ############################################

# Median of the Data as a Whole
med_Putah_Creek = Putah_Creek[,5:13]
med_Putah_Creek = as.data.frame(lapply(med_Putah_Creek, median, na.rm = T))

# MEedian of Individual Location ID's
Location1 = Location1[,5:13]
median1 = as.data.frame(lapply(Location1, median, na.rm = T))

Location2 = Location2[,5:13]
median2 = as.data.frame(lapply(Location2, median, na.rm = T))

Location3 = Location3[,5:13]
median3 = as.data.frame(lapply(Location3, median, na.rm = T))

Location4 = Location4[,5:13]
median4 = as.data.frame(lapply(Location4, median, na.rm = T))

Location5 = Location5[,5:13]
median5 = as.data.frame(lapply(Location5, median, na.rm = T))

Location6 = Location6[,5:13]
median6 = as.data.frame(lapply(Location6, median, na.rm = T))

Location7 = Location7[,5:13]
median7 = as.data.frame(lapply(Location7, median, na.rm = T))

# Median Dataframe For Individual Locations
median_dataframe = rbind(median1, median2, median3, median4, median5, median6, median7)
median_dataframe$ID = as.factor(c(1,2,3,4,5,6,7))
median_dataframe$Location = as.factor(c("Preschool", "Wyatt Theater", "Mark Hall", "Horse Barn", "Putah Creek", "Outlet",
                                        "Inlet"))

#################################################### X-Axis for Median Plots ####################################################

# Need this for the x-axis of the median plots
trace_0 = as.data.frame(matrix(c(1,2,3,4,5,6,7), nrow = 1, ncol = 7))
colnames(trace_0) = c("Location1", "Location2", "Location3", "Location4", "Location5", "Location6", "Location7")
trace_0 = as.factor(trace_0)

################################################### Plots Temperature:Temp(C) ###################################################

# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "Temperature(C)",
  titlefont = f,
  range = c(0, 35)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~Temp.C.,
        color = ~Location,
        text = ~paste("Location: ",Location ,"<br>Temperature(C): ", Temp.C., "C<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
) %>%
  layout(title = "Temperature Levels in Water by Location", xaxis = x, yaxis = y,  annotations=legendtitle,
         legend = l)
```

Row {.tabset .tabset-fade}
-------------------------------------
### Temperature: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
Density_Putah_Creek = Putah_Creek[,5:13]
Density_Putah_Creek$Location = Putah_Creek$Location
Density_Putah_Creek = na.omit(Density_Putah_Creek)

# Temperature:Temp(C)
a = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = Temp.C., fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Density Plot of Temperature(C) by Location") +
  xlab("Temperature Values") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(a)
```

### Temperature: Line Plot (Median Values)
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Median Plot by ID
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "Temperature(C)",
  titlefont = f,
  range = c(0, 35)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~Temp.C., 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste("Location: ",Location ,"<br>Temperature(C): ", Temp.C.), hoverinfo = "text") %>%
  add_segments(x = 1, y = med_Putah_Creek[1,1], xend = 7, yend = med_Putah_Creek[1,1], name = 'Median of Data') %>%
  layout(title = "Median Temperature Levels in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```

Electrical Conductivity {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### Electrical Conductivity: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "Electrical Conductivity(us/cm)",
  titlefont = f,
  range = c(0, 1600)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~EC.us.cm.,
        color = ~Location,
        text = ~paste("Location: ",Location, "<br>microsiemens per centimeter: ", EC.us.cm., "<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
       ) %>%

   layout(title = "Electrical Conductivity Levels in Water by Time", xaxis = x, yaxis = y,  annotations=legendtitle,
          legend = l)

```

Row {.tabset .tabset-fade}
-------------------------------------
### Electrical Conductivity: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
# Electrical Conductivity: EC(us/cm)
b = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = EC.us.cm., fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Density Plot of Electrical Conductivity(us/cm) by Location") +
  xlab("Electrical Conductivity(us/cm)") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(b)
```

### Electrical Conductivity: Median Values
```{r}
# Median Plot by ID
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "Electrical Conductivity(us/cm)",
  titlefont = f,
  range = c(0, 1600)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~EC.us.cm., 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste("Location: ",Location ,"<br>microsiemens per centimeter: ", EC.us.cm.), hoverinfo = "text") %>%
  add_segments(x = 1, y = med_Putah_Creek[1,2], xend = 7, yend = med_Putah_Creek[1,2], name = 'Median of Data') %>%
     
  layout(title = "Median Electrical Conductivity in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```

pH {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### pH: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "pH",
  titlefont = f,
  range = c(6,10)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~pH,
        color = ~Location,
        text = ~paste("Location: ",Location, "<br>pH Level: ", pH, "<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
       ) %>%

   layout(title = "pH Levels in Water by Time", xaxis = x, yaxis = y,  annotations=legendtitle,
          legend = l)
```

Row {.tabset .tabset-fade}
-------------------------------------
### pH: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
# pH Levels: pH
c = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = pH, fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Density Plot of pH by Location") +
  xlab("pH") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(c)
```

### pH: Median Values
```{r}
# Median Plot by ID
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "pH",
  titlefont = f,
  range = c(6,10)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~pH, 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste( "Location: ",Location, "<br>pH Level: ", pH), hoverinfo = "text") %>%
 
  add_segments(x = 1, y = med_Putah_Creek[1,3], xend = 7, yend = med_Putah_Creek[1,3], name = 'Median of Data') %>%
     
  layout(title = "Median pH in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```

Turbidity {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### Turbidity: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "Turbidity(ntu)",
  titlefont = f,
  range = c(0,300)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~Turbidity.ntu.,
        color = ~Location,
        text = ~paste("Location: ",Location, "<br>nephelometric turbidity unit: ", Turbidity.ntu., "<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
       ) %>%

   layout(title = "Turbidity Levels in Water by Time", xaxis = x, yaxis = y,  annotations=legendtitle,
          legend = l)
```

Row {.tabset .tabset-fade}
-------------------------------------
### Turbidity: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
d = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = Turbidity.ntu., fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Density Plot of Turbidity(ntu) by Location") +
  xlab("Turbidity(ntu)") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(d)
```

### Turbidity: Median Values
```{r}
# Median Plot by ID
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "Turbidity(ntu)",
  titlefont = f,
  range = c(0,300)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~Turbidity.ntu., 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste("Location: ",Location, "<br>nephelometric turbidity unit: ", Turbidity.ntu.), hoverinfo = "text") %>%
  
  add_segments(x = 1, y = med_Putah_Creek[1,4], xend = 7, yend = med_Putah_Creek[1,4], name = 'Median of Data') %>%
     
  layout(title = "Median Turbidity Levels in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```


Total Phosphorus {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### Total Phosphorus: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "Total Phosphorus(mg/L)",
  titlefont = f,
  range = c(0,14)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~TP.mg.L.,
        color = ~Location,
        text = ~paste("Location: ",Location, "<br>milligrams per liter: ", TP.mg.L., "<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
       ) %>%

   layout(title = "Total Phosphorus Levels in Water by Time", xaxis = x, yaxis = y,  annotations=legendtitle,
          legend = l)
```

Row {.tabset .tabset-fade}
-------------------------------------
### Total Phosphorus: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
# Total Phosphorus: TP(mg/L)
e = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = TP.mg.L., fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Density Plot of Total Phosphorus(mg/L) by Location") +
  xlab("Phosphorus(mg/L)") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(e)
```

### Total Phosphorus: Median Values
```{r}
# Median Plot by ID
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "Total Phosphorus(mg/L)",
  titlefont = f,
  range = c(0,14)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~TP.mg.L., 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste("Location: ",Location, "<br>milligrams per liter: ", TP.mg.L.), hoverinfo = "text") %>%
  add_segments(x = 1, y = med_Putah_Creek[1,5], xend = 7, yend = med_Putah_Creek[1,5], name = 'Median of Data') %>%
     
  layout(title = "Median Total Phosphorus Levels in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```


Total Nitrogen {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### Total Nitrogen: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "Total Nitrogen(mg/L)",
  titlefont = f,
  range = c(0,18)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~TN.mg.L..,
        color = ~ID,
        text = ~paste("Location: ",Location, "<br>milligrams per liter: ", TN.mg.L.., "<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
       ) %>%

   layout(title = "Total Nitrogen Levels in Water by Time", xaxis = x, yaxis = y,  annotations=legendtitle,
          legend = l)
```

Row {.tabset .tabset-fade}
-------------------------------------
### Total Nitrogen: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
# Total Nitrogen: TN(mg/L )
h = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = TN.mg.L.., fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Density Plot of Total Nitrogen(mg/L) by Location") +
  xlab("Total Nitrogen(mg/L)") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(h)
```

### Total Nitrogen: Median Values
```{r}
# Median Plot by ID
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "Total Nitrogen(mg/L)",
  titlefont = f,
  range = c(0,18)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~TN.mg.L.., 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste("Location: ",Location, "<br>milligrams per liter: ", TN.mg.L..), hoverinfo = "text") %>%
  add_segments(x = 1, y = med_Putah_Creek[1,6], xend = 7, yend = med_Putah_Creek[1,6], name = 'Median of Data') %>%
     
  layout(title = "Median Total Nitrogen Levels in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```


Dissolved Organic Nitrogen {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### Dissolved Organic Nitrogen: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "Dissolved Organic Nitrogen(mg/L)",
  titlefont = f,
  range = c(0,3)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~DON.mg.L.,
        color = ~ID,
        text = ~paste("Location: ",Location,"<br>milligrams per liter: ", DON.mg.L., "<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
       ) %>%

   layout(title = "Dissolved Organic Nitrogen Levels in Water by Time", xaxis = x, yaxis = y,  annotations=legendtitle,
          legend = l)

```

Row {.tabset .tabset-fade}
-------------------------------------
### Dissolved Organic Nitrogen: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
j = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = DON.mg.L., fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Dissolved Organic Nitrogen(mg/L) by Location") +
  xlab("Dissolved Organic Nitrogen(mg/L)") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(j)
```

### Dissolved Organic Nitrogen: Median Values
```{r}
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "Dissolved Organic Nitrogen(mg/L)",
  titlefont = f,
  range = c(0,3)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~DON.mg.L., 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste("Location: ",Location,"<br>milligrams per liter: ", DON.mg.L.), 
        hoverinfo = "text") %>%
  add_segments(x = 1, y = med_Putah_Creek[1,7], xend = 7, yend = med_Putah_Creek[1,7], name = 'Median of Data') %>%
     
  layout(title = "Median Dissolved Organic Nitrogen Levels in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```


Dissolved Organic Carbon {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### Dissolved Organic Carbon: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))


f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "Dissolved Organic Carbon(mg/L)",
  titlefont = f,
  range = c(0,31)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~DOC.mg.L.,
        color = ~Location,
        text = ~paste("Location: ",Location, "<br>milligrams per liter: ", DOC.mg.L., "<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
       ) %>%

   layout(title = "Dissolved Organic Carbon Levels in Water", xaxis = x, yaxis = y,  annotations=legendtitle,
          legend = l)
```

Row {.tabset .tabset-fade}
-------------------------------------
### Dissolved Organic Carbon: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
m = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = DOC.mg.L., fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Density Plot of Dissolved Organic Carbon(mg/L) by Location") +
  xlab("Dissolved Organic Carbon(mg/L)") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(m)
```

### Dissolved Organic Carbon: Median Values
```{r}
# Median Plot by ID
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "Dissolved Organic Carbon(mg/L)",
  titlefont = f,
  range = c(0,31)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~DOC.mg.L., 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste("Location: ",Location, "<br>milligrams per liter: ", DOC.mg.L.),
        hoverinfo = "text") %>%
  add_segments(x = 1, y = med_Putah_Creek[1,8], xend = 7, yend = med_Putah_Creek[1,8], name = 'Median of Data') %>%
     
  layout(title = "Median Dissolved Organic Carbon Levels in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```





Dissolved Organic Matter {data-orientation=rows data-navmenu="Exploratory Data Analysis"}
=====================================     
Row 
-------------------------------------
### Dissolved Organic Matter: Scatter Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
# Plot Over Time
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Location by Colors",showarrow = FALSE)

l = list(
  font = list(
    family = "Times New Roman, monospace",
    size = 12
   ))

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Date",
  titlefont = f
)
y = list(
  title = "Dissolved Organic Matter(C:N ratio)",
  titlefont = f,
  range = c(0,40)
)
plot_ly(data = Putah_Creek, x = ~Date, y = ~DOM.C.N.ratio.,
        color = ~Location,
        text = ~paste("Location: ", Location,"<br>carbon/nitrogen ratio: ", DOM.C.N.ratio., "<br>Date(yr-mon-day): ", Date),
        hoverinfo = "text"
       ) %>%

   layout(title = "Putah Creek Dissolved Organic Matter Levels in Water", xaxis = x, yaxis = y,  annotations=legendtitle,
          legend = l)
```

Row {.tabset .tabset-fade}
-------------------------------------
### Dissolved Organic Matter: Density Plot
```{r, echo = FALSE, message = FALSE, warning = FALSE}
######################################################### Density Plots #########################################################
# Dissolved Organic Matter: DOM(C:N ratio)
n = ggplot(Density_Putah_Creek) + 
  geom_density(aes(x = DOM.C.N.ratio., fill = Location)) + 
  facet_wrap(~Location, ncol = 7) +
  ggtitle("Density Plot of Dissolved Organic Matter(C:N ratio) by Location") +
  xlab("Dissolved Organic Matter(C:N ratio)") +
  ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(size = 14, hjust = -0.99), legend.position = "none")
ggplotly(n)
```

### Dissolved Organic Matter: Median Values
```{r}
# Median Plot by ID
legendtitle = list(yref='paper',xref="paper",y=1.10,x=1.06, text="Line Representation",showarrow = FALSE)

f = list(
  family = "Times New Roman, monospace",
  size = 12,
  color = "#444"
)

x = list(
  title = "Location ID",
  titlefont = f
)
y = list(
  title = "Dissolved Organic Matter(C:N ratio)",
  titlefont = f,
  range = c(0,40)
)

plot_ly(data = median_dataframe, x = ~trace_0, y = ~DOM.C.N.ratio., 
        name = 'Median by Location', type = 'scatter', mode = 'lines',
        text = ~paste("Location: ", Location,"<br>carbon/nitrogen ratio: ", DOM.C.N.ratio.),
        hoverinfo = "text") %>%
  add_segments(x = 1, y = med_Putah_Creek[1,9], xend = 7, yend = med_Putah_Creek[1,9], name = 'Median of Data') %>%
     
  layout(title = "Putah Creek Median Dissolved Organic Matter Levels in Water by Location ID", xaxis = x, yaxis = y,  annotations=legendtitle)
```

```{r}
# Susbetting and Variables
HC_Putah_Creek1 = Putah_Creek[,5:13]
HC_Putah_Creek1$ID = Putah_Creek$ID
HC_Putah_Creek1$Month = Putah_Creek$Month
HC_Putah_Creek1$Year = Putah_Creek$Year
HC_Putah_Creek1 = na.omit(HC_Putah_Creek1)

# Year_all
Year_all = na.omit(HC_Putah_Creek1) 

# Subsetting by 2016
Year_2016 = subset(HC_Putah_Creek1, Year == 2016)


# Subsetting by 2017
Year_2017 = subset(HC_Putah_Creek1, Year == 2017)



# Subsetting by 2018
Year_2018 = subset(HC_Putah_Creek1, Year == 2018)
```

Hierarchal Clustering Preface {data-orientation=rows data-navmenu="Hierarchal Clustering"}
=====================================  
Row 
-------------------------------------
### Hierarchal Clustering Preface
The following graphs are circular dendrograms and heatmaps using hierarchal clustering and more specific using Ward's Method (ward.D). For the circular dendrograms and heatmaps, there are four plots including a circular dendrogram and heatmap for all the years (between 2016 and 2018) to see a more aggregated picture and a circular dendrogram and heatmap by year to see the yearly pattern change. With this in mind, it is very interesting to compare and contrast the changes over the years between the graphs. I decided to go ahead and use these graphs because as the quote goes "A picture is worth a thousand words" and a lot of insights can be gathered from the pictures. I believe a more visual depiction is the best way to show this data because it makes it readable for anyone since people can look at pictures easily and extract information, you don’t have to be very technically gifted to look at a picture and tell people what you see.  

Reading a Circular Dendrogram:
To read a circular dendrogram you want to look at the tree each part of the circle corresponds to. For example, if the first part of the tree you want to look at is Location 1 then you would look at the subset of the tree with the color red which corresponds to Location 1. Now that you are looking at that subset, you would then see what colors are in that part of the dendrogram and say there are similarities between Location 1 and the corresponding colors and therefore locations within that tree. 

Reading a Heatmap
To read the heatmaps, you want to see where there are color patterns between each variable. For example, if looking at the variable Temperature, we want to see similar shades of colors between variables and compare and contrast to see how those variables are similar within the given time span. 

These graphs are all very visual and require just a creative mind to pick up patterns and be able to compare why there are certain patterns within the clustering. However, this clustering was done using purely mathematics through the use of the Euclidean Distance Function, and then Hierarchal Clustering using Ward's Method, therefore these patterns are found using mathematics. This is a mathematical way to represent the relationship between the data through the years at the UC Davis Arboretum.



Circular Dendrograms {data-orientation=rows data-navmenu="Hierarchal Clustering"}
=====================================     
Column {.tabset .tabset-fade}
-------------------------------------
### Circular Dendrogram All Years 2016 to 2018
```{r}
# Dend Variable
Year_all_scale = scale(Year_all[,1:9])


Heat_Putah_Creek_all = as.matrix(Year_all_scale)

d_Putah_all = dist(Heat_Putah_Creek_all) 
hc_Putah_all = hclust(d_Putah_all, method = "ward.D")
Putah_label_all = levels(Year_all[,10])


dend_all = as.dendrogram(hc_Putah_all)

dend_all = rotate(dend_all, 1:43)

dend_all = color_branches(dend_all, k = 7)

labels_colors(dend_all) =
   rainbow_hcl(7)[sort_levels_values(
      as.numeric(Year_all[,10])[order.dendrogram(dend_all)]
   )]

labels(dend_all) = paste(as.character(Year_all[,10])[order.dendrogram(dend_all)],
                           "(",labels(dend_all),")", 
                           sep = "")

dend_all = hang.dendrogram(dend_all, hang_height=0.1)

dend_all = set(dend_all, "labels_cex", 0.6)




some_col_func <- function(n) rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))



# Circle 
par(mar = rep(0,4))
circlize_dendrogram(dend_all, labels= TRUE)
legend("topleft", legend = Putah_label_all, fill = rainbow_hcl(7), 
      title = "Location ID by Color: All Years 2016 to 2018",
      box.col = "white",
      cex = 0.65)
```

### Circular Dendrogram 2016
```{r}
# Dend Variable
Year_2016_scale = scale(Year_2016[,1:9])


Heat_Putah_Creek_2016 = as.matrix(Year_2016_scale)

d_Putah_2016 = dist(Heat_Putah_Creek_2016) 
hc_Putah_2016 = hclust(d_Putah_2016, method = "ward.D")
Putah_label_2016 = levels(Year_2016[,10])


dend_2016 = as.dendrogram(hc_Putah_2016)

dend_2016 = rotate(dend_2016, 1:43)

dend_2016 = color_branches(dend_2016, k = 7)

labels_colors(dend_2016) =
   rainbow_hcl(7)[sort_levels_values(
      as.numeric(Year_2016[,10])[order.dendrogram(dend_2016)]
   )]

labels(dend_2016) = paste(as.character(Year_2016[,10])[order.dendrogram(dend_2016)],
                           "(",labels(dend_2016),")", 
                           sep = "")

dend_2016 = hang.dendrogram(dend_2016, hang_height=0.1)

dend_2016 = set(dend_2016, "labels_cex", 0.6)


# Circle 
par(mar = rep(0,4))
circlize_dendrogram(dend_2016, labels= TRUE)
legend("topleft", legend = Putah_label_2016, fill = rainbow_hcl(7), 
      title = "Location ID by Color: 2016",
      box.col = "white",
      cex = 0.65)
```


### Circular Dendrogram 2017
```{r}
# Dend Variable
Year_2017_scale = scale(Year_2017[,1:9])


Heat_Putah_Creek_2017 = as.matrix(Year_2017_scale)

d_Putah_2017 = dist(Heat_Putah_Creek_2017) 
hc_Putah_2017 = hclust(d_Putah_2017, method = "ward.D")
Putah_label_2017 = levels(Year_2017[,10])

dend_2017 = as.dendrogram(hc_Putah_2017)

dend_2017 = rotate(dend_2017, 1:117)

dend_2017 = color_branches(dend_2017, k = 7)

labels_colors(dend_2017) =
   rainbow_hcl(7)[sort_levels_values(
      as.numeric(Year_2017[,10])[order.dendrogram(dend_2017)]
   )]

labels(dend_2017) = paste(as.character(Year_2017[,10])[order.dendrogram(dend_2017)],
                           "(",labels(dend_2017),")", 
                           sep = "")

dend_2017 = hang.dendrogram(dend_2017, hang_height=0.1)

dend_2017 = set(dend_2017, "labels_cex", 0.6)

# Circle 
par(mar = rep(0,4))
circlize_dendrogram(dend_2017)
legend("topleft", legend = Putah_label_2017, fill = rainbow_hcl(7), 
      title = "Location ID by Color: 2017",
      box.col = "white",
      cex = 0.65)
```

### Circular Dendrogram 2018
```{r}
# Dend Variable
Year_2018_scale = scale(Year_2018[,1:9])


Heat_Putah_Creek_2018 = as.matrix(Year_2018_scale)

d_Putah_2018 = dist(Heat_Putah_Creek_2018) 
hc_Putah_2018 = hclust(d_Putah_2018, method = "ward.D")
Putah_label_2018 = levels(Year_2018[,10])

dend_2018 = as.dendrogram(hc_Putah_2018)

dend_2018 = rotate(dend_2018, 1:117)

dend_2018 = color_branches(dend_2018, k = 7)

labels_colors(dend_2018) =
   rainbow_hcl(7)[sort_levels_values(
      as.numeric(Year_2018[,10])[order.dendrogram(dend_2018)]
   )]

labels(dend_2018) = paste(as.character(Year_2018[,10])[order.dendrogram(dend_2018)],
                           "(",labels(dend_2018),")", 
                           sep = "")

dend_2018 = hang.dendrogram(dend_2018, hang_height=0.1)

dend_2018 = set(dend_2018, "labels_cex", 0.6)

# Circle 
par(mar = rep(0,4))
circlize_dendrogram(dend_2018)
legend("topleft", legend = Putah_label_2018, fill = rainbow_hcl(7), 
      title = "Location ID by Color: 2018",
      box.col = "white",
      cex = 0.65)
```

### Circular Dendrogram Side by Side Comparison
```{r}
# Circle 
par(mar = rep(0,4))
circlize_dendrogram(dend_all, labels= TRUE)
legend("topleft", legend = Putah_label_all, fill = rainbow_hcl(7), 
      title = "Location ID by Color: All Years",
      box.col = "white",
      cex = 0.65)

par(mar = rep(0,4))
circlize_dendrogram(dend_2016, labels= TRUE)
legend("topleft", legend = Putah_label_2016, fill = rainbow_hcl(7), 
      title = "Location ID by Color: 2016",
      box.col = "white",
      cex = 0.65)

par(mar = rep(0,4))
circlize_dendrogram(dend_2017)
legend("topleft", legend = Putah_label_2017, fill = rainbow_hcl(7), 
      title = "Location ID by Color: 2017",
      box.col = "white",
      cex = 0.65)

# Circle 
par(mar = rep(0,4))
circlize_dendrogram(dend_2018)
legend("topleft", legend = Putah_label_2018, fill = rainbow_hcl(7), 
      title = "Location ID by Color: 2018",
      box.col = "white",
      cex = 0.65)
```

Heatmaps {data-orientation=rows data-navmenu="Hierarchal Clustering"}
=====================================     
Row {.tabset .tabset-fade}
-------------------------------------
### Heatmap All Years 2016 to 2018
```{r}
gplots::heatmap.2(Heat_Putah_Creek_all,
          main = "Heatmap for All Years 2016 to 2018",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend_all,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend_all)), # to add nice colored strips        
          col = some_col_func
         )
```

### Heatmap 2016 
```{r}
gplots::heatmap.2(Heat_Putah_Creek_2016,
          main = "Heatmap for 2016",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend_2016,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend_2016)), # to add nice colored strips        
          col = some_col_func
         )
```
          
### Heatmap 2017
```{r}
gplots::heatmap.2(Heat_Putah_Creek_2017,
          main = "Heatmap for 2017",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend_2017,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend_2017)), # to add nice colored strips        
          col = some_col_func
         )
```

### Heatmap 2018
```{r}
gplots::heatmap.2(Heat_Putah_Creek_2018,
          main = "Heatmap for 2018",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend_2018,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend_2018)), # to add nice colored strips        
          col = some_col_func
         )
```

### Heatmaps Side by Side Comparison
```{r}
gplots::heatmap.2(Heat_Putah_Creek_all,
          main = "Heatmap for All Years 2016 to 2018",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend_all,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend_all)), # to add nice colored strips        
          col = some_col_func
         )

gplots::heatmap.2(Heat_Putah_Creek_2016,
          main = "Heatmap for 2016",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend_2016,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend_2016)), # to add nice colored strips        
          col = some_col_func
         )

gplots::heatmap.2(Heat_Putah_Creek_2017,
          main = "Heatmap for 2017",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend_2017,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend_2017)), # to add nice colored strips        
          col = some_col_func
         )

gplots::heatmap.2(Heat_Putah_Creek_2018,
          main = "Heatmap for 2018",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend_2018,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend_2018)), # to add nice colored strips        
          col = some_col_func
         )
```
