# Stellwagen Cable Routing
Cable routing analysis for Stellwagen National Marine Sanctuary

## Data sources
| **Layer** | **Data Source** | **Download Link** | **Metadata**  | **Notes** |
|---------------|---------------|---------------|---------------|---------------|
| Ocean disposal sites | NOAA / OCM | [Ocean disposal sites](https://marinecadastre.gov/downloads/data/mc/OceanDisposalSite.zip) | [Metadata](https://www.fisheries.noaa.gov/inport/item/54193) | [MarineCadastre](https://hub.marinecadastre.gov/datasets/noaa::ocean-disposal-sites) |
| Intertidal flats |||[Metadata]()||
| Preliminary offshore sand resources APTIM Technical Report No. 631226219 (2018) | Massachusetts CZM | [Sand patches](https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::preliminary-offshore-sand-resources-aptim-technical-report-no-631226219-2018) | [Metadata](https://www.arcgis.com/sharing/rest/content/items/be12362142734c6d8e50392dd219eec5/info/metadata/metadata.xml?format=default&output=html) | [RESTService](https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Preliminary_offshore_sand_resources_APTIM_Technical_Report_No_631226219_2018/FeatureServer/0)|
| National Channel Framework - Channel Area (ACOE) | CZM MORIS | [Channel areas](https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::national-channel-framework-channel-area-acoe) | [Metadata](https://mass-eoeea.maps.arcgis.com/sharing/rest/content/items/9227967a2748410983352b501c0c7b39/info/metadata/metadata.xml?format=default&output=html) | [RESTService](https://services7.arcgis.com/n1YM8pTrFmm7L4hs/ArcGIS/rest/services/National_Channel_Framework/FeatureServer/1) |
| Anchorages |NOAA / OCM| [Anchorage areas](https://marinecadastre.gov/downloads/data/mc/Anchorage.zip) | [Metadata](https://www.fisheries.noaa.gov/inport/item/48849) | [MarineCadastre](https://hub.marinecadastre.gov/datasets/noaa::anchorages) |
| Northeast Ocean Data |---------------| [Eelgrass meadows](https://www.northeastoceandata.org/files/metadata/Themes/Habitat.zip) | [Metadata](https://www.northeastoceandata.org/files/metadata/Themes/Habitat/EelgrassMeadows.pdf) | --------------- |
| Cable and pipelines |---------------|[Cable and pipelines](https://www.northeastoceandata.org/files/metadata/Themes/EnergyAndInfrastructure.zip)|[Metadata](https://www.northeastoceandata.org/files/metadata/Themes/EnergyAndInfrastructure/CableAndPipelineAreas.htm)|---------------|
| Submarine cables |---------------|[Submarine cables](https://www.northeastoceandata.org/files/metadata/Themes/EnergyAndInfrastructure.zip)|[Metadata](https://www.northeastoceandata.org/files/metadata/Themes/EnergyAndInfrastructure/SubmarineCables.htm)|---------------|
| Neptune LNG Pipeline | Massachusetts CZM | [Liquid natural gas](https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::neptune-lng-pipeline) | [Metadata](https://www.arcgis.com/sharing/rest/content/items/0df6b39f58444cc9a236755b75d1c92b/info/metadata/metadata.xml?format=default&output=html) | [RESTService](https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Neptune_LNG_Pipeline/FeatureServer/1) |
| Northeast Gateway LNG Pipeline | Massachusetts CZM | [Liquid natural gas](https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::northeast-gateway-lng-pipeline) | [Metadata](https://www.arcgis.com/sharing/rest/content/items/7826f80140934c869571dfb2b46f0313/info/metadata/metadata.xml?format=default&output=html) | [RESTService](https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Northeast_Gateway_LNG_Pipeline/FeatureServer/1) |
| Algonquin Hubline LNC Pipeline | Massachusetts CZM | [Liquid natural gas](https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::algonquin-hubline-lnc-pipeline) | [Metadata](https://www.arcgis.com/sharing/rest/content/items/8cbe1bdd72a443a5bf04c2d50c78df10/info/metadata/metadata.xml?format=default&output=html) | [RESTService](https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Algonquin_Hubline_LNC_Pipeline/FeatureServer/1) |
| CONMAPSG | USGS | [CONMAPSG](https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg.zip) | [Metadata](https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg.htm)  ([text](https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg-metadata.txt)) | [FAQ](https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg-faq.htm), Data provided for the analysis are more expansive than this dataset |

## Methods
### 
| **Layer** | **Buffer distance (m)** | **Score** | **Notes** |
|---------------|---------------|---------------|---------------|
| Sediment (CONMAPSG) - Sand/Mud | --------------- | 0.2 | |
| Sediment (CONMAPSG) - Mix | --------------- | 0.4 | |
| Sediment (CONMAPSG) - Gravel | --------------- | 0.6 | |
| Sediment (CONMAPSG) - Rock | --------------- | 1.0 | |
| Cod Spawning IBS (High)/Telemetry (UD 50) | --------------- | 0.8 | |
| Cod Spawning IBS (Low)/Telemetry (UD 90) | --------------- | 0.2 | |
| Active and Inactive Disposal Sites | 500 | 0.8 | |
| Intertidal Flats | 500 | 0.2 | |
| Sand Patches | 500 | 1.0 | |
| Channel Areas | 600 | 1.0 | |
| Anchorage Areas | 600 | 0.8 | |
| Eelgrass Meadows | 675 | 0.4 | |
| Cable and Pipeline Areas | 675 | 0.4 | |
| Submarine Cables | 675 | 0.4 | |
| LNG Pipelines | 675 | 0.4 | |

### Barriers
| **Layer** | **Buffer distance (m)** | **Notes** |
|---------------|---------------|---------------|
| Coral points | 675 | Provided by MA CZM |
| Sites to avoid | 1000 | Provided by NMS |
| Boulder ridges | 500 | Provided by NMS |
| Cape Cod shore | 500 | Generated by MA CZM |

## Documented issues
7 August 2024

1. 

8 August 2024

1. [Submarine cable area data](https://www.northeastoceandata.org/files/metadata/Themes/EnergyAndInfrastructure.zip) by [Northeast Ocean Data](https://www.northeastoceandata.org/data-download/) differ than the [data](https://hub.marinecadastre.gov/datasets/noaa::submarine-cable-areas/) provided by [MarineCadastre](https://hub.marinecadastre.gov/). Northeast Ocean Data provided data got integrated due to past use in previous model iterations. However, the [metadata document](https://www.northeastoceandata.org/files/metadata/Themes/EnergyAndInfrastructure/SubmarineCables.htm) suggest they are originally from ENC Direct (see [map services](https://nauticalcharts.noaa.gov/learn/encdirect/#map-services)). When comparing data between the two providers there exist differences and discrepancies. The document does not detail enough why these discrepancies exist or how to reconcile them.
2. [Northeast Ocean Data](https://www.northeastoceandata.org/data-download/) [cable and pipeline data](https://www.northeastoceandata.org/files/metadata/Themes/EnergyAndInfrastructure.zip) differ from [data](https://marinecadastre.gov/downloads/data/mc/PipelineArea.zip) hosted on [MarineCadastre](https://hub.marinecadastre.gov/). The [metadata document](https://www.northeastoceandata.org/files/metadata/Themes/EnergyAndInfrastructure/CableAndPipelineAreas.htm) suggests the Northeast Ocean Data data got created by combining ENC Direct data at the approach and harbor data for the submarine cable areas and the pipelines. Still unclear why and how differences exist across the datasets.

3 September 2024
1. Shoreline limit for Cape Cod seems to come from the USGS global islands dataset. Is this dataset preferable to the Massachusetts [Coastal Zone Management dataset on shorelines between 1800 - 2014](https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/783773f353f846a4ae0d5f4dcbcb9919_3)? The USGS global islands dataset got published in 2019 -- data were from 2014 as well. Which is the more authoritative data source?