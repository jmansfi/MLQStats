# MLQStats

Coding for operating the Major League Quadball Stats Dashboard hosted on [mlquadball.com](https://www.mlquadball.com/stats/2023)


## Description

This repository contains two R files:
* ```MLQStats.R```, coding for creating the stats dashboard and
* ```TeamStatCalcs.R```, a file for doing additional data cleaning to calculate team stats

This repository contains the following additional files:
* ```webcss.css```, a file including the css instructions for dashboard formatting.
* ```playerstats.csv, playoffstats.csv, seriesstats.csv, teamstat.csv```, files included for necessary dashboard functions.

## Getting Started

### Dependencies

Requires the following packages:
* data.table
* stringr
* shiny
* DT
* leaflet

### Executing program

* This dashboard can be run as is from the MLQStats.R file, as long as the approriate .css and .csv files listed above are in the same directory.
* TeamStatCalcs requires input currently pulled from a local SQL database - please message me if you would like the additional data required for these calculations, but they are not needed to run the dashboard as is.

## Authors

Joshua Mansfield<br />
josh.mansfield93@gmail.com

## Versions

Version 1, May 2023
* Player stats table added
Version 1.1, June 2023
* CSS, and series stats table added
Version 1.2, July 2023
* Stats per game operations and team stat (map) added
Version 1.3, October 2023
* Playoff stats added

## Acknowledgments

Thank you to MLQ Commissioner Amanda Dallas and MLQ Gameplay Director Ryan Davis for their help and support in concept design for this application.

Thank you to the MLQ Stats team, including Ryan Hsu, Misha Whittingham, and Sinan Keyder for help in collecting all stats for the 2023 season. This dashboard could not have been done without you all.