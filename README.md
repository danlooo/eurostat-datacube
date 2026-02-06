#  <img src='logo.png' align="right" height="120px" /> Eurostat Data Cube 

This is your unnofficial data cube gateway to [eurostat](https://ec.europa.eu/eurostat/en/), allowing you to use census data in time series analyses and combine it with satellite imagery.

# Motivation

Statistical data collection is designed to show the spatial distribution of features at a given reporting period.
This makes it easy to generate a map but difficult to analyse time series.
Some features are reported monthly, others annually.
In addition, statistical [NUTS](https://ec.europa.eu/eurostat/en/web/nuts) regions may change their name, id, and even boundaries over time, making it challenging to track what happened at a given geographical region.
This project provides a harmonised and aggregated view on eurostat data, providing arrays with constant spatiotemporal boundaries.