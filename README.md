#  <img src='logo.png' align="right" height="120px" /> Eurostat Data Cube 

This is your unnofficial data cube gateway to [eurostat](https://ec.europa.eu/eurostat/en/), allowing you to use temporal census data in [xarray](https://docs.xarray.dev/en/latest/index.html), e.g., to combine it with satellite imagery.

## Motivation

Statistical data collection is designed to show the spatial distribution of features at a given reporting period.
This makes it easy to generate a map but difficult to analyse time series.
Some features are reported monthly, others annually.
In addition, statistical [NUTS](https://ec.europa.eu/eurostat/en/web/nuts) regions may change their name, id, and even boundaries over time, making it challenging to track what happened at a given geographical region.
This project provides a harmonised and aggregated view on eurostat data, providing arrays with constant spatiotemporal boundaries.

## Features

- follows xarray and [CF Conventions](https://cfconventions.org/)
- bulk download of eurostat including meta data
- All regions are aggregated or upsampled to NUTS3 as of version 2024
- All timespans are aggregated or upsampled to quarters
- Accounts for NUTS region splits and merges weighted by population using [nuts::nuts_convert_version](https://docs.ropensci.org/nuts/reference/nuts_convert_version.html)

## Build

This repo only contains the code required to build the actual datacube. To build your own, run:

```bash
git clone https://github.com/danlooo/eurostat-datacube
cd eurostat-datacube
docker compose up --build
```

Alternatively , it can be excuted rootlessly using Apptainer/Singularity:

```bash
singularity run --bind $PWD/_targets:/work/_targets --workdir /work docker://danlooo/eurostat-datacube \
    bash -c 'cd /work && R -e "targets::tar_make()"'
```

## Disclaimer

This project is a re-analysis of the official data of eurostat.
Values were modified during aggragation.
The codebase may still contain errors that can lead to false interpretation of the data.
Use the products at your own risk.
We are not affiliated with eurostat.


## Funding

<p>
<a href = "https://earthmonitor.org/">
<img src="https://earthmonitor.org/wp-content/uploads/2022/04/european-union-155207_640-300x200.png" align="left" height="50" />
</a>

<a href = "https://earthmonitor.org/">
<img src="https://earthmonitor.org/wp-content/uploads/2022/04/OEM_Logo_Horizontal_Dark_Transparent_Background_205x38.png" align="left" height="50" />
</a>
</p>

This project has received funding from the [Open-Earth-Monitor Cyberinfrastructure](https://earthmonitor.org/) project that is part of European Union's Horizon Europe research and innovation programme under grant [101059548](https://cordis.europa.eu/project/id/101059548).
