Repository description \[UNDER MAKING\]
================
Ivann Schlosser

# Folder structure

The folder `scripts` contains all the necessary code to reproduce the
results of the work on cycling networks for London. The folder
`reproducibility` contains a set of functions that run the scripts in
the right order to reproduce the results.

# Reproducibility

Aiming to have a reproducible workflow, we provide the necessary to be
able to reproduce as much as possible, given potential constraints
related to data sharing and other computational limitations. After
revisions, most of this work can be reproduced on an average power
personal laptop.

## Prerequisites

You will need recent versions of **R** (\> 4.0.0) and **python** (\>
3.8) for better results.

In order to set up and run this code, you need to first download the
github repository. To do so, either manually, or open a terminal and run
the following command:

``` bash
```

## Data

Due to it’s size, the data is stored separately and needs to be
downloaded and added to the repository in a folder called *data*. The
data can be downloaded “from this dropbox(?)”. It contains copies of
openly available data sets that were downloaded at the time of the work.

## Network setup

The workflow is divided into 3 main parts, first the network set up, it
uses data downloaded from [OSM](openstreetmap.org) with the *OSMNx*
package in python to get the network data. It will download 3 networks
for the different profiles considered in this work: *cycling*, *walking*
and *all*.

Run the following command in terminal to launch the network download
process.

``` bash

python3 /scripts/network/data_import.py
```

Or, using the provided data sets (pre downloaded, but in raw format.)
run the `network_setup.R` from the reproducibility folder. It will set
up already routable networks for all the profiles considered in the
work.

## Routing setup

The routing setup requires pre processing of the London MSOA data, we
need to find the different centroids. Run `area_setup.R` from the
reproducibility folder to do that. The routing process will take the
previously produced networks, do some preprocessing on them, find the
nodes from which the routing will be done, and generate distance
matrices. It will also produce summary statistics of the networks.

Run the following terminal command in order to do the routing:

``` bash
```

## Modelling setup

The spatial interaction model part of the work originally relied on a
set of R functions to be run, but since then, a package was made that
has an increased performance and allowed to run these models locally on
smaller machines quickly. The model outputs with both methods are the
same. These changes have been incorporated since to allow for easier
reproducibility.

Run the following command to do gravity models:

``` bash
```

This will populate the directory with the outputs and results that are
shown in the paper.

For any questions, bugs, additional info, do not hesitate to contact me
directly.

# Session info

Here is the basic information about the R it was last run on.

``` r
R.version
```

More details on the package versions will be added soon.

# Images

The images illustrate the number of users by transport mode at MSOA
level. This work will focus on pedestrians and cyclists.

<figure>
<img src="images/light/transport_use_map.jpg"
alt="Transport usage distribution" />
<figcaption aria-hidden="true">Transport usage distribution</figcaption>
</figure>

<figure>
<img src="images/light/shortest_paths_camden019.jpg"
alt="Example of paths from one point." />
<figcaption aria-hidden="true">Example of paths from one
point.</figcaption>
</figure>
