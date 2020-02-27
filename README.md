---
output: pdf_document
---
# IDA - Integrated Data Analysis
## Objectives
IDA is supposed to facilitate the work with data at Climate Analytics by centralizing data management and those data related task that can be easily automated. This automation ensures consistency of methodology and output.

The main tasks that IDA is supposed to be used for

* Harmonize data from a variety of sources, e.g. the World Development Indicators ([WDI](http://data.worldbank.org/data-catalog/world-development-indicators)), [UN World Population Prospects](http://esa.un.org/unpd/wpp/) or the [AR5 database](https://tntcat.iiasa.ac.at/AR5DB/dsd?Action=htmlpage&page=about).
* Each new datasource must only be handled once. A new data source to be included into IDA is read in, put into the common format, units and variable names are harmonized (As of early 2016, the variables names from the various IPCC related databases hosted at IIASA are used). Through version control, each user can access new data after updating his/her version from gitlab.
* Common tasks can be automated and carried out in R instead of Excel:
    * Automated handling of error prone tasks: unit converion, source management, ...
    * Allow flexible data aggregation both across space and time.
    * Calculation of growth rates
    * Applying the growth rate of one time series to another time series.
    * Downscaling e.g. IAM results from AR5 or LIMITS according to country level GDP or population figures from the SSP database.
* Allow flexible output generation in line with currently used tools. For instance, to be compatible with current CAT work, IDA must be able to provide Excel-Files in line with the styling standards of the CAT team.
* Allow flexible plotting of data:
    * Comparison of data for the same geographical entity from different sources or for different scenarios.
    
## Contents of the package
IDA contains both a harmonized and cleaned data set from a variety of sources as well as function that can be used to manipulate, export and visualize this data. If you load the package into an R installation, you have access to all the contained data and this functionality.

If a new dataset is included, a new functionality is implemented or a bug in the package is fixed, by updating IDA from gitlab you gain access to these improvements.

### The IDA dataset (idata)
IDA's data is contained in a dataframe called idata. After loading the package, you have to load the dataset, as well.
    library(IDA)
    data(idata)
    
`idata` 

### The functions
The data already contained in `idata` has a certain structure. It contains the following columns:
* `source_id` contains a unique identifier for the data source, e.g. "AR5-database"
* `model` contains a description of the model which was used for supllying the data, e.g. "REMIND" or "MESSAGE". It can also contain further specifications like the model version used ("REMIND 1.5")
* `scenario` contains the different scenarios e.g. for IAM models from the AR5 database. For historical data like that from the WDI, the scenario is usually names `history` to indicate that it is measured data, not model output data.
* `variable` contains the obvious - a variable identifier
* `unit` is the unit, in which the values of `variable` are stored.
* `value` is the actual value of each combination of the other columns, which serve as identifiers

# Installing IDA
## Configure secure shell (ssh)
``ssh`` is needed to access gitlab so that you can download the R-package and later, add your own code to the package. ``ssh`` works by using a pair of keys, which are nothing else but textfiles, one is private and should never leave your computer via insecure ways like email, the other one is public. Anyone who has your private key can compromise any ssh-connection set up using this key.

1. Check if there is already an ssh-key available. To do so, open up a terminal window (on Mac, hit the command and the space key and start typing *terminal*). Once terminal is open, type ``ls .ssh`` and hit the enter button, which lists files contained in the hidden (because of its name starting with a dot) directory ``.ssh``. If you see two files named ``id_rsa`` and ``id_rsa.pub``, the you already have a key available and skip to step 3.

2. 

## Create and setup  gitlab account
* Go to [https://gitlab.com/users/sign_in](https://gitlab.com/users/sign_in) and create an user account.
* Ask Mario, Fabio or Niklas to add you to the climateanalytics group (and the respective projects?!) on gitlab
* 

## Install R

## Install R-Studio

## Create a gitlab account



## Create ssh-key locally and add public key to gitlab

## Do local git setup



