# sftraj: A central class for tracking and movement data

![sftraj logo](Figures/sftraj-200-white.png "sftraj logo")

This is the homepage for the development of `sftraj`, an R package
offering a generic and flexible approach for a **central trajectory
class to support all stages of movement studies (pre-processing,
post-processing and analysis). The only aim of the package will be to
present this central class and basic functions to build, handle,
summarize and plot movement data**. Our project relies on three
complementary pillars: a broad involvement of the movement community,
a robust conceptual data model, and a sf-based implementation in R.


## Get started

To get started, install from github

```
# To install the stable version
devtools::install_github('mablab/sftrack')

# To install with built vignettes
devtools::install_github('mablab/sftrack', build_opts = c("--no-resave-data", "--no-manual"),build_vignettes = TRUE)

# To install the dev version:
devtools::install_github('mablab/sftrack',ref='dev')

```
The dev version is updated much more frequently and should pass the majority of CRAN checks. However, if you install the dev version, understand it is still may contain some bugs. Please submit any bugs you find to the `issues` page.

## creating an `sftrack` 

In order to convert your raw data into `sftrack` use function `as_sftrack()`. The function requires three main things:  
- coordinate columns in atleast the xy axis (UTM, lat/long, etc...)  
- a timestamp column in POSIXct *or* an integer  
- a grouping column (refered to as a 'burst')  

```
#raccoon <- read.csv(system.file('my_data/raccoon_data.csv')

#xyz
coords = c('longitude','latitude')
crs = '+init=epsg:4326'

#burst
burst = 'animal_id'

#time
time = as.POSIXct(raccoon$timestamp, tz='EST')

my_sftrack <- as_sftrack(data = raccoon, coords = coords, burst = burst, 
                         time = time, crs = crs)
```
## How can you help?

Although we have set out a precise work plan, the very first step will
decide if the package is successful and meet the requirements for
broad acceptance by the movement community. Even before we start
writing a single line of code, we need to precisely understand what is
expected from such a package. This is the *raison d'être* of the use
cases mentioned above. And this is why your contribution would be much
critical, whether you are a package developer, an R guru, or simply a
casual R user interested in movement data.

**We need your feedback!**

The idea is to collect all possible use cases for a trajectory object
in R. We know they are multiple, and will contribute our own use cases
— however, we want `sftraj` to be as useful as possible, and to act as
a center piece for movement in R, so we need you to tell us how you
would use it. **In other words, we want to understand what you expect
from such a package, as a user or as a developer.** For this, we ask
you to fill out special issues in the GitHub tracker of the package,
following the ['Use case'
template](https://github.com/mablab/sftraj/issues/new?assignees=&labels=&template=use-case.md&title=%5BUse+case%5D+Change+this+title).

Use cases do not need to be very complicated, but need to present a
specific use in human terms, the technical requirements associated to
it, and the input and output of the use case. Such use case could look
like this:

> **[Use case] Amazing plot for trajectory**
> 
> **Use case:** 
> 
> Plot a trajectory using my `special_trajplot` function, which shows
> [something amazing].
> 
> **Requirements:**
> 
> - spatial coordinates (x,y) as geographic coordinates with
>   projection information
> 
> - a time (t) as POSIXt object, ordered in time
> 
> - information that identifies individuals (e.g. animal) for each
>   location
> 
> - data associated to each location directly accessible
> 
> **Input:**
> a `sftraj` object
> 
> **Output:**
> a plot with [something amazing] about the trajectory
> 
> **Additional information:**
> See my `special_trajplot` function here [with link].

Another example could be like this:

> **[Use case] Fill in missing locations in a sequence**
> 
> **Use case:** 
> Fill in the missing locations of a trajectory that contains spatial or temporal gaps.
> (for instance coming from GPS with failed fixes); In other words
> add in the missing values of a trajectory, i.e. timestamps with no geographic
> coordinates.
> 
> **Requirements:**
> 
> - a time (t) as POSIXt object, ordered in time
> 
> - information that identifies sequences of locations (optional, if
>   several sequences), which could be different circuits of one
>   individual, or different individuals, etc.
> 
> - `sftraj` should be capable of handling/storing missing values
> 
> **Input:**
> a `sftraj` object
> 
> **Output:**
> a `sftraj` object with additional timestamps for gaps (but otherwise
> identical in every way to the original `sftraj`)
> 
> **Additional information:**
> See `adehabitatLT::setNA`, which does exactly that on `ltraj`
> objects.
