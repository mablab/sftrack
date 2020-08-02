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


## Project plan

All of the work is going to happen openly on GitHub, in this
repository. The code will be released under the [MIT
license](https://opensource.org/licenses/MIT) for the package, a fully
open license that allows for more contributions, and wide acceptation
by other package developers. Last but not least, we abide by a strict
[code of conduct](CODE_OF_CONDUCT.md) to enforce a safe and inclusive
environment for the community interested in `sftraj`.

The time frame of this project is over 6 months, until
March 2020. Following the [MoSCoW
method](https://en.wikipedia.org/wiki/MoSCoW_method), we determined
what will be delivered from this project, starting from a minimum
viable product to future development.

### Must have

*Must have* are requirements necessary for project completion, which
define together our minimum viable product, i.e. a usable `sftraj`
package:
 
* Use cases described [month 1–2]
* Data model revisited and class definitions [month 3]
* Creators and converters from basic objects (`data.frames`, `sf`,
  trajectories) [month 4]
* Installable package (GitHub) [months 4–6]
* Accessors and summaries (`print`, `summary`) [month 5]
* Full function documentation and unit testing [months 5–6]

### Should have

*Should have* are important requirements, which are however not
necessary for project completion:

* Vignette [month 6]

### Could have

*Could have* are desirable requirements developed if time allows:

* Package on [CRAN](https://cran.r-project.org/) (the package will be
  installable from GitHub as a *must have*)
* Basic plot (static) of trajectory object

### Won't have

*Won’t have* are requirements that are identified, but not planned at
this stage of the work:

* Full-fledged package, including submission to
  [CRAN](https://cran.r-project.org/) and
  [rOpenSci](https://ropensci.org/).
* Preparation of a detailed article (targeting the [R
  Journal](https://journal.r-project.org/)) to present the technical
  choices and the solution offered by `sftraj`, in order to favor
  adoption by users and package developers.
* Broad adoption by package developers, by continuing open
  conversation with them, and help them develop conversion tools to
  major existing classes.
* Dynamic visualization of trajectories, allowing keyboard- and
  mouse-controlled exploration of trajectories, step by step (based on
  the solution provided in `rpostgisLT`).

