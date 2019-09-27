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

All of the work is going to happen openly on GitHub, on this
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
viable product to future development:

### Must have

*Must have* are requirements necessary for project completion, which
define together our minimum viable product, i.e. a usable `sftraj`
package:
 
* Use cases described [month 1–2]
* Data model revisited and classes definition [month 3]
* Creators and converters from basic objects (data.frames, sf,
  trajectories) [month 4]
* Installable package (GitHub) [months 4–6]
* Accessors and summaries (print, summary) [month 5]
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


## How can you help?

Although we proposed a well devised work plan, the very first step
will decide if the package is successful and meet the requirements for
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
would use it. For this, we ask you to fill out special issues in the
GitHub tracker of the package, following the ['Use case'
template](https://github.com/mablab/sftraj/issues/new?assignees=&labels=use+case&template=use-case.md&title=%5BUse+case%5D).

Use cases do not need to be very complicated, but need to present a
specific use in human terms, the technical requirements associated to
it, and the input and output of the use case. Such use case could look
like this:

> **[Use case] Fill in missing locations in a sequence**
> 
> **Use case:** 
> Filling in missing locations in a sequence with gaps
> (for instance coming from GPS with failed fixes); in other words
> adding missing values, i.e. timestamps with no geographic
> coordinates
> 
> **Requirements:**
> 
> - (t) as POSIXt object, ordered in time 
> 
> - Sequence identification present (optional, if several sequences)
>   to allow for potential gaps or different ids between sequences
> 
> - `sftraj` capable of handling/storing missing values
> 
> **Input:**
> a `sftraj` object
> 
> **Output:**
> a `sftraj` object with additional timestamps (but otherwise
> identical in every way to the original `sftraj`)
> 
> **Additional information:**
> See `adehabitatLT::setNA`, which does exactly that on `ltraj`
> objects.
