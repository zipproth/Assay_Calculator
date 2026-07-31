# Assay Calculator

A Shiny web application for analysing plate reader measurements of plant
immune responses:

* **Calcium assay** — aequorin luminescence measurements, normalised to the
  luminescence still remaining in each well (L/Lmax).
* **ROS assay** — reactive oxygen species measurements, blanked against the
  control wells of the same genotype and corrected for the background recorded
  before elicitation.

For a 96 or 384 well plate the app draws the raw well curves, reads a plate
layout that maps every well to a genotype and an elicitor, and summarises the
replicates per genotype × elicitor combination as mean kinetics, peak heights
and areas under the curve. Everything can be downloaded as an Excel workbook,
as a zip of csv files or as a PDF of the plots.

The app was written by Alexander Kutschera (TU Munich) as an
[OpenPlantScience](https://github.com/vektorious/Assay_Calculator) community
project. This repository is a fork that is being maintained again after the
upstream project went dormant in 2020.

## Running it

```sh
Rscript setup.R          # install the runtime packages
Rscript -e 'shiny::runApp(".")'
```

Requirements: R ≥ 4.1, `ggplot2` ≥ 3.4 and `shiny` ≥ 1.5. `setup.R` installs
everything else. `renv.lock` records the exact versions the current test suite
was verified against — see *Reproducible environments* below.

Open the *Instructions* tab in the running app for example data, empty
templates and the file format.

## Running the tests

```sh
Rscript setup.R --tests   # adds testthat
Rscript run_tests.R       # whole suite
Rscript run_tests.R auc   # only test files matching a pattern
```

Roughly 370 expectations in 95 blocks; a full run takes a few minutes, since
every `testServer()` block rebuilds the whole analysis. Pass a pattern while
iterating.

The suite is the reason this fork can be changed with any confidence. Most of
it consists of characterization tests: they do not describe what the app
*should* do, they record what it *does* do — the peak heights, the normalised
ROS curves, the exported sheets — against the example files shipped with the
repository. Any change that moves a number fails a test, so a refactoring can
be shown to be behaviour preserving and a deliberate fix shows up as a
deliberate change.

## Layout of the code

| Path | Contents |
| --- | --- |
| `ui.R` | the dashboard: tabs, inputs, plot placeholders |
| `server.R` | the reactive layer only: inputs in, plots and downloads out |
| `R/io.R` | reading measurement, layout, reference and Luminoscan files |
| `R/calculations.R` | normalisation, group means, peak heights, areas |
| `R/plots.R` | the ggplot builders and the colour palettes |
| `R/export.R` | assembling the Excel workbook and the csv archive |
| `tests/testthat/` | the test suite |

Everything under `R/` consists of plain functions of their arguments, so the
numerical core can be tested without starting a Shiny session. The reactive
layer is exercised with `shiny::testServer()`.

## What this fork changes

### New analysis features

* **Area under the curve.** Next to the peak height of the mean curve the app
  reports the area below it, over an integration window whose start and end
  are chosen with a range slider — in seconds for the calcium assay, in
  measurement points for the ROS assay. The area is integrated per well and
  then averaged per genotype × elicitor, so the bars carry a real SD (SDOM for
  ROS) over the replicates rather than an integrated error band. Two methods
  are offered: the trapezoidal rule, which integrates over the x axis and
  therefore stays correct if the measurement interval changes, and the plain
  sum of the values inside the window. The areas can additionally be shown as
  a percentage of a chosen reference group.
* **Seconds or minutes on the kinetics x axis.** The axis used to be
  unlabelled and always showed raw time values. Calcium measurements can be
  shown in seconds or minutes; ROS measurements keep their measurement point
  axis by default and can be converted once the measurement interval is set.
* **Reference curves.** A reference can be drawn into every facet of the mean
  kinetics as a dashed black line, taken from a single well, from the mean over
  a selection of wells, from a genotype × elicitor group of the current plate,
  or from an uploaded file. The file format is the one the app exports itself
  (time in the first column, one column per curve), so the *mean data* sheet of
  an earlier download can be used directly.
* **Configurable measurement interval.** The interval between two measurement
  points used to be hard wired to 10 seconds, in the time axis *and* inside the
  L/Lmax normalisation. It is now an input — entered in seconds or minutes,
  without an upper bound — defaulting to 10 s for the calcium assay and 60 s
  for the ROS assay.
* **Configurable ROS background.** The number of measurement points recorded
  before elicitation and how many of them are averaged into the background used
  to be hard wired to 10 and 5. Both are inputs now.
* **Excluding wells.** Individual wells can be dropped from every mean, maximum
  and area without editing the layout file. Excluded wells are flattened in the
  well curve overview and their label is drawn in red.
* **A settings sheet in every download.** Every value that influenced the
  numbers — assay type, file names, measurement interval, background window,
  excluded wells, integration window and method — is written to the workbook,
  so a result stays reproducible.
* **CSV download.** All data sets of the analysis as a zip of csv files, next
  to the existing Excel and PDF downloads.
* **Colour palettes.** The plots can use the ggplot2 default, the Nature
  Publishing Group palette, or the colour blind safe Okabe-Ito and Viridis
  palettes.

### Bugs fixed

* **The plate mean was a single well.** `calculate_data()` selected the well
  columns with a logical vector over the *rows* (`!time`), which selects the
  first column only. The green "plot plate mean" overlay and the blue wildtype
  overlay therefore showed well A1 instead of the mean over the plate.
* **The labels of the exported well sheet.** The genotype and elicitor of every
  well were derived by comparing the layout column with the data column names
  element by element. The two vectors have different lengths, so the time
  column ended up without a name, and any layout whose rows were not sorted
  exactly like the data columns produced mostly empty labels. It now matches
  the wells by name.
* **Peak heights were matched by value.** The SD belonging to a peak was found
  by searching the data for the peak value, which broke as soon as a value
  appeared twice — the original code carried a comment saying exactly that. The
  peak position is now used directly.
* **The Excel download wrote into the application directory** and then renamed
  the result into place. That left files next to the application, made two
  simultaneous downloads overwrite each other, and failed outright with
  "invalid cross-device link" whenever the temp directory sits on a different
  file system than the download directory. Everything is now assembled below
  the session temp directory.
* **The exported maxima sheet had a trailing empty row**, because the matrix
  was allocated with two rows and only the first was ever filled.
* **Genotype × elicitor combinations without wells** produced empty bars from
  `NaN`/`-Inf` instead of being skipped.
* **Broken icons.** The sidebar used Font Awesome 4 names, which current Shiny
  versions no longer ship, so several menu icons rendered as blanks.
* **A JVM tuning option that never applied.** `options(java.parameters =
  "-Xss2560k")` was set *after* the Java based Excel package had already been
  loaded and the JVM started. It is gone together with the Java dependency.
* A leftover debug `print()` in the ROS calculation, a guard that tested
  `is.null()` on the base function `data` and therefore never fired, the well
  labels of the plate overview being drawn once per measurement point instead
  of once per well, and the typos *Standart Analysis* and `multiple_calulate`.

### Under the hood

* **No Java.** Excel files were read and written with `XLConnect`, which needs
  a Java runtime and was the most common reason for the app to fail on a fresh
  server. Reading now uses `readxl`, writing uses `openxlsx`; both are pure
  R/C++ packages.
* **The calculations moved out of `server.R`** into `R/`, as plain functions
  that can be tested directly. `server.R` shrank from about 1200 lines of mixed
  reactives and mathematics to the reactive layer.
* **Understandable error messages.** Missing files, a layout without the
  required columns, a ROS layout without `control` wells and layouts that do
  not match the measurement now produce a sentence explaining what to do
  instead of a red R stack trace. The long calculations show a progress bar.

### Known differences in the numbers

Two changes move results compared with the upstream version. Both are
deliberate and are covered by tests:

* The **plate mean overlay** now really is the mean over the plate (see above).
* The **ROS well curve x axis** runs in real seconds. It used to be labelled
  with the calcium interval of 10 s regardless of the assay, which was simply
  wrong; the axis of the well curve overview carries no tick labels, so this is
  only visible in the axis limit sliders.

Everything else — every peak height, every mean curve, every normalised ROS
value — is unchanged, and the test suite asserts exactly that against the
example files.

One open question is deliberately **not** changed: the ROS background window
ends one measurement point before the elicitation. With the defaults (10 points
recorded, 5 used) it averages the points 5 to 9, not 6 to 10. That is how the
original implementation behaved, and changing it would silently move every ROS
result, so it was kept.

## Reproducible environments

`renv.lock` lists the exact package versions the test suite was verified
against, under **R 4.6.1**. It can be restored with

```r
renv::restore()
```

Note that these versions require a recent R. A deployment on an older R has to
regenerate the lockfile on the target machine; `setup.R` installs the current
CRAN versions without pinning and is the simpler route there.

## Licence

GNU General Public License v3.0, see `license.txt`.
