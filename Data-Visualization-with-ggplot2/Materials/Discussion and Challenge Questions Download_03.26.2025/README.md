# Data Visualization with `ggplot2`

## About The Coffee, Cookie and Coding $\left(C^3\right)$ Workshops

Yale's Public Health Data Science and Data Equity (DSDE) team created this workshop series for Public Health and Biostatistics masters-level students at Yale. They are designed to help learners effectively leverage computational tools and analytical methods in their educational and professional endeavors. You can find out more about past and upcoming tutorials on our YouTube (coming soon) and [website](https://ysph.yale.edu/public-health-research-and-practice/research-centers-and-initiatives/public-health-data-science-and-data-equity/events/).


## About Workshop

**Workshop Title:** &nbsp; Data Visualization with `ggplot2`

**Date:** &emsp;&emsp;&emsp;&emsp;&emsp;&nbsp; Thursday March $27^{\text{th}}$, 2025

Upon completing the workshop, you will be able to:
- Classify the Grammar of Graphics layers used in `ggplot` syntax.
- Utilize `ggplot2` for common tasks, including applications of different geometries, effective use of layering, and polishing the result.
- Applying `ggplot2` to make interactive plots, map projections, and leverage AI-assisted coding.

You can find out more about past and upcoming tutorials on our [website](https://ysph.yale.edu/public-health-research-and-practice/research-centers-and-initiatives/public-health-data-science-and-data-equity/events/). If you are affiliated with Yale, you can set up an office hour appointment with one of the data scientists ([Bookings Page](https://outlook.office365.com/owa/calendar/DataScienceDataEquityOfficeHours@yale.edu/bookings/)).

## About Repository

This is a copy of the code and data used by participants of the workshop. You can access the worked through example and workshop slides by visiting the workshop webpage: [ysph-dsde.github.io/Data-Visualization-with-ggplot2](https://ysph-dsde.github.io/Data-Visualization-with-ggplot2/Worked-Through-Example.html). The repository containing all of the project code, references, and other workshop contents can be found in [ysph-dsde/Data-Visualization-with-ggplot2](https://github.com/ysph-dsde/Data-Visualization-with-ggplot2).

### Overview Of Contents

- **P Project:** ``Data-Visualization-with-ggplot2.Rproj``
- **Dataset:** ``RSV-NET Infections.gz.parquet``
- **Discussion and Challenge Questions Code:** ``Discussion and Challenge Questions.R``
- **R version:** 4.4.3
- ``renv version:`` 1.0.11. This is included to reproduce the environment.

## Using this Repository

### No Version Control

We recommend that you use version control with git for all of your projects. If you do not wish to do this, you can save the entire directory in any subfolder you wish to house the project. All of the code should still work so long you initialize the environment correctly. See **Initializing the Environment** below.

### Adding Version Control with git

The following steps assume that you have installed git on your computer and you have a command-line application (i.e. Terminal for Macs and Windows Terminal for windows).

1. Move the project directory (the entire folder originally downloaded as a *.zip file) to the location you want to store it.

2. Open the command-line application and navigate into the project directory.
   ```
   cd "/file_location/project_directory_name"
   ```
   
3. Initialize git.
   ```
   git init
   ```

**NOTE:** We have already included a ``.gitignore`` to the directory listing the types of files you do not need git to version control or share with GitHub.


### Adding to GitHub

These directions assume you have initialized git in your project folder. If you have not done this, you will need to follow the steps listed under **Adding Version Control with git**.

1. [Create a new](https://github.com/new) GitHub repository ([Further documentation](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-new-repository)).
   
   **NOTE:** Do not use a template or include a description, README file, ``.gitignore``, or license. Only adjust the GitHub account owner as needed and create the name for the new repository. We recommend initially setting the repository to Private.
   
2. Open the command-line application and navigate into the project directory.
   ```
   cd "/file_location/project_directory_name"
   ```

3. Confirm that git has been initialized by checking the project status. If the project is initialized you should see that git is present and is tracking files.
   ```
   git status
   ```

4. If ``git status`` shows untracked files or untracked edits for tracked files, then add and commit these to git.
    ```
   git add .
   git commit -m "first commit"
   ```

   
5. Create a branch called ``main``.
   ```
   # check branch main does not exist
   git branch -a
   
   # if no branches add one called main
   git branch -M main
   ```

6. Assign the GitHub repository location and save this location to the proxy name ``origin``. The file transfer protocol can either by SSH or HTML, depending on your git configurations. Change the location used below by copying it from the empty GitHub repository you just created.
   ```
   # if using SSH
   git remote add origin git@github.com:EXAMPLE-USER/NEW-REPOSITORY.git
   
   # or if using HTTPS
   git remote add origin https://github.com/EXAMPLE-USER/NEW-REPOSITORY.git
   ```
   
7. Finally push the directory to your empty GitHub repository.
   ```
   git push -u origin main
   ```
   
### Initializing the Environment

1. Open the project directory on your local device.
2. Launch the project by opening `Data-Visualization-with-ggplot2.Rproj`.
3. Open `Discussion and Challenge Questions.R`.
4. In the R console, activate the environment by running:
    ```
    renv::init()                   # initialize the project
    renv::restore()            # download packages and their version saved in the lockfile.
    ```

   **NOTE:** If you are asked to update packages, say no. The ``renv()`` is intended to recreate the same environment under which the project was created, making it reproducible. You are ready to proceed when running ``renv::restore()`` gives the output ``- The library is already synchronized with the lockfile.``. You can read more about ``renv()`` in their [vignette](https://rstudio.github.io/renv/articles/renv.html).

## About Original Data Source

This workshop uses the Center for Disease Control's (CDC) [Respiratory Syncytial Virus Hospitalization Surveillance Network (RSV-NET)](https://www.cdc.gov/rsv/php/surveillance/rsv-net.html) surveillance data. It is one of the CDC's Respiratory Virus Hospitalization Surveillance Network (RESP-NET) Emerging Infections Programs (EIP). This version was downloaded from [data.gov](https://data.cdc.gov/Public-Health-Surveillance/Weekly-Rates-of-Laboratory-Confirmed-RSV-Hospitali/29hc-w46k/about_data) in January of 2025. RSV-NET conducts active, population-based surveillance for laboratory-confirmed RSV-associated hospitalizations. It contains stratification for geolocation, race/ethnicity, age, and sex.

The cleaned and harmonized version of the RSV-NET dataset was compiled as part of the YSPH’s very own PopHIVE project. You will see that its contents differ slightly from what you would see on the data.gov website. Special thanks to [Professor Daniel Weinberger](https://ysph.yale.edu/profile/daniel-weinberger/) for allow us to adopt his plot code in this workshop.
