# `moveslite`

- Authors: Tim Fraser, Yan Guo, and Oliver Gao
- Description: repository for `moveslite` R package and validation analyses.

## What is `moveslite`?

`moveslite` is an `R` package for fast emissions estimation based on Cornell's server of previous MOVES analyses.

The goal of `moveslite` is to provide fast, highly accurate predictions of emissions tailored to a user's county. `moveslite` achieves this goal by generating predictions from statistical models built off of a vast supply of default emissions estimates for every county in the US generated by the EPA's MOVES software, compiled by the Cornell Climate Action in Transportation Team. While MOVES data is extremely time consuming to generate, `moveslite` builds models that approximate that data to allow for fast comparison of multiple scenarios within seconds. The goal is to enable fast, data-driven decision making using computational methods, built atop the EPA's MOVES model, the gold standard in the US for emissions modeling and regulatory compliance.

## How do I use `moveslite`?

### Using `moveslite` as a member of the public

Our team at Gao Labs is currently developing a front-end user interface that anyone can use to make queries to the Cornell CATSERVER via `moveslite`. We expect this online interface to be available by the end of 2024.

### Using `moveslite` in your research

To use MOVESLite as an external user, you will need `CATSERVER` credentials to access the Cornell CATSERVER that MOVESLite queries. Since these credentials are sensitive, CATSERVER access is currently limited to research and development personnel. If you would like to be one of our user testing partners for `moveslite`, reach out to Dr. Tim Fraser at Gao Labs @ Cornell at <tmf77@cornell.edu>.


## Setup for `moveslite`

To use `moveslite`, you will need an active `RStudio` coding environment. You can build the package from source using our `dev.R` script in this repository, or you can install the most recent version of the package from github using our `workflow.R` script, which contains helpful examples of how to use it.

### Configuring `CATSERVER` credentials

These credentials are environmental variables, which should be saved in a `.Renviron` file in your main directory. 

Your `.Renviron` file might look like this:

```
CATSERVER_USERNAME="someusernamehere"
CATSERVER_PASSWORD="somepasswordhere"
CATSERVER_HOST="someurl.cornell.edu"
CATSERVER_PORT=anumberhere
```



### Links

- For more tools by Gao Labs @ Cornell, see our Github Organization Page here at https://github.com/Gao-Labs
- For more information about Gao Labs, see our website at https://gao.cee.cornell.edu/
