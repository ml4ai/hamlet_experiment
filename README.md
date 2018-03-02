# Hamlet experiments

## Hamlet directory structure:

The following is the recommended hamlet project structure.
Note: it's not strictly necessary that <hamlet_executable> live under
    <hamlet_root>/src/hdp_hmm_lt/
It could be anywhere -- but still need <hamlet_root>/<symlink-to-executable>
to point to wherever the <hamlet_executable> lives

    <hamlet_root>               # on venti: /projects/hamlet/

      <symlink-to-executable>   # venti (or local): points to: <hamlet_root>/src/hdp_hmm_lt/hamlet

      data/    # archived       # venti
        data/
        results/                # saved results
        plots/                  # for published plots

      hamlet_experiment/        # ml4ai git repo
        parameters/             #
        plots/                  # temporary
        results/                # temporary
        scripts/                # experiment management
          python/               # Root of python experiment manager framework
            experiments/        # experiment definitions, by domain
              cocktail_party/   #
                experiment/     #
              ...
            run/                # experiment execution
          r/
            queries/
            scripts/
          shell/                #

      src/                      # kjb/projects root; includes lib, Make, include_before, include_after
        hdp_hmm_lt/
          <hamlet_executable>   # hamlet executable home location


## Checking out hamlet from kjb

The following must be checked out from kjb using svn, and should
be placed in the <hamlet_root>/src/ directory
(the svn commands are provided here, executed within <hamlet_root>/src/)

```
$ svn co svn://vision.cs.arizona.edu/src/Make/trunk Make
$ svn co svn://vision.cs.arizona.edu/src/lib/trunk lib
$ svn co svn://vision.cs.arizona.edu/src/include_before/trunk include_before
$ svn co svn://vision.cs.arizona.edu/src/include_after/trunk include_after
$ svn co svn://vision.cs.arizona.edu/src/projects/hdp_hmm_lt/trunk hdp_hmm_lt
```

From <hamlet_root>, create a symlink to the <hamlet_executable>:
```
$ ln -s src/hdp_hmm_lt/hamlet hamlet
```


## Notes on running experiments using python experiment manager framework:

Experiments are executed from scripts within the <domain>/experiment/ subdir
For example, to run the cocktail_party regression test, execute:
    <hamlet_root>/hamlet_experiment/scripts/python/experiments/cocktail_party/experiment/run_cocktail_regression.py

You must have the following in your PYTHONPATH environment variable:
    <hamlet_root>/hamlet_experiment/scripts/python/


## Notes on python 3 sibling nested-module access:
    1. Sub-modules require __init__.py and those must be empty
    2. Add the path to root in PYTHONPATH environment variable
    3. Reference anything from one script starting at level right below root-module

For example, if you have the following structure:
```
    root-module
        submodule1
            script_a.py
        submodule2
            script_b.py
```
... then for script-a to reference script-b, it must import as follows:
```
    import submodule2.script_b as s_b
```


## Notes on batch experiment results directory naming convention:

Convention for directory naming for batch results:
    <results_root> / <experiment_name> / <data_path> / <model> / <replication> / <specific_results>

    <experiment_name> : Top-level name for experiment.
        For example, for the version of the cocktail_party data with
        16 speaker and 12 microphones, the name is: 'cocktail_s16_m12'
        This is often the same name as the data source root name.
    <data_path> : Concatenation of the sub-directory names
        For example, if the data source is the h10.0_nocs (i.e., noise
        precision = 10.0 and no center/scaling (nocs)), and data source
        variant cp0 (i.e., the 0'th sample of the cocktail_party data
        under this noise regime), then the data_path is: 'h10.0_nocs_cp0'
    <model> : Shortened descriptive name of the model.
        For example, if running hamlet in the no LT condition (noLT),
        with the hdp transition prior (hdp) for the hidden markov model
        dynamics (hmm), and with no weight learning (w0), then
        the model name is: 'noLT_hdp_hmm_w0'
    <replication> : Inference may be run multiple times for the same
        model (due to sampling having variable outcomes).  This
        represents the replication number, and is simply a 2-digit
        name, with 0-leading for numbers smaller than 10.
        For example, if this was the fifth replication, then the
        replication name would be: '05'
    <specific_results> : The directory containing all of the results output.
        We do a lot of bookkeeping for debugging (this can be controlled)
        so there can be a large number of files and subdirectories.

        A/
        accuracy.txt
        beta.txt
        F1_score.txt
        gamma.txt
        h.txt
        lambda.txt
        mu.txt
        N/
        n_dot.txt
        obs.txt
        parameters.config
        phi/
        pi/
        pi0/
        precision.txt
        Q/
        recall.txt
        test_log_likelihood.txt
        theta/
        thetastar/
        train_log_likelihood.txt
        u.txt
        W/
        z.txt

