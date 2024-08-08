# General

This repo contains all files and analyses scripts for the RSOS submission "Autistic Traits relate to reduced Reward Sensitivity in Learning from Social Point-Light Displays (PLD’s)".
The analysis is split into several folders:

## RL_parameter_fit

Contains all subject data and custom python scripts to fit the Rescorla-Wagner model to the choice data and to obtain parameter values described in the manuscript.
The most important script is "read_data.py" where participant data is read in and parameters are fit for each subject using negative log-likelihood and several helper functions 
defined in other files. These are then saved as best_params_df.csv and analyzed further in the parameter_analysis folder.

## parameter_analysis
Contains the main analysis of paramaters. The most important file is "glmmTMB.r", where the beta-GLM models are fit and analyzed. 
The main results can be replicated using this file, but several older files for analysis are also included in this folder.

## raw_choice_analysis
Contains the scripts and data for the GLM analysis of raw choice data. Most important file is "GLM.r". Plots are created in the "plots.r" script.

## raw_choice_analysis_OLD
An older version of the analysis, included for transparency, but not immediately relevant for the reported analysis.


