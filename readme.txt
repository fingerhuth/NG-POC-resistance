### readme

# main workflow (for timeline, observed cases averted):
1. simulate model: run s_run-model_12-preexisting.R -> calls _run-model_12bb_preexisting.R
1b. simulate model, sensitivity analyses where only culture (with xi_{G, culture=90%}) used at baseline: s_run-model_12-preexisting_varying-xi-g.R -> calls s_run-model_12bb_preexisting_varying-xi-g.R (then go to steps 2-3 to data for Figure S10 (timeline), S11 (observed cases averted))
2. calculate prevalence, incidence, proportion resistant and their summary statistics for a given timepoint: run s_run-model_12cc.R -> calls s_dd-wrapper.sh -> calls s_run-model_12dd.R
3. merge summary statistics from step 2 (Figure 2, S10), calculate cumulative incidence: s_run-model_12ee.R -> calls s_run-model_12ff.R
4. calculate (observed) cases averted from cumulative incidence (Figure 3, Figures S3-9): s_run-model_12kk_preexisting.R -> calls s_run-model_12ll_preexisting.R

# ratio of resistance spread workflow:
A. simulate model and compute rates of resistance spread: s_run-fit-lm-xir_send.R -> calls s_run-fit-lm-xir.R (Figure 4, Figure 5)
Ab. simulate model and compute rates of resistance spread for alternative scenario where xi_{G, baseline}=xi_{G, culture}=90%: s_run-fit-lm-xir_send_varying_xi-g.R -> calls s_run-fit-lm-xir_varying_xi-g.R (xi_{G, baseline} different from xi_{G, test}, Figure S12)
B. s_run-fit-lm-xir_combine.R (Figure 4, Figure 5, Figure S12)


# scripts
- f_mixing:
   - computes sexual mixing matrix
- f_model: 
   - ODE model
- p_Fig*: scripts for producing figures, some additionally calculate values reported in manuscript
- s_run-model_12-preexisting.R: 
   - sets parameters as specified in pn_* file, calls s_run-model_12bb_preexisting.R for each batch of parameter sets
   - arguments: pn
   - loads/sources: pn*
- s_run-model_12bb_preexisting.R: 
   - simulates the model for each parameter set in batch
   - arguments: set by s_run-model_12-preexisting.R
   - loads/sources: f_mixing.R, f_model.R, ../data/behav*, ../data/outros*
   - output: ../data/12_printh_*, ../data/12_resce_*
- s_run-model_12cc.R:
   - call s_dd-wrapper.sh
   - arguments: pn
   - loads/sources: pn*
- s_dd-wrapper.sh: 
   - copies ../data/12_resce_$4_*_$7.data, ../data/12_printh_$4_*_$7.data, ../data/behav_$4.data, s_run-model_12dd.R to new directory, calls s_run-model_12dd.R, copies results back to initial directory
   - arguments: set by s_run-model_12cc.R:
- s_run-model_12dd.R: 
   - for given number n.tp of timepoints tp, calculates prevalence, incidence and proportion resistance and their summary statistics
   - arguments: set by s_run-model_12cc.R, passed by s_dd-wrapper.sh
   - loads/sources: ../data/behav*, ../data/12_resce*, ../data/12_printh*
   - output: ../data/12_sus-prev*, ../data/12_sus-inc*, ../data/12_sus-obsinc*, ../data/12_sus-pres*, ../data/12_tmp-incT*, ../data/12_tmp-res-incT*
- s_run-model_12ee.R: 
   - calls s_run-model_12ff.R
   - arguments: pn
   - loads/sources: pn*
- s-run-model_12ff.R:
   - merges summary statistics, calculates cumulative incidence (taking into account that incidence has unit per year and simulation gives several outputs per year)
   - arguments: set by s_run-model_12ee.R
   - sources/loads: pn, ../data/12_sus-prev*, ../data/12_sus-inc*, ../data/12_sus-obsinc*, ../data/12_sus-pres*, ../data/12_tmp-incT*, ../data/12_tmp-res-incT*
   - output: ../data/12_tss-prevL*, ../data/12_tss-prevH*, ../data/12_tss-prevT*, ../data/12_tss-incL*, ../data/12_tss-incH*, ../data/12_tss-incT*, ../data/12_tss-obsincL*, ../data/12_tss-obsincH*, ../data/12_tss-obsincT*, ../data/12_tss-presL*, ../data/12_tss-presH*, ../data/12_tss-presT*, ../data/12_cumincT*, ../data/12_cumobsincT*, ../data/12_res-cumincT*, ../data/12_res-cumobsincT*
- s_run-model_12kk_preexisting.R:
   - calls s_run-model_12ll_preexisting.R
   - arguments: pn
   - loads/sources: pn*
- s_run-model_12ll_preexisting.R:
   - arguments: set by s_run-model_12kk_preexisting.R
   - loads/sources: pn*, ../data/12_cumincT*, ../data/12_cumobsincT*, ../data/12_res-cumincT*, ../data/12_res-cumobsincT*
   - output: ../data/12_naat-obscasesAverted*, ../data/12_naat-casesAverted*
- s_prevalence-incidence.R
   - calculate prevalence and incidence median and IQR for 1000 calibrated parameter sets used in this study
   - loads/sources: ../data/outros*
   - output: prints table
- s_run-model_12-preexisting_varying-xi-g.R:
   - calls s_run-model_12bb_preexisting_varying-xi-g.R
   - arguments: pn
- s_run-model_12bb_preexisting_varying-xi-g.R
   - simulates the model for each parameter set in batch with xi_{G, baseline} until it reaches equilibrium, and xi_G after resistance is introduced
   - arguments: set by s_run-model_12-preexisting_varying-xi-g.R
   - loads/sources: f_model.R, f_mixing.R, ../data/outros*, ../data/behav*
   - output: ../data/12_printh*, ../data/12_resce*
- s_run-fit-lm-xir_send.R:
   - calls s_run-fit-lm-xir.R
   - arguments: pn
   - loads/sources: pn*
- s_run-fit-lm-xir.R:
   - simulates model and fits linear model to log transformed ratio of resistant/sensitive
   - arguments: set by s_run-fit-lm-xir_send.R
   - loads/sources: f_model.R, f_mixing.R, ../data/outros*, ../data/behav*
   - output: ../data/fittedRates*
- s_run-fit-lm-xir_send_varying_xi-g.R:
   - calls s_run-fit-lm-xir_varying_xi-g.R
   - arguments: pn
   - loads/sources: pn*
- s_run-fit-lm-xir_varying_xi-g.R
   - simulates model with different xi_{G, baseline} and xi_{G, test} and fits linear model to log transformed ratio of resistant/sensitive
   - arguments: set by s_run-fit-lm-xir_send.R
   - loads/sources: f_model.R, f_mixing.R, ../data/outros*, ../data/behav*
   - output: ../data/fittedRates*
- s_run-fit-lm-xir_combine.R
   - combines fitted rates from each calibrated parameter set to one object
   - arguments: population (het or msm), pn
   - loads/sources: pn*, ../data/fittedRates*
   - output: ../data/fr*


# data
- data/behav*: partner change rate and proportion of population that is in low activity class (see Fingerhuth et al. 2016 PLoS Pathog)
- data/outros*: parameter sets that yield incidence and prevalence in desired range (see Fingerhuth et al. 2016 PLoS Pathog)
- data/Fig2/12_tss-presT_*: proportion resistant infections over 30 years (Figure 2)
- data/Fig3/12_naat-obscasesAverted_*: observed cases averted when compared with NAAT (Figure 3)
- data/Fig4+5+S2/fr_*: rates of resistance spread (Figure 4, 5, S2)
- data/FigS3-9/12_naat-obscasesAverted_*: observed cases averted when compared with NAAT, sensitivity analysis, (Figure S3-9) 
- data/FigS10/12_tss-presT_*: proportion resistant infections over 30 years, sensitivity analysis: only culture used at baseline (Figure S10)
- data/FigS11/12_naat-obscasesAverted_*: observed cases averted when compared with NAAT, sensitivity analysis: only culture used at baseline (Figure 11)
- data/FigS12/fr_*: rates of resistance spread, sensitivity analysis: only culture used at baseline (Figure S12)


# parameters/scenarios
pn_*: files that set diagnosis and treatment parameters
The following list for what scenario a specific parameter set (with a certain ‘pn’ number) is used and in which figure the resulting data is used:
- 120-128: sensitivity analysis observed cases averted (Figure 3), POC+R and culture
- 129: baseline model (Figure 2, FigureS3-S9)
- 130: baseline model with xi_R=0, only scenario 4 (POC, in this case POC-R) used (Figure 2, Figure S4)
- 131-139: sensitivity analysis: xi_G (Figure S3)
- 140-148: sensitivity analysis: xi_R (Figure S4)
- 149-157: sensitivity analysis: lambda_{A, baseline} (Figure S5)
- 158-166: sensitivity analysis: lambda_S (Figure S6)
- 167-175: sensitivity analysis: psi (Figure S7)
- 176-181: sensitivity analysis: delta_baseline (Figure S8)
- 182-87: sensitivity analysis: omega (Figure S9)
- 188-196: sensitivity analysis observed cases averted (Figure 3), POC-R
- 197: rate of resistance spread comparison (Figure 4, Figure 5, Figure S2)
- 198-200: sensitivity analyses: only culture (with xi_{G, culture} = 90%) used at baseline (Figure S10, Figure S11)
- 201-202: rate of resistance spread comparison, sensitivity analyses: only culture (with xi_{G, culture} = 90%) used at baseline (additional parameter so that xi_{G, baseline} can be different from xi_{G, test})  (Figure S12)