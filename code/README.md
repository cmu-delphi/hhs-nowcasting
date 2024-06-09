# Organization and manifest of code folder 

Scripts in this folder produces nowcasts and conducts ablation studies.

## `pred_scrip/`

Scripts for producing nowcasts of state-level, geo-pooled and mixed model. 

To produce nowcasts under scenario one, navigate to this folder and execute 
in terminal: `sbatch monthly_up.sh`. \
To produce quantile tracking of state-levle model under scenario 1, execute 
in terminal `sbatch state_QTI.sh` \
To produce nowcasts of scenario 2 for all three models, execute in terminal
`sbatch noupdate.sh`

## `Ablation_merged/`

Scripts for producing ablation studies, as specified in section 3.5. Each 
subfolder contains the code for producing predictions for both scenario 1 and 2. The commands needed to execute the script is similar to those 
in `pred_scrip/`. 