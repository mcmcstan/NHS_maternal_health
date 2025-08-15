//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.

// maternal_disparities_improved.stan
// Enhanced Bayesian logistic regression for maternal health disparities
// Improved version with hierarchical effects and better convergence
// Removed: travel_distance, pregnancy_hypertension
// Added: missed_appointments, language_barrier

data {
  int<lower=0> N;
  int<lower=0,upper=1> adverse_outcome[N];
  vector[N] deprivation;
  vector[N] age;
  int<lower=1> K_ethnicity;
  int<lower=1,upper=K_ethnicity> ethnicity[N];
  vector[N] bmi;
  int<lower=0,upper=1> smoking_status[N];
  int<lower=0,upper=1> mental_health_risk[N];
  int<lower=0,upper=1> high_risk[N];
  int<lower=0,upper=1> previous_live_birth[N];
  int<lower=0,upper=1> previous_miscarriage[N];
  int<lower=0,upper=1> prev_cesarean[N];
  vector[N] care_continuity;
  int<lower=0> visit_count[N];
  int<lower=0> early_visits[N];
  int<lower=0> missed_appointments[N];
  int<lower=0,upper=1> language_barrier[N];
  vector<lower=0>[N] total_cost;
  int<lower=0,upper=1> is_rare_outcome;
}

transformed data {
  // Standardize continuous predictors for better convergence
  vector[N] age_std = (age - mean(age)) / sd(age);
  vector[N] bmi_std = (bmi - mean(bmi)) / sd(bmi);
  vector[N] continuity_std = (care_continuity - mean(care_continuity)) / sd(care_continuity);
  vector[N] visits_std = (to_vector(visit_count) - mean(to_vector(visit_count))) / sd(to_vector(visit_count));
  
  // Create interaction terms
  vector[N] deprivation_continuity = deprivation .* care_continuity;
  vector[N] age_bmi = age_std .* bmi_std;
  vector[N] deprivation_squared = deprivation .* deprivation;
}

parameters {
  // Main effects
  real alpha;
  real<lower=0> beta_deprivation;      // Constrained positive for deprivation
  real beta_age;
  vector[K_ethnicity] beta_ethnicity_raw;  // Raw ethnicity effects
  real<lower=0> sigma_ethnicity;       // Hierarchical shrinkage for ethnicity
  real beta_bmi;
  real beta_smoking;
  real beta_mental_health;
  real beta_high_risk;
  real beta_previous_birth;
  real beta_previous_miscarriage;
  real beta_prev_cesarean;
  real<upper=0> beta_continuity;       // Continuity should reduce risk
  real beta_visit_count;
  real beta_early_visits;
  
  // NEW: Updated variables (removed travel_distance)
  real<lower=0> beta_missed_appointments;  // More missed appointments = higher risk
  real<lower=0> beta_language_barrier;     // Language barrier increases risk
  
  // Interaction effects
  real beta_deprivation_continuity;
  real beta_age_bmi;
  real beta_deprivation_squared;       // Non-linear deprivation effects
  
  // Rare event adjustment
  real beta_rare_baseline;             // Baseline adjustment for rare events
}

transformed parameters {
  vector[N] eta;                       // Linear predictor
  vector[N] risk_score;                // Individual risk scores
  vector[K_ethnicity] beta_ethnicity;  // Hierarchical ethnicity effects
  
  // Hierarchical shrinkage for ethnicity effects
  beta_ethnicity = beta_ethnicity_raw * sigma_ethnicity;
  
  // Calculate linear predictor
  for (n in 1:N) {
    eta[n] = alpha
      + beta_deprivation * deprivation[n]
      + beta_age * age_std[n]
      + beta_ethnicity[ethnicity[n]]
      + beta_bmi * bmi_std[n]
      + beta_smoking * smoking_status[n]
      + beta_mental_health * mental_health_risk[n]
      + beta_high_risk * high_risk[n]
      + beta_previous_birth * previous_live_birth[n]
      + beta_previous_miscarriage * previous_miscarriage[n]
      + beta_prev_cesarean * prev_cesarean[n]
      + beta_continuity * continuity_std[n]
      + beta_visit_count * visits_std[n]
      + beta_early_visits * early_visits[n]
      // NEW: Updated predictors
      + beta_missed_appointments * missed_appointments[n]
      + beta_language_barrier * language_barrier[n]
      // Interaction effects
      + beta_deprivation_continuity * deprivation_continuity[n]
      + beta_age_bmi * age_bmi[n]
      + beta_deprivation_squared * deprivation_squared[n]
      // Rare event adjustment
      + is_rare_outcome * beta_rare_baseline;
    
    // Calculate risk score
    risk_score[n] = inv_logit(eta[n]);
  }
}

model {
  // Improved priors
  alpha                     ~ normal(-2, 1);
  beta_deprivation          ~ normal(0.5, 0.3);  // Expect positive deprivation effect
  beta_age                  ~ normal(0, 0.5);
  beta_ethnicity_raw        ~ normal(0, 1);      // Raw effects before shrinkage
  sigma_ethnicity           ~ normal(0, 0.5);    // Shrinkage parameter
  beta_bmi                  ~ normal(0, 0.5);
  beta_smoking              ~ normal(0.3, 0.3);  // Expect positive smoking effect
  beta_mental_health        ~ normal(0.2, 0.3);  // Expect positive mental health effect
  beta_high_risk            ~ normal(0.8, 0.3);  // Strong positive effect expected
  beta_previous_birth       ~ normal(0, 0.5);
  beta_previous_miscarriage ~ normal(0.2, 0.3);  // Slight positive effect expected
  beta_prev_cesarean        ~ normal(0.3, 0.3);  // Positive effect expected
  beta_continuity           ~ normal(-0.3, 0.3); // Negative effect (protective)
  beta_visit_count          ~ normal(-0.1, 0.2); // More visits slightly protective
  beta_early_visits         ~ normal(-0.1, 0.2); // Early visits protective
  
  // NEW: Updated variable priors
  beta_missed_appointments  ~ normal(0.2, 0.2);  // Positive effect expected
  beta_language_barrier     ~ normal(0.3, 0.3);  // Positive effect expected
  
  // Interaction effect priors
  beta_deprivation_continuity ~ normal(-0.2, 0.2);  // Continuity more protective for deprived
  beta_age_bmi                ~ normal(0.1, 0.1);   // Interaction between age and BMI
  beta_deprivation_squared    ~ normal(0.1, 0.1);   // Non-linear deprivation effects
  
  // Rare event parameters
  beta_rare_baseline        ~ normal(-1, 0.5);    // Baseline adjustment for rare events
  
  // Likelihood
  adverse_outcome ~ bernoulli_logit(eta);
}

generated quantities {
  vector[N] y_pred;                    // Posterior predictions
  vector[N] log_lik;                   // Log likelihood for model comparison
  vector[N] pathway_assignment;        // Care pathway recommendations
  vector[N] intervention_priority;     // Intervention priority scores
  
  // Intervention scenarios
  vector[N] pred_continuity_boost;     // +20% continuity improvement
  vector[N] pred_visit_boost;          // +2 additional visits
  vector[N] pred_language_support;     // Language barrier removal
  vector[N] pred_comprehensive;        // Combined intervention
  
  // Population-level metrics
  real baseline_population_risk;
  real intervention_population_risk;
  real absolute_risk_reduction;
  real relative_risk_reduction;
  real number_needed_to_treat;
  
  // Pathway-specific outcomes
  real avg_cost_by_pathway[3];
  real avg_risk_by_pathway[3];
  real patients_by_pathway[3];    // Changed from int to real
  
  // Generate predictions and calculate metrics
  for (n in 1:N) {
    // Posterior predictions
    y_pred[n] = bernoulli_logit_rng(eta[n]);
    log_lik[n] = bernoulli_logit_lpmf(adverse_outcome[n] | eta[n]);
    
    // Risk-stratified pathway assignment
    if (risk_score[n] < 0.15) {
      pathway_assignment[n] = 1;  // Standard care
    } else if (risk_score[n] < 0.35) {
      pathway_assignment[n] = 2;  // Enhanced care
    } else {
      pathway_assignment[n] = 3;  // Intensive care
    }
    
    // Multi-factor intervention priority
    intervention_priority[n] = risk_score[n] * deprivation[n] * 
                              (1 - care_continuity[n]) * 
                              (1 + 0.5 * language_barrier[n]) *
                              (1 + 0.3 * missed_appointments[n] / 5.0);
    
    // Intervention scenario predictions
    pred_continuity_boost[n] = bernoulli_logit_rng(
      eta[n] + beta_continuity * 0.2 + 
      beta_deprivation_continuity * deprivation[n] * 0.2
    );
    
    pred_visit_boost[n] = bernoulli_logit_rng(
      eta[n] + beta_visit_count * 2
    );
    
    pred_language_support[n] = bernoulli_logit_rng(
      eta[n] - beta_language_barrier * language_barrier[n]
    );
    
    pred_comprehensive[n] = bernoulli_logit_rng(
      eta[n] + beta_continuity * 0.2 + 
      beta_visit_count * 2 +
      beta_deprivation_continuity * deprivation[n] * 0.2 -
      beta_language_barrier * language_barrier[n] -
      beta_missed_appointments * missed_appointments[n] * 0.5
    );
  }
  
  // Population-level intervention effectiveness
  baseline_population_risk = mean(risk_score);
  intervention_population_risk = mean(to_vector(pred_comprehensive));
  absolute_risk_reduction = baseline_population_risk - intervention_population_risk;
  relative_risk_reduction = absolute_risk_reduction / baseline_population_risk;
  number_needed_to_treat = absolute_risk_reduction > 0 ? 1.0 / absolute_risk_reduction : 999;
  
  // Calculate pathway-specific metrics
  {
    vector[3] sum_cost = rep_vector(0, 3);
    vector[3] sum_risk = rep_vector(0, 3);
    vector[3] count_pathway = rep_vector(0, 3);
    
    for (n in 1:N) {
      int pathway;
      if (pathway_assignment[n] == 1) {
        pathway = 1;
      } else if (pathway_assignment[n] == 2) {
        pathway = 2;
      } else {
        pathway = 3;
      }
      sum_cost[pathway] += total_cost[n];
      sum_risk[pathway] += risk_score[n];
      count_pathway[pathway] += 1;
    }
    
    for (p in 1:3) {
      patients_by_pathway[p] = count_pathway[p];
      avg_cost_by_pathway[p] = count_pathway[p] > 0 ? sum_cost[p] / count_pathway[p] : 0;
      avg_risk_by_pathway[p] = count_pathway[p] > 0 ? sum_risk[p] / count_pathway[p] : 0;
    }
  }
}