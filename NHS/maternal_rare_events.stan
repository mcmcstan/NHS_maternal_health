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

// maternal_rare_events.stan
// Specialized Bayesian model for rare maternal health events
// Designed specifically for stillbirth and maternal mortality analysis
// Uses techniques optimized for rare events (<1% prevalence)
# =============== CORRECTED STAN DATA SETUP ================================


data {
  int<lower=0> N;
  int<lower=0,upper=1> rare_outcome[N];  // Stillbirth or maternal mortality
  vector[N] deprivation;
  vector[N] age;
  int<lower=1> K_ethnicity;
  int<lower=1,upper=K_ethnicity> ethnicity[N];
  vector[N] bmi;
  int<lower=0,upper=1> high_risk[N];
  int<lower=0,upper=1> preeclampsia[N];        // Strong predictor for rare events
  int<lower=0,upper=1> emergency_cesarean[N];  // Strong predictor for maternal mortality
  vector[N] care_continuity;
  int<lower=0> missed_appointments[N];
  int<lower=0,upper=1> language_barrier[N];
}

transformed data {
  // Standardize continuous predictors
  vector[N] age_std = (age - mean(age)) / sd(age);
  vector[N] bmi_std = (bmi - mean(bmi)) / sd(bmi);
  vector[N] continuity_std = (care_continuity - mean(care_continuity)) / sd(care_continuity);
  
  // Calculate observed prevalence for model diagnostics
  real observed_prevalence = mean(to_vector(rare_outcome));
  
  // Create specialized interaction terms for rare events
  vector[N] high_risk_deprivation = to_vector(high_risk) .* deprivation;
  vector[N] preeclampsia_deprivation = to_vector(preeclampsia) .* deprivation;
  vector[N] age_deprivation = age_std .* deprivation;
}

parameters {
  // RARE EVENT SPECIFIC: More constrained priors due to sparse data
  real alpha;                          // Intercept (will be very negative for rare events)
  real<lower=0> beta_deprivation;      // Primary effect of interest
  real beta_age;
  vector[K_ethnicity] beta_ethnicity_raw;
  real<lower=0,upper=1> sigma_ethnicity_shrink;  // Strong shrinkage for rare events
  real beta_bmi;
  real<lower=0> beta_high_risk;        // Should be positive for rare events
  real<upper=0> beta_continuity;       // Protective effect
  real<lower=0> beta_missed_appointments;
  real<lower=0> beta_language_barrier;
  
  // RARE EVENT SPECIFIC: Strong predictors
  real<lower=0> beta_preeclampsia;     // Very strong predictor for rare events
  real<lower=0> beta_emergency_cesarean;  // Strong predictor for maternal mortality
  
  // Interaction effects (more conservative for rare events)
  real beta_high_risk_deprivation;
  real beta_preeclampsia_deprivation;
  real beta_age_deprivation;
}

transformed parameters {
  vector[N] eta;                       // Linear predictor
  vector[N] risk_score;                // Risk scores
  vector[K_ethnicity] beta_ethnicity;  // Shrunken ethnicity effects
  
  // Strong shrinkage for ethnicity effects in rare events
  beta_ethnicity = beta_ethnicity_raw * sigma_ethnicity_shrink;
  
  // Linear predictor with rare event specific terms
  for (n in 1:N) {
    eta[n] = alpha
      + beta_deprivation * deprivation[n]
      + beta_age * age_std[n]
      + beta_ethnicity[ethnicity[n]]
      + beta_bmi * bmi_std[n]
      + beta_high_risk * high_risk[n]
      + beta_continuity * continuity_std[n]
      + beta_missed_appointments * missed_appointments[n]
      + beta_language_barrier * language_barrier[n]
      // RARE EVENT SPECIFIC: Critical predictors
      + beta_preeclampsia * preeclampsia[n]
      + beta_emergency_cesarean * emergency_cesarean[n]
      // Interaction effects
      + beta_high_risk_deprivation * high_risk_deprivation[n]
      + beta_preeclampsia_deprivation * preeclampsia_deprivation[n]
      + beta_age_deprivation * age_deprivation[n];
    
    risk_score[n] = inv_logit(eta[n]);
  }
}

model {
  // RARE EVENT SPECIFIC: Highly informative priors
  alpha                      ~ normal(-6, 1.5);  // Very negative for rare events
  beta_deprivation           ~ normal(0.8, 0.4);   // Strong deprivation effect expected
  beta_age                   ~ normal(0.02, 0.3);  // Age effect per year
  beta_ethnicity_raw         ~ normal(0, 1);
  sigma_ethnicity_shrink     ~ beta(2, 8);         // Strong shrinkage (mean ~0.2)
  beta_bmi                   ~ normal(0.05, 0.3);  // BMI effect per unit
  beta_high_risk             ~ normal(1.0, 0.4);   // Strong high risk effect
  beta_continuity            ~ normal(-0.5, 0.3);  // Strong protective effect
  beta_missed_appointments   ~ normal(0.15, 0.2);
  beta_language_barrier      ~ normal(0.4, 0.3);
  
  // RARE EVENT SPECIFIC: Critical predictor priors
  beta_preeclampsia          ~ normal(1.5, 0.5);   // Very strong effect
  beta_emergency_cesarean    ~ normal(1.2, 0.5);   // Very strong effect for mortality
  
  // Interaction priors (conservative for rare events)
  beta_high_risk_deprivation   ~ normal(0.3, 0.2);
  beta_preeclampsia_deprivation ~ normal(0.4, 0.2);
  beta_age_deprivation         ~ normal(0.02, 0.1);
  
  // Main likelihood
  rare_outcome ~ bernoulli_logit(eta);
}

generated quantities {
  vector[N] y_pred;
  vector[N] log_lik;
  vector[N] intervention_benefit;      // Expected benefit from comprehensive intervention
  
  // RARE EVENT SPECIFIC: Specialized predictions
  vector[N] pred_optimal_care;         // Prediction under optimal care scenario
  vector[N] preventable_fraction;      // Fraction of risk that's preventable
  
  // Population metrics for rare events
  real population_attributable_risk;   // PAR for deprivation
  real intervention_effectiveness;     // Overall intervention effectiveness
  real high_risk_population_fraction;  // Fraction at very high risk
  
  // RARE EVENT SPECIFIC: Early warning system
  vector[N] early_warning_score;       // Score for early intervention targeting
  int high_priority_patients;          // Count of patients needing urgent attention
  
  for (n in 1:N) {
    y_pred[n] = bernoulli_logit_rng(eta[n]);
    log_lik[n] = bernoulli_logit_lpmf(rare_outcome[n] | eta[n]);
    
    // Optimal care scenario (maximum protective interventions)
    pred_optimal_care[n] = bernoulli_logit_rng(
      eta[n] 
      + beta_continuity * 0.5           // 50% continuity improvement
      - beta_language_barrier * language_barrier[n]  // Remove language barriers
      - beta_missed_appointments * missed_appointments[n]  // Eliminate missed appointments
    );
    
    // Calculate preventable fraction
    real optimal_risk = inv_logit(
      eta[n] + beta_continuity * 0.5
      - beta_language_barrier * language_barrier[n]
      - beta_missed_appointments * missed_appointments[n]
    );
    
    preventable_fraction[n] = (risk_score[n] - optimal_risk) / risk_score[n];
    intervention_benefit[n] = risk_score[n] - optimal_risk;
    
    // RARE EVENT SPECIFIC: Early warning score
    early_warning_score[n] = risk_score[n] * (1 + 2 * preeclampsia[n] + 1.5 * high_risk[n]) * 
                            (1 + deprivation[n]) * (2 - care_continuity[n]);
  }
  
  // Population-level calculations
  population_attributable_risk = mean(to_vector(intervention_benefit));
  intervention_effectiveness = population_attributable_risk / mean(risk_score);
  
  // Calculate high-risk population fraction (>90th percentile)
  {
    real threshold_90 = 0;
    vector[N] sorted_risks = sort_asc(risk_score);
    int index_90 = (N * 90) / 100;  // 90% of N
    if (index_90 < 1) index_90 = 1;
    if (index_90 > N) index_90 = N;
    
    threshold_90 = sorted_risks[index_90];
    
    high_risk_population_fraction = 0;
    for (n in 1:N) {
      if (risk_score[n] > threshold_90) {
        high_risk_population_fraction += 1.0 / N;
      }
    }
  }
  
  // Count high priority patients (top 5% early warning scores)
  {
    vector[N] sorted_warnings = sort_desc(early_warning_score);
    int index_5pct = (N * 5) / 100;  // 5% of N
    if (index_5pct < 1) index_5pct = 1;
    if (index_5pct > N) index_5pct = N;
    
    real high_priority_threshold = sorted_warnings[index_5pct];
    high_priority_patients = 0;
    for (n in 1:N) {
      if (early_warning_score[n] > high_priority_threshold) {
        high_priority_patients += 1;
      }
    }
  }
}