import hssm
import numpy as np
import pandas as pd
import bambi as bmb

# Import data
data = pd.read_csv('../data/DDM_Mixed_VaryingBlocks_AllSubs.csv')

data['intervalType'] = data['intervalType'].replace({'Accuracy': 0.5, 'Speed': -0.5})
data['Switch'] = data['Switch'].replace({'Repeat': 0.5, 'Switch': -0.5})
data['congruency'] = data['congruency'].replace({'congruent': 0.5, 'incongruent': -0.5})
data['Age'] = 100*data['Age']
data['Age_2'] = 100*data['Age_2']

# Specify the model
model = hssm.HSSM(
    data=data,
    model="ddm_sdv", #ddm_sdv
    loglik_kind="analytical",  # approx_differentiable = LAN likelihood; analytical = Navarro & Fuss
    prior_settings="safe",
    p_outlier = {"name": "Beta", "alpha": 10, "beta": 200},
    lapse=bmb.Prior("Uniform", lower=0.0, upper=20.0),
    include=[
        {
            "name": "v",
            "formula": "v ~ 1 + intervalType * Switch * Age + congruency + (1 + congruency + intervalType * Switch | subj_idx)",
            "prior": {
                "Intercept": {"name": "Normal", "mu": 1, "sigma": 2, "initval": 1},
                "intervalType": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "Switch": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "congruency": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "Age": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "intervalType:Switch": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "intervalType:Age": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "Switch:Age": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "intervalType:Switch:Age": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "1|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "intervalType|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "Switch|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "congruency|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "intervalType:Switch|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}}
            },
            "link": "identity",
        },
        {
            "name": "t",
            "formula": "t ~ 1 + intervalType + (1 + intervalType | subj_idx)",
            "prior": {
                "Intercept": {"name": "Weibull", "alpha": 1.5, "beta": 0.3, "initval": 0.05},
                "intervalType": {"name": "Normal", "mu": 0.0, "sigma": 1.0, "initval": 0.0},
                "1|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "intervalType|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}}
            },
            "link": "identity",
        },
        {
            "name": "z",
            "formula": "z ~ 1 + congruency + (1 + congruency | subj_idx)",
            "prior": {
                "Intercept": {"name": "Weibull", "alpha": 1.5, "beta": 0.3, "initval": 0.5},
                "congruency": {"name": "Normal", "mu": 0, "sigma": 0.2, "initval": 0},
                "1|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "congruency|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}}
            },
            "link": "identity",
        },
        {
            "name": "a",
            "formula": "a ~ 1 + intervalType * Switch * Age + scaledRunningTime + (1 + scaledRunningTime + intervalType * Switch | subj_idx)",
            "prior": {
                "Intercept": {"name": "Gamma", "mu": 0.5, "sigma": 1.75, "initval": 1},
                "intervalType": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "Switch": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "Age": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "intervalType:Switch": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "intervalType:Age": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "Switch:Age": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "intervalType:Switch:Age": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "scaledRunningTime": {"name": "Normal", "mu": 0, "sigma": 1, "initval": 0},
                "1|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "scaledRunningTime|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "intervalType|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "Switch|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
                "intervalType:Switch|subj_idx": {"name": "Normal", "mu": 0, "sigma": {"name": "Weibull", "alpha": 1.5, "beta": 0.3}},
            },
            "link": "identity",
        },
    ],
    noncentered=True
)

# Sample
samples = model.sample(
    sampler="nuts_numpyro",  # type of sampler to choose, 'nuts_numpyro', 'nuts_blackjax' of default pymc nuts sampler
    cores=4,  # how many cores to use
    chains=4,  # how many chains to run
    draws=1000,  # number of draws from the markov chain
    tune=2000,  # number of burn-in samples
    idata_kwargs=dict(log_likelihood=True),  # return log likelihood
    random_seed=42,
    # target_accept = 0.85
)

# Save the model
samples.to_netcdf('../output/Model6')



