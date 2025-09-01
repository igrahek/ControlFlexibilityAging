import hssm
import hssm.plotting
import numpy as np
import pandas as pd
import arviz as az
import seaborn as sns
from matplotlib import pyplot as plt
import bambi as bmb
import re

class hssm_analyze:
    @staticmethod
    def load_model(model_name):
        """
        Loads the model specs and attaches samples from a NetCDF file to the model's _inference_obj attribute.
        
        Parameters:
        - model: Name of the model object. 
        """
        # Load the model specification and run it to get the model object
        with open(f"../models/{model_name}.py", "r") as file:
            script = file.read()

        # Remove everything after the "# Sample" line
        script = re.split(r"# Sample", script, maxsplit=1)[0]

        # Execute the script in a local namespace and retrieve the model
        local_vars = {}
        exec(script, {}, local_vars)  # Execute script safely
        model = local_vars.get("model")  # Retrieve the model object

        if model is None:
            raise ValueError("The script did not define a 'model' object.")

        # Load the inference data
        samples = az.InferenceData.from_netcdf(f"../output/{model_name}")

        # Attach inference data to the model
        model._inference_obj = samples
        return model

    @staticmethod
    def generate_summary(model, var_names='v_Intercept'):
        """
        Generates a summary table for specified variables from the model's inference data.
        
        Parameters:
        - model: The Bayesian model object with attached inference data.
        - var_names: List of variable names to include in the summary. Defaults to a predefined list.
        
        Returns:
        - A pandas DataFrame containing the summary statistics.
        """
        if not hasattr(model, '_inference_obj'):
            raise AttributeError("No inference data attached to the model. Use attach_samples first.")
        return az.summary(model._inference_obj, var_names=var_names)

    @staticmethod
    def plot_traces(model, var_names='v_'):
        """
        Plots trace diagrams for specified variables from the model's inference data.
        
        Parameters:
        - model: The Bayesian model object with attached inference data.
        - var_names: Variable name(s) or substring(s) to filter variables for plotting.
        """
        if not hasattr(model, '_inference_obj'):
            raise AttributeError("No inference data attached to the model. Use attach_samples first.")
        az.rcParams["plot.max_subplots"] = 70
        az.plot_trace(model._inference_obj, var_names=var_names, filter_vars="like")
        plt.tight_layout()
        plt.show()

    @staticmethod
    def plot_ppcs(model, col_var="intervalType", row_var="blockType", x_size=12, y_size=12):
        """
        Plots posterior predictive checks faceted by specified variables.
        
        Parameters:
        - model: Bayesian model object with attached inference data
        - col_var: Variable name for column faceting (default: "intervalType")
        - row_var: Variable name for row faceting (default: "blockType")
        - x_size: Figure width in inches (default: 12)
        - y_size: Figure height in inches (default: 8)
        """
        if not hasattr(model, '_inference_obj'):
            raise AttributeError("No inference data attached to the model. Use attach_samples first.")
            
        plt.figure(figsize=(x_size, y_size))
        hssm.plotting.plot_posterior_predictive(
            model,
            col=col_var,
            row=row_var,  # Fixed syntax error
            linewidths=1.6,
            linestyles=['-', '--'])