# -*- coding: utf-8 -*-
"""
Created on Tue Nov 22 01:01:14 2022

@author: lavinia
"""

#Load libraries
import pandas as pd
import numpy as np
import csv
import docplex.mp
import numpy
import geopandas
import pandas
import pulp
from shapely.geometry import Point
import matplotlib.pyplot as plt

# Model parameters
input_data_folder = r"C:\Users\lavinia\Documents\MCLP_AMB"
demand_csv = input_data_folder + "\Demand.csv"
candidates_csv = input_data_folder + "\candidate_sites.csv"
ODMatrix_csv = input_data_folder + "\OD_matrix.csv"
Output_csv= input_data_folder + "\output.csv"
time_threshold = 5  # minutes
unit_car_capacity = 2387  # 224355/94
maximal_cars_per_site = 3

max_time_rural= 16 #minutes     
max_time_urban= 48 #minutes 

#load datasets
#load excel file of stations with including ID and Number of ambulance vehicles
def read_candidate_sites(candidates_csv):
    df = pd.read_csv(candidates_csv)
    df["NumberOfAmbulances"] = df["NumberOfAmbulances"].fillna(value=0)
    return df

#load excel file of demand points including ID, Call frequancy, and location (urban/rural)
def read_demands(demand_csv):
    df = pd.read_csv(demand_csv)
    df["CallFreq"] = df["CallFreq"].fillna(value=0)
    return df

#load cost matrix including FacilityID, DemandID, DriveTime
def create_OD_matrix(OD_csv, time_threshold):
    df = pd.read_csv(OD_csv)
    # determine the cover relationship based on the travel time and the service standard
    df["Covered"] = np.where(df["DriveTime"] < time_threshold, 1, 0)
    # create pivot table for OD matrix
    pivot = df.pivot("StationID", "DemandID", "Covered")
    return pivot

# read input data for the model
demands = read_demands(demand_csv)
candidates = read_candidate_sites(candidates_csv)
coverage_matrix = create_OD_matrix(ODMatrix_csv, time_threshold)

# Scenario 0: Current distribution
def calculate_objective_with_current_car_distribution(demand_csv, candidates_csv, ODMatrix_csv, time_threshold,
                                                      unit_car_capacity):
    # read input data for the model
    demands = read_demands(demand_csv)
    candidates = read_candidate_sites(candidates_csv)
    coverage_matrix = create_OD_matrix(ODMatrix_csv, time_threshold)
    
from docplex.mp.environment import Environment
env = Environment()
env.print_information()

from docplex.mp.model import Model
mdl = Model("EMS vehicles")

# Define the decision variables
# percentage of demand i covered by facility j
y_i_j_vars = mdl.continuous_var_matrix(demands["DemandID"], candidates["StationID"], ub=1, name="y")

# add constraints
# ct1: the allocated demand should not exceed the capacity of the facility
for j in candidates["StationID"]:
    mdl.add_constraint(mdl.scal_prod([y_i_j_vars[i, j] for i in demands["DemandID"]], demands["CallFreq"])
                       <= unit_car_capacity * candidates.loc[
                           candidates["StationID"] == j, "NumberOfAmbulances"].item())
    
# ct2: The allocated demand at i should not exceed 100%
for i in demands["DemandID"]:
    mdl.add_constraint(mdl.sum(y_i_j_vars[i, j] for j in candidates["StationID"]) == 1)
    
# express the objective
total_covered_demand = mdl.sum(y_i_j_vars[i, j] * demands.loc[demands["DemandID"] == i, "CallFreq"].item()* 
                               coverage_matrix.loc[j, i]
                               for i in demands["DemandID"]
                               for j in candidates["StationID"])

mdl.maximize(total_covered_demand)
mdl.print_information()  

# solve the model
mdl.solve()

