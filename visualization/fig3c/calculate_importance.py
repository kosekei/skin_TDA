import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import math
import gc
import pickle
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV
from sklearn.feature_selection import VarianceThreshold
from sklearn.decomposition import PCA
from sklearn.model_selection import train_test_split

df = pd.read_table("data/persim_binary.txt")
df = df.replace('M', 0)
df = df.replace('F', 1)
df = df.drop("corneo", axis=1)
df = df.drop("skicon", axis=1)

df["temperature"] = 0
df["humidity"] = 0
df.loc[ (df["location"]=="Akita") & (df["season"]=="S") , "temperature"] = 25.0
df.loc[ (df["location"]=="Akita") & (df["season"]=="S") , "humidity"] = 76
df.loc[ (df["location"]=="Tokyo") & (df["season"]=="S") , "temperature"] = 28.1
df.loc[ (df["location"]=="Tokyo") & (df["season"]=="S") , "humidity"] = 77
df.loc[ (df["location"]=="Akita") & (df["season"]=="W") , "temperature"] = 2.7
df.loc[ (df["location"]=="Akita") & (df["season"]=="W") , "humidity"] = 75
df.loc[ (df["location"]=="Tokyo") & (df["season"]=="W") , "temperature"] = 8.3
df.loc[ (df["location"]=="Tokyo") & (df["season"]=="W") , "humidity"] = 61


df_para = df[["ID","num","sex","age","temperature","humidity", "TEWL","location","season"]]
df_grid = df.drop(["ID","num","sex","age","temperature","humidity", "TEWL","location","season"], axis=1)   

df_grid2 = df_grid.loc[:,df_grid.apply(np.var) != 0]

pca = PCA()
pca.fit(df_grid2)
feature = pca.transform(df_grid2)

df_pca = pd.DataFrame(feature, columns=["PC{}".format(x + 1) for x in range(len(df_grid2.columns))])
df_pca2 = df_pca.loc[:, pca.explained_variance_ratio_>0.01]
df2 = pd.concat([df_para,df_pca2],axis=1)
X = df2.drop(["ID","num","TEWL","location","season"], axis=1)
y = df2["TEWL"]
df_imps = pd.DataFrame()

for i in range(10):
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=i)

    params = {
        'bootstrap': [True],
        'criterion': ['mse'],
        'max_depth': [None, 2, 3, 4, 5],
        'max_features': ['auto', round(math.log2(len(X_train.columns))/4),round(math.log2(len(X_train.columns))/2),round(math.log2(len(X_train.columns))*2),round(math.log2(len(X_train.columns))*4)],
        'max_leaf_nodes': [None],
        'min_impurity_decrease': [0.0],
        'min_samples_leaf':[1],
        'min_samples_split':[2],
        'min_weight_fraction_leaf': [0.0],
        'n_estimators': [100,1000,5000],
        'n_jobs': [6],
        'oob_score': [False],
        'random_state': [None],
        'verbose': [0],
        'warm_start': [False]
    }

    gs = GridSearchCV(RandomForestRegressor(), params, verbose=2, n_jobs=8)
    gs.fit(X_train, y_train)
    model = gs.best_estimator_

    df_feature = pd.DataFrame(X_train.columns, columns=['feature'])
    df_weight = pd.DataFrame(model.feature_importances_, columns=['weight'])
    df_importance = pd.concat([df_feature, df_weight],axis=1).sort_values('weight', ascending=False)
    df_imps = pd.concat([df_imps, df_weight], axis = 1)

df_imps2 = pd.concat([df_feature, np.mean(df_imps, axis = 1)], axis = 1)
df_imps2.columns=["feature","importance_mean"]
df_imps2.to_csv('data/variable_importance_mean_10times.txt', sep='\t', index=False)

df_comp = pd.DataFrame(abs(pca.components_), columns=df_grid2.columns, index=["PC{}".format(x + 1) for x in range(len(df_grid2.columns))])


df_imp_grid = df_comp.loc["PC1"] * df_imps2.query('feature == "PC1"').iloc[0,1] 
+ df_comp.loc["PC2"] * df_imps2.query('feature == "PC2"').iloc[0,1]
+ df_comp.loc["PC3"] * df_imps2.query('feature == "PC3"').iloc[0,1]
+ df_comp.loc["PC4"] * df_imps2.query('feature == "PC4"').iloc[0,1]
+ df_comp.loc["PC5"] * df_imps2.query('feature == "PC5"').iloc[0,1]

imp_sorted = df_imp_grid.sort_values(ascending=False)
imp_sorted.to_csv('data/variable_importance_grid.txt', sep='\t',columns =False)

