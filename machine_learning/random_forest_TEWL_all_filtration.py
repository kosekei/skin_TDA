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

df = pd.read_table("data/all_filtration_data.txt")
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
df_pca = pd.DataFrame(feature, columns=["PC{}".format(x + 1) for x in range(min(len(df_grid2.columns), len(df_grid2.index)))])
df_pca2 = df_pca.loc[:, pca.explained_variance_ratio_>0.01]
df2 = pd.concat([df_para,df_pca2],axis=1)

IDs = pd.read_table("data/IDs.txt", header=None)

train_df = df2[~df2['ID'].isin(IDs.iloc[:,0])]
X_train = train_df.drop(["ID","num","TEWL","location","season"], axis=1)
y_train = train_df["TEWL"]

test_df = df2[df2['ID'].isin(IDs.iloc[:,0])]
X_test = test_df.drop(["ID","num","TEWL","location","season"], axis=1)
y_test = test_df["TEWL"]

params = {
    'bootstrap': [True],
    'criterion': ['mse'],
    'max_depth': [None, 2, 3, 4, 5],
    'max_features': ['auto', round(math.log2(len(X_train.columns))/4),round(math.log2(len(X_train.columns))/2),round(math.log2(len(X_train.columns))*2),round(math.log2(len(X_train.columns))*4), len(X_train.columns)/2, len(X_train.columns)],
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
gs_result = pd.DataFrame.from_dict(gs.cv_results_)
model = gs.best_estimator_

model.get_params()
model.score(X_test,y_test)

result = model.score(X_test, y_test)
gs_result.to_csv('data/result_all_filtration.txt', sep='\t', index=False)

df_feature = pd.DataFrame(X_train.columns, columns=['feature'])
df_weight = pd.DataFrame(model.feature_importances_, columns=['weight'])
df_importance = pd.concat([df_feature, df_weight],axis=1).sort_values('weight', ascending=False)

df_importance.to_csv('data/variable_importance_all_filtration.txt', sep='\t', index=False)

filename = 'data/model_all_filtration.sav'
pickle.dump(model, open(filename, 'wb'))
#model = pickle.load(open(filename, 'rb'))


y_pred = model.predict(X_test)

obs_pred = pd.DataFrame({"obs": y_test, "pred": y_pred})
paras = pd.DataFrame({"ID": test_df["ID"],"num":test_df["num"], "sex":test_df["sex"], "season":test_df["season"]})

obs_pred2 = pd.concat([paras,obs_pred], axis=1)

d0 = pd.DataFrame()
for ID in pd.Series.unique(obs_pred2["ID"]):
    d = obs_pred2[obs_pred2["ID"]==ID]
    try:
        s = d[d["season"]=="S"]
        meds = np.median(s["pred"])
        s2 = s.iloc[0,:]
        s3 = s2.replace(s2["pred"], meds)
        d0 = pd.concat([d0, s3], axis=1)
    except:
        pass
    try:
        w = d[d["season"]=="W"]
        medw = np.median(w["pred"])
        w2 = w.iloc[0,:]
        w3 = w2.replace(w2["pred"], medw)
        d0 = pd.concat([d0, w3], axis=1)
    except:
        pass

obs_pred3 = d0.T.drop("num", axis=1)
obs = obs_pred3["obs"]
pred = obs_pred3["pred"]

from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score

eval1 = pd.Series({"MAE":mean_absolute_error(obs, pred), "RMSE":np.sqrt(mean_squared_error(obs, pred)), "R2":r2_score(obs, pred)})
eval2 = eval1.round(3)
eval2.to_csv("data/eval_all_filtration.txt", sep='\t', index=False)

