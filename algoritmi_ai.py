import pandas as pd
import numpy as np
import os
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
import matplotlib.pyplot as plt
import seaborn as sns

pd.options.mode.chained_assignment = None

from xgboost import XGBClassifier

plt.style.use('seaborn-v0_8-whitegrid')
plt.rcParams.update({
    'font.family': 'sans-serif',
    'font.size': 11,
    'axes.labelsize': 11,
    'axes.titlesize': 13,
    'xtick.labelsize': 10,
    'ytick.labelsize': 10
})

def run_complete_ml_pipeline(price_file, gs_csv, asset_label, output_name):
    if not os.path.exists(price_file):
        print(f"Fisierul {price_file} nu a fost gasit.")
        return []

    # Incarcare date
    df_price = pd.read_excel(price_file) if price_file.endswith('.xlsx') else pd.read_csv(price_file)
    df_price['date'] = pd.to_datetime(df_price['date'])
    df_price.sort_values('date', inplace=True)
    
    df_bubbles = pd.read_csv(gs_csv)
    df_bubbles['Inceput'] = pd.to_datetime(df_bubbles['Inceput'])
    df_bubbles['Sfarsit'] = pd.to_datetime(df_bubbles['Sfarsit'])
    
    # Etichetare bule
    bubbles_asset = df_bubbles[df_bubbles['Activ'] == asset_label].copy()
    df_price['is_bubble'] = 0
    for _, row in bubbles_asset.iterrows():
        mask = (df_price['date'] >= row['Inceput']) & (df_price['date'] <= row['Sfarsit'])
        df_price.loc[mask, 'is_bubble'] = 1
        
    # Constructia predictorilor financiari
    df_price['return'] = df_price['adjusted'].pct_change()
    df_price['volatility_20d'] = df_price['return'].rolling(window=20).std()
    df_price['skewness_20d'] = df_price['return'].rolling(window=20).skew()
    df_price['momentum_10d'] = df_price['adjusted'].pct_change(periods=10)
    df_price['momentum_20d'] = df_price['adjusted'].pct_change(periods=20)
    df_price['volume_change'] = df_price['volume'].pct_change()
    
    # Curatare valori
    df_price.replace([np.inf, -np.inf], np.nan, inplace=True)
    df_model = df_price.dropna().copy()
    
    # Redenumire etichete
    feature_clean_names = ['Return', 'Volatility (20d)', 'Skewness (20d)', 'Momentum (10d)', 'Momentum (20d)', 'Vol. Change']
    X = df_model[['return', 'volatility_20d', 'skewness_20d', 'momentum_10d', 'momentum_20d', 'volume_change']]
    y = df_model['is_bubble']
    
    # Split temporal 80% train / 20% test
    split_idx = int(len(df_model) * 0.8)
    X_train, X_test = X.iloc[:split_idx], X.iloc[split_idx:]
    y_train, y_test = y.iloc[:split_idx], y.iloc[split_idx:]
    
    # Calcul ponderi pentru dezechilibrul claselor
    ratio = (len(y_train) - sum(y_train)) / max(1, sum(y_train))
    
    # Definire modele
    models = {
        'Random Forest': RandomForestClassifier(n_estimators=300, max_depth=12, random_state=42, class_weight='balanced'),
        'XGBoost': XGBClassifier(scale_pos_weight=ratio, eval_metric='logloss', random_state=42)
    }
    
    results_list = []
    excel_tables = {}

    # Pregatire panou grafic
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
    axes_map = {'Random Forest': ax1, 'XGBoost': ax2}

    # Antrenare, predictie si evaluare
    for name, model in models.items():
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)
        
        # Calcul metrici
        acc = accuracy_score(y_test, y_pred)
        report = classification_report(y_test, y_pred, output_dict=True, zero_division=0)
        cm = confusion_matrix(y_test, y_pred)
        
        # Extragere metrici specifice clasei de interes (clasa 1 = bula)
        precision_bule = report.get('1', {}).get('precision', 0)
        recall_bule = report.get('1', {}).get('recall', 0)
        f1_bule = report.get('1', {}).get('f1-score', 0)
        
        # Extragere importanta variabile
        importances = model.feature_importances_
        f_imp = pd.DataFrame({'Feature': feature_clean_names, 'Importance': importances}).sort_values('Importance', ascending=False)
        top_factor = f_imp.iloc[0]['Feature']
        top_weight = f_imp.iloc[0]['Importance']
        
        # Colectare date pentru tabelul centralizator
        results_list.append({
            'Activ Analizat': asset_label,
            'Model Algoritm': name,
            'Acuratețe Globală': f"{acc:.2%}",
            'Precizie (Bule)': f"{precision_bule:.2%}",
            'Recall (Bule)': f"{recall_bule:.2%}",
            'F1-Score': f"{f1_bule:.2%}",
            'Top Factor': f"{top_factor} ({top_weight:.1%})"
        })
        
        # Salvare date pentru fisiere Excel individuale ale activelor
        excel_tables[f'{name}_Performanta'] = pd.DataFrame(report).transpose()
        excel_tables[f'{name}_Importanta'] = f_imp
        excel_tables[f'{name}_Matrice'] = pd.DataFrame(cm, index=['Real: Normal', 'Real: Bulă'], columns=['Pred: Normal', 'Pred: Bulă'])
        
        # Generare vizualizare matrice de confuzie
        cmap_theme = 'Blues' if name == 'Random Forest' else 'mako_r'
        sns.heatmap(cm, annot=True, fmt='d', cmap=cmap_theme, ax=axes_map[name], cbar=False,
                    xticklabels=['Normal', 'Bulă'], yticklabels=['Normal', 'Bulă'],
                    annot_kws={"size": 13, "weight": "bold"})
        axes_map[name].set_title(f'{name} - {asset_label}', fontweight='bold', pad=15)
        axes_map[name].set_ylabel('Realitate (GSADF)')
        axes_map[name].set_xlabel('Predicție Model (ML)')

    plt.tight_layout()
    plt.savefig(f'Grafic_Comparativ_ML_{output_name}.png', dpi=300, bbox_inches='tight')
    plt.close()

    with pd.ExcelWriter(f'Rezultate_Detaliate_{output_name}.xlsx') as writer:
        for sheet_name, df_sheet in excel_tables.items():
            df_sheet.to_excel(writer, sheet_name=sheet_name)
            
    return results_list

gsadf_database = 'Tabel_Datare_Bule_GSADF.csv'
pipeline_config = [
    ('GSPC_date.xlsx', 'S&P 500', 'SP500'),
    ('IXIC_date.xlsx', 'NASDAQ', 'NASDAQ'),
    ('GLD_date.xlsx', 'Aur (GLD)', 'AUR'),
    ('USO_date.xlsx', 'Petrol (USO)', 'PETROL'),
    ('ETH-USD_date.xlsx', 'Ethereum', 'ETHEREUM')
]

sinteza_generala = []

for file_path, label_csv, name_out in pipeline_config:
    try:
        asset_results = run_complete_ml_pipeline(file_path, gsadf_database, label_csv, name_out)
        sinteza_generala.extend(asset_results)
    except Exception as e:
        print(f"Eroare intampinata la activul {label_csv}: {e}")

# Generare si export tabel centralizator
if sinteza_generala:
    df_sinteza_finala = pd.DataFrame(sinteza_generala)
    df_sinteza_finala.to_excel('Tabel_Sinteza_RF_XGBoost_Acuratete.xlsx', index=False)
    print(df_sinteza_finala)