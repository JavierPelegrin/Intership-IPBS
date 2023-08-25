import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random
from pyteomics import mass
from pyteomics import electrochem

import joblib

PROTON_MASS = 1.007276
PH = 3.0

# data_path = 'data\Complex Mixture 230502 DDA_2023-05-09_1400(1).xlsx'
# data_path = 'data\\2023-04-12 DBO Hela 30min hybrid library_2023-05-02_1701(2).xlsx'
data_path = 'data\OXAJS210701_05 Hela thermo Exploris Mascot MS2Rescore_2023-05-10_1527.xlsx'


def mz_to_mass(mz, charge):
    return (mz * charge) - (charge * PROTON_MASS)

def count_basic_residues(sequence):
    return sequence.count('K') + sequence.count('R') + sequence.count('H')


def gererate_dic(data_path):
    xls = pd.ExcelFile(data_path)

    df1 = pd.read_excel(xls, sheet_name='Quantified peptide ions',
                        header=None, 
                        names=["peptide_id", "sequence", "modifications", "ptm_protein_positions", "master_quant_peptide_id", "master_elution_time", "is_validated_for_quanti", "#psm_prot_sets", "master_quant_peptide_ion_id", "master_quant_peptide_ion_moz", "master_quant_peptide_ion_charge", "master_quant_peptide_ion_elution_time", "master_quant_peptide_ion_feature_id", "protein_set_id", "accession", "gene_name", "description", "protein_set_score", "is_protein_set_validated", "best_score_HeLa_50", "psm_count_HeLa_50", "elution_time_HeLa_50", "corrected_time_HeLa_50", "raw_abundance_HeLa_50", "abundance_HeLa_50", "Total", "%" ]
                        )

    peptide_ion_ids = df1['master_quant_peptide_ion_id'].values
    peptide_ion_ids = peptide_ion_ids[1:]
    dic = {}
    for i, ids in enumerate(peptide_ion_ids):
        if df1.loc[i+1]['peptide_id'] not in dic.keys():
            dic.update( { df1.loc[i+1]['peptide_id'] : {} } )
        dic[df1.loc[i+1]['peptide_id']].update( { df1.loc[i+1]['master_quant_peptide_ion_charge'] : { 'sequence':df1.loc[i+1]['sequence'] , 'abundance': df1.loc[i+1]['abundance_HeLa_50'], "ion_moz" : df1.loc[i+1]['master_quant_peptide_ion_moz'], 'master_quant_peptide_ion_id':ids, '%': None, 'Masse': None, 'master_elution_time': df1.loc[i+1]['master_elution_time'], 'ptm_protein_positions': df1.loc[i+1]['ptm_protein_positions'] } } )

    for k in dic.keys():
        dic[k].update( { 'total': sum([dic[k][i]['abundance'] for i in dic[k].keys()]) } )
        for z in dic[k].keys():
            if z != 'total':
                # compute de % of abundance
                dic[k][z].update( { '%' : dic[k][z]['abundance']/dic[k]['total'] } )
                # compute the mass with excel charge
                dic[k][z].update( { 'Masse' : mz_to_mass(dic[k][z]['ion_moz'], z) } )
                # add the ptm_protein_positions
                # dic[k][z].update( { 'ptm_protein_positions' :  } )
    xls.close()
    joblib.dump(dic, 'data/dic.pkl')


def generate_data_set(dic):
    def generate_data(dic, min_mass=0, max_mass=np.inf, min_charge=0, max_charge=np.inf, min_residues=0, max_residues=np.inf):
        mass_2 = []
        residues_2 = []
        charge_2 = []
        mass_2_excel = []
        labels_2 = []

        mass_3 = []
        residues_3 = []
        charge_3 = []
        mass_3_excel = []
        labels_3 = []

        for i in dic.keys():
            for j in dic[i].keys():
                if j == 2 and dic[i][j]['%'] < 1 and electrochem.charge(dic[i][j]['sequence'], pH=PH) > min_charge and electrochem.charge(dic[i][j]['sequence'], pH=PH) < max_charge and dic[i][j]['Masse'] > min_mass and dic[i][j]['Masse'] < max_mass and count_basic_residues(dic[i][j]['sequence']) > min_residues and count_basic_residues(dic[i][j]['sequence']) < max_residues: 
                    mass_2.append(dic[i][j]['Masse'])
                    mass_2_excel.append(dic[i][j]['Mass_excel'])
                    residues_2.append(count_basic_residues(dic[i][j]['sequence']))
                    charge_2.append(electrochem.charge(dic[i][j]['sequence'], pH=PH))
                    labels_2.append(dic[i][j]['%'])
                if j == 3 and dic[i][j]['%'] < 1 and electrochem.charge(dic[i][j]['sequence'], pH=PH) > min_charge and electrochem.charge(dic[i][j]['sequence'], pH=PH) < max_charge and dic[i][j]['Masse'] > min_mass and dic[i][j]['Masse'] < max_mass and count_basic_residues(dic[i][j]['sequence']) > min_residues and count_basic_residues(dic[i][j]['sequence']) < max_residues:
                    mass_3.append(dic[i][j]['Masse'])
                    mass_3_excel.append(dic[i][j]['Mass_excel'])
                    residues_3.append(count_basic_residues(dic[i][j]['sequence']))
                    charge_3.append(electrochem.charge(dic[i][j]['sequence'], pH=PH))
                    labels_3.append(dic[i][j]['%'])
        
        return mass_2, residues_2, charge_2, mass_2_excel, labels_2, mass_3, residues_3, charge_3, mass_3_excel, labels_3

    mass_2, residues_2, charge_2, mass_2_excel, labels_2, mass_3, residues_3, charge_3, mass_3_excel, labels_3 = generate_data(dic, min_charge=2)

    def shuffle(l1, l2):
        temp = list(zip(l1, l2))
        random.shuffle(temp)
        res1, res2 = zip(*temp)
        res1, res2 = np.array(res1), np.array(res2)
        return res1, res2

    x_train = np.array([ [m, r, c] for m, r, c in zip(mass_2_excel, residues_2, charge_2) ])
    y_train = np.array(labels_2)
    print(x_train.shape, y_train.shape)
    x_test = []
    y_test = []

    for _ in range(10):
        x_train, y_train = shuffle(x_train, y_train)

    for i in range(round(x_train.shape[0]*0.25)):
        x_test.append(x_train[i])
        x_train = np.delete(x_train, i, 0)
        y_test.append(y_train[i])
        y_train = np.delete(y_train, i)

    x_test = np.array(x_test)
    y_test = np.array(y_test)
    y_test = (y_test*100).round().astype(int)
    y_train = (y_train*100).round().astype(int)

    # Affichage du nombre d'exemples totales dans le corpus
    print('Taille du corpus total')
    print('\t• train :', len(x_train), 'exemples')
    print('\t• test :', len(x_test), 'exemples')

    # Affichage de la taille des images et des labels dans le corpus 
    print('\nTaille des données d\'apprentissage')
    print('\t• X_train (masse,residues,charge):', x_train.shape)
    print('\t• y_train (labels) :', y_train.shape)

    print('\nTaille des données de test')
    print('\t• X_test (masse,residues,charge) :', x_test.shape)
    print('\t• y_test (labels) :', y_test.shape)
    return x_train, y_train, x_test, y_test

def find_peptide(dic, condition):
    for i in dic.keys():
        for k in dic[i].keys():
            if k != 'total':
                if eval(condition):
                    print(dic[i])
                    return dic[i]

def adjacent_values(vals, q1, q3):
    upper_adjacent_value = q3 + (q3 - q1) * 1.5
    upper_adjacent_value = np.clip(upper_adjacent_value, q3, vals[-1])

    lower_adjacent_value = q1 - (q3 - q1) * 1.5
    lower_adjacent_value = np.clip(lower_adjacent_value, vals[0], q1)
    return lower_adjacent_value, upper_adjacent_value


def set_axis_style(ax, labels):
    ax.set_xticks(np.arange(1, len(labels) + 1), labels=labels)
    ax.set_xlim(0.25, len(labels) + 0.75)
    ax.set_xlabel('Sample name')

def compute_hyfrofobicity(sequence):
    kd = { 'A': 1.8,'R':-4.5,'N':-3.5,'D':-3.5,'C': 2.5,
       'Q':-3.5,'E':-3.5,'G':-0.4,'H':-3.2,'I': 4.5,
       'L': 3.8,'K':-3.9,'M': 1.9,'F': 2.8,'P':-1.6,
       'S':-0.8,'T':-0.7,'W':-0.9,'Y':-1.3,'V': 4.2 }
    return sum([kd[i] for i in sequence])

def pass_to_EncyclopeDIA_style(seq,mod):
    DIA = {
        "Carbamidomethyl" : ( "+57.021464", ("C")),
        "Oxidation": ("+15.99491" ,("M")),
        "Phosphorylation": ("+79.966331" ,("S", "T", "Y")),
        "Acetylation": ("+42.010565" ,("K", "N-term")),
        "Succinylation": ("+101.023869" ,("K")),
        "Ubiquitylation": ("+114.042927" ,("K")),
        "Mono-methylation": ("+14.015650" ,("K", "R")),
        "Di-methylation": ("+28.031300" ,("K", "R")),
        "Tri-methylation": ("+42.046950" ,("K")),
        "TMT0": ("+224.152478" ,("K", "N-term")),
        "TMT10": ("+229.162932" ,("K", "N-term")),
        "Pyroglutamate": ("-18.010565" ,("N-term E")),
        "Pyroglutamate": ("-17.026549" ,("N-term","Q")),
        "Cyclized S-CAM-Cys": ("+39.994915" ,("N-term", "C"))
    }
    mod = mod.split("|")
    
    new_seq = ""
    while len(mod) > 0:
        n = int(mod.pop(0))
        new_seq = f"{new_seq}{seq[:n]}[{DIA[mod.pop(0)][0]}]"
        seq = seq[n:]
    return f"{new_seq}{seq}"