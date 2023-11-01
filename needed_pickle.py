import pandas as pd

file_path = r'C:\Users\igorr\Documents\archive\ingr_map.pkl'

with open(file_path, 'rb') as file:
    loaded_data = pd.read_pickle(file)

csv_file_path = 'ingrediants_list.csv'

df = pd.DataFrame(loaded_data)

df.to_csv(csv_file_path, index=False)
