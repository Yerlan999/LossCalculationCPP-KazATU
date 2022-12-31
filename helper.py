num_pris = 6;
pris_num = 1; # 1 - only first, 2 - only second, 3 - only third, ... 0 - all of them
num_phases = 3; #num_phases--;
num_harms = 49; #num_harms--;
num_recs = 560; #num_recs--; # number of recors (indexes) for one prisoed
str_label_met_count = 0;

num_all_recs = num_pris * num_recs

sheets_counter = 1;

print(num_all_recs/pris_num)
