
import csv

#this program converts the state laws database collecting the sourced data from Durs
# the original database is in 1's and 0's for if the laws exist, the new database will list
#   each state's categories for which there are laws

original_file_name = "statelawsdatabase1.csv"
new_file_name = "statelawsdatabase2.csv"
header = []
rows = []
with open(original_file_name, 'r') as file:
    csvreader = csv.reader(file)
    header = next(csvreader)
    for row in csvreader:
        rows.append(row)

new_header = ["State", "Laws"]

new_rows = []

for row in rows:
    new_row = [row[0]]
    for elementnum in range(2,len(row)):
        if row[elementnum] == "1":
            new_row.append(header[elementnum])
    new_rows.append(new_row)
    print(new_row)

with open(new_file_name, 'w') as csvfile:
    csvwriter = csv.writer(csvfile)
    csvwriter.writerow(new_header)
    csvwriter.writerows(new_rows)
