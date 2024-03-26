import os
from os import path
import time
import json
import fitz
import xlrd
import shutil
import requests


from datetime import date
from dateutil.parser import parse as parsedate

data_path = path.join('data')
os.makedirs(path.join(os.getcwd(), data_path, 'NCHS'), exist_ok=True)
os.makedirs(path.join(os.getcwd(), data_path, 'NCHS', 'death'), exist_ok=True)
os.makedirs(path.join(os.getcwd(), data_path, 'NCHS', 'death', 'output_data'), exist_ok=True)
out_path = path.join(os.getcwd(), data_path, 'NCHS', 'death', 'output_data')


class DeathExtractor:
    """ Using the BeautifulSoup to extract the data table embedded in the pdf
        The mortality data prior 1999, by 5-year age groups, sex, causes of deaths (with cause name and code in 113 code list)
    """

    def __init__(self):
        self.today = date.today().strftime("%Y-%m-%d")

    def get_data_upto44(self):
        url = 'https://www.cdc.gov/nchs/data/statab/hist002_1.pdf'
        ## updated daily

            r = requests.get(url)
            r.raise_for_status()

            with open("data/NCHS/death/death_0-44.pdf", "wb") as f:
                f.write(r.content)
            doc = fitz.Document(path.join(os.getcwd(), data_path, 'NCHS', 'death',"death_0-44.pdf"))
            # find the page
            # from page 2 will extract for each table and then filter what we want
            finalLines = doc.get_page_text(-1).splitlines()
            final_page = int(finalLines[1].split('Page ')[1])
            for page_num in range(1,(final_page+1)):
                lines = doc.get_page_text(page_num).splitlines()
                # age_groups = [lines[5], lines[6], lines[7], lines[8], lines[9].split(' years')]
                age_groups = [lines[5], lines[6], lines[7], lines[8], '10-14 years', '15-19 years', '20-24 years', '25-29 years',
                              '30-34 years', '35-39 years', '40-44 years']
                cause_name = lines[10]
                race, sex = lines[11].split(', ')
                death_data = {}
                death_data['age'] = age_groups
                death_data['race'] = race
                death_data['sex'] = sex
                death_data['cause_name'] = cause_name
                for yr_count in range(1, 21):
                    death_data[lines[12 * yr_count]] = lines[(12 * yr_count + 1):(12 * (yr_count + 1))]
                with open(path.join(out_path, 'first-table_page-{}.json'.format(page_num)), "w") as f:
                    json.dump(death_data, f)
                print('\n------ Processed first table in page {} ------\n'.format(page_num))
                print(death_data)

                # processing for the second table
                cause_name = lines[252]
                race, sex = lines[253].split(', ')
                death_data = {}
                death_data['age'] = age_groups
                death_data['race'] = race
                death_data['sex'] = sex
                death_data['cause_name'] = cause_name
                for yr_count in range(0, 20):
                    death_data[lines[12 * yr_count + 254]] = lines[(12 * yr_count + 1 + 254):(254 + 12 * (yr_count + 1))]
                with open(path.join(out_path, 'second-table_page-{}.json'.format(page_num)), "w") as f:
                    json.dump(death_data, f)
                print('\n------ Processed second table in page {} ------\n'.format(page_num))
                print(death_data)


    def get_data_45_plus(self):
        url = 'https://www.cdc.gov/nchs/data/statab/hist002_2.pdf'
        ## updated daily

        r = requests.get(url)
        r.raise_for_status()

        with open("data/NCHS/death/death_45plus.pdf", "wb") as f:
            f.write(r.content)
        doc = fitz.Document(path.join(os.getcwd(), data_path, 'NCHS', 'death', "death_45plus.pdf"))
        # find the page
        # from page 2 will extract for each table and then filter what we want
        finalLines = doc.get_page_text(-1).splitlines()
        final_page = int(finalLines[1].split('Page ')[1])
        for page_num in range(1, (final_page + 1)):
            lines = doc.get_page_text(page_num).splitlines()
            # age_groups = [lines[5], lines[6], lines[7], lines[8], lines[9].split(' years')]
            age_groups = ['All ages', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years',
                          '70-74 years', '75-79 years', '80-84 years', '85 + years', 'Age not states']
            cause_name = lines[10]
            race, sex = lines[11].split(', ')
            death_data = {}
            death_data['age'] = age_groups
            death_data['race'] = race
            death_data['sex'] = sex
            death_data['cause_name'] = cause_name
            for yr_count in range(1, 21):
                death_data[lines[12 * yr_count]] = lines[(12 * yr_count + 1):(12 * (yr_count + 1))]
            with open(path.join(out_path, 'first-second-part_table_page-{}.json'.format(page_num)), "w") as f:
                json.dump(death_data, f)
            print('\n------ Processed first table in page {} ------\n'.format(page_num))
            print(death_data)

            # processing for the second table
            cause_name = lines[252]
            race, sex = lines[253].split(', ')
            death_data = {}
            death_data['age'] = age_groups
            death_data['race'] = race
            death_data['sex'] = sex
            death_data['cause_name'] = cause_name
            for yr_count in range(0, 20):
                death_data[lines[12 * yr_count + 254]] = lines[(12 * yr_count + 1 + 254):(254 + 12 * (yr_count + 1))]
            with open(path.join(out_path, 'second-second-part_table_page-{}.json'.format(page_num)), "w") as f:
                json.dump(death_data, f)
            print('\n------ Processed second table in page {} ------\n'.format(page_num))
            print(death_data)

if __name__ == "__main__":
    Extractor = DeathExtractor()
    try:
        print("\n### Processing for pdf for people younger than 45 ###\n")
        Extractor.get_data_upto44()
    except:
        print("\n!!!FAILED !!!\n")

    try:
        print("\n### Processing for pdf for people older than 45 ###\n")
        Extractor.get_data_45_plus()
    except:
        print("\n!!!FAILED !!!\n")
