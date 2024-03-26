import os
from os import path
import time
import json
import fitz
import re
import requests


from datetime import date
from dateutil.parser import parse as parsedate

data_path = path.join('data')
os.makedirs(path.join(os.getcwd(), data_path, 'NCHS'), exist_ok=True)
os.makedirs(path.join(os.getcwd(), data_path, 'NCHS', 'death'), exist_ok=True)
os.makedirs(path.join(os.getcwd(), data_path, 'NCHS', 'death', 'raw'), exist_ok=True)
out_path = path.join(os.getcwd(), data_path, 'NCHS', 'death', 'raw')


class DeathExtractor:
    """ Using the BeautifulSoup to extract the data table embedded in the pdf
        The mortality data prior 1999, by 5-year age groups, sex, causes of deaths (with cause name and code in 113 code list)
    """

    def __init__(self):
        self.today = date.today().strftime("%Y-%m-%d")

    def get_page(self):
            url = 'https://data.nber.org/mortality/1993/dt93icd9.pdf'
             ## updated daily
            r = requests.get(url)
            r.raise_for_status()

            with open("data/NCHS/death/raw_nchs/descp_1993.pdf", "wb") as f:
                f.write(r.content)
            doc = fitz.Document(path.join(os.getcwd(), data_path, 'NCHS', 'death', 'raw_nchs', "descp_1993.pdf"))
            # find the page
            # from page 180 - 222
            for page_num in range(184,226):

                lines = doc.get_page_text(page_num).splitlines()
                # find the location of the cause names
                name_loc = {}
                j = 0
                lines[2:]
                for i in range(22,len(lines)-2):
                    if re.match("[A-Z]", lines[i]):
                        name_loc[j] = i
                        j = j + 1

                ratio = {}
                cause_113_code = {}
                cause_name = {}
                comp_ratio = {}
                comp_sd = {}
                for cause_num in name_loc.values():
                    print(cause_num)
                    # check if the cause name occupied two lines, then the next element would be a character
                    if not re.match('[A-Z]', lines[cause_num + 1]):
                         if re.match('[0-9]', lines[cause_num + 3]) or re.match('\\*', lines[cause_num + 3]):
                            print('\n------ Processed ratio with cause name {} ------\n'.format(lines[cause_num]))

                            cause_113_code[cause_num] = lines[cause_num-1]
                            cause_name[cause_num] = lines[cause_num]
                            comp_ratio[cause_num] = lines[cause_num + 3]
                            comp_sd[cause_num] = lines[cause_num + 4]

                ratio['cause_113_code'] = cause_113_code
                ratio['cause_name'] = cause_name
                ratio['comp_ratio'] = comp_ratio
                ratio['comp_sd'] = comp_sd

                with open(path.join(out_path, 'table_ratio_page-{}.json'.format(page_num+1)), "w") as f:
                    json.dump(ratio, f)
                print('\n------ Processed ratio table in page {} ------\n'.format(page_num+1))
                print(comp_ratio)


if __name__ == "__main__":
    Extractor = DeathExtractor()
    try:
        print("\n### Processing for comparability ratios in pdfs ###\n")
        Extractor.get_page_ratio()
    except:
        print("\n!!!FAILED !!!\n")
