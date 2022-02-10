import selenium
import chromedriver_binary
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
#from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
import csv
import time

import math

def scrape():
    #savefile = "ebrd_projects.csv"
    #with open(savefile, 'w') as csvfile:
        #writer = csv.writer(csvfile)
        #dataheader = ["Project number", "Project Title", "Location", "Business sector", "Notice type", "Environmental category", "Approval date", "Status", "PSD disclosed", "EBRD Finance", "Total Cost"]
        #writer.writerow(dataheader)
    driver = webdriver.Chrome()
    #links_scrape(driver)
    quick_scrape(driver)
    time.sleep(2)
    driver.quit()


FIPS = ['AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY']

def quick_scrape(driver):

    for fips in FIPS:
        driver.get('https://lehd.ces.census.gov/data/lodes/LODES7/{code}/wac/{code}_wac_S000_JT00_2010.csv.gz'.format(code = fips.lower()))

        driver.get('https://lehd.ces.census.gov/data/lodes/LODES7/{code}/wac/{code}_wac_S000_JT00_2014.csv.gz'.format(code = fips.lower()))

        driver.get('https://lehd.ces.census.gov/data/lodes/LODES7/{code}/rac/{code}_rac_S000_JT00_2010.csv.gz'.format(code = fips.lower()))

        driver.get('https://lehd.ces.census.gov/data/lodes/LODES7/{code}/rac/{code}_rac_S000_JT00_2014.csv.gz'.format(code = fips.lower()))


def links_scrape(driver):
    wait = WebDriverWait(driver, 5)
    main_link = 'https://lehd.ces.census.gov/data/'


    driver.get(main_link)
    region_button = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="lodes_download_area"]/p[1]/span[2]')))
    #region_button = driver.find_element_by_xpath()
    type_button = driver.find_element_by_xpath('//*[@id="lodes_type"]')
    load_button = driver.find_element_by_xpath('//*[@id="lodes_files_load"]')
    rac_button = driver.find_element_by_xpath('//*[@id="lodes_type"]/option[2]')
    rac_button.click()

    for i in range(53):
        if i!= 44 and i!=11 and i!=8 and i!=39:
            region_button.click()
            state = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="lodes_state"]/option[{num}]'.format(num = i+1))))
            state.click()
            load_button.click()
            time.sleep(1)

            file1 = wait.until(EC.presence_of_element_located((By.XPATH,'//*[@id="lodes_file_list"]/div/div[9]/div[1]/a')))
            file1_link = file1.get_attribute('href')
            driver.get(file1_link)
            time.sleep(0.5)
            file2 = wait.until(EC.presence_of_element_located((By.XPATH,'//*[@id="lodes_file_list"]/div/div[13]/div[1]/a')))
            file2_link = file2.get_attribute('href')
            driver.get(file2_link)
            time.sleep(0.5)
            #//*[@id="lodes_file_list"]/div/div[9]/div[1]/a
            #//*[@id="lodes_file_list"]/div/div[13]/div[1]/a


    wac_button = driver.find_element_by_xpath('//*[@id="lodes_type"]/option[3]')
    wac_button.click()

    for i in range(53):
        if i!= 44 and i!=11 and i!=8 and i!=39:
            region_button.click()
            state = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="lodes_state"]/option[{num}]'.format(num = i+1))))
            state.click()
            load_button.click()
            time.sleep(1)



            file1 = wait.until(EC.presence_of_element_located((By.XPATH,'//*[@id="lodes_file_list"]/div/div[9]/div[1]/a')))
            file1_link = file1.get_attribute('href')
            driver.get(file1_link)
            time.sleep(1)
            file2 = wait.until(EC.presence_of_element_located((By.XPATH,'//*[@id="lodes_file_list"]/div/div[13]/div[1]/a')))
            file2_link = file2.get_attribute('href')
            driver.get(file2_link)
            time.sleep(1)
            #//*[@id="lodes_file_list"]/div/div[9]/div[1]/a
            #//*[@id="lodes_file_list"]/div/div[13]/div[1]/a




if __name__ == '__main__':
    scrape()
