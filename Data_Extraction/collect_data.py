#!/usr/bin/python

import os
import requests

from bs4 import BeautifulSoup

def create_dir(dirs):
    for directory in dirs:
        if not os.path.exists(directory):
            os.makedirs(directory)

# Create output directory if it does not exist already
out_dir = "addresses"
out_html_dir = "addresses_html"
out_clean_dir = "addresses_cleaned"
create_dir([out_dir, out_html_dir, out_clean_dir])

# Scrapping part
headers = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/76.0.3809.100 Safari/537.36'}
base_url = "http://stateoftheunion.onetwothree.net/texts"

index_url = base_url + "/index.html"
r = requests.get(index_url, headers=headers)

soup = BeautifulSoup(r.text, 'html.parser')

print("-"*85)
print("File\t\t\t|President\t\t\t|Address Date\t\t|Done")

for link in soup.find("div", {"id": "text"}).ul.find_all("a"):
    address_url = base_url + "/" + link["href"]
    address_file_name = link["href"].split(".")[0] + ".txt"
    address_file = out_dir + "/" + address_file_name
    address_html_file = out_html_dir + "/" + address_file_name
    address_clean_file = out_clean_dir + "/" + address_file_name

    r = requests.get(address_url, headers=headers)
    s = BeautifulSoup(r.text, 'html.parser')
    text_div = s.find("div", {"id": "text"})
    president = text_div.find("h2").text
    address_date = text_div.find("h3").text

    print("{}\t\t|{:31s}|{:23s}|{}".format(address_file_name, president, address_date, ".."), end="\r")

    with open(address_file, "w") as fh:
        for p in text_div.find_all("p"):
            fh.write(p.text + "\n")

    with open(address_html_file, "w") as fh:
        fh.write(r.text)

    # Clean the file. Remove punctuation, html and stop words
    html_removed_text = r.get_text()
    with open(address_clean_file, "w") as fh:

    print("{}\t\t|{:31s}|{:23s}|{}".format(address_file_name, president, address_date, u"\u2713  "),)
    print("-"*85)

