#!/bin/bash
cd /home/rcsql/amazon
wget http://deepyeti.ucsd.edu/jianmo/amazon/categoryFiles/Gift_Cards.json.gz
wget http://deepyeti.ucsd.edu/jianmo/amazon/metaFiles2/meta_Gift_Cards.json.gz
wget http://deepyeti.ucsd.edu/jianmo/amazon/categoryFiles/Musical_Instruments.json.gz
wget http://deepyeti.ucsd.edu/jianmo/amazon/metaFiles2/meta_Musical_Instruments.json.gz
python3 amazon.py Gift_Cards
python3 amazon.py Musical_Instruments
