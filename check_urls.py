#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# author: Anna Klezovich
# e-mail: belkannkl@gmail.com
# date: 23.09.17

'''
This program checks validity of list of urls from the file.
'''

import urllib.request as url_mod
import csv


def urls(file):
    '''
    This function gets all urls from the data by the key 'urls' and returns them as a list.

    :param file:
    :return url_s1: list
    '''

    reader = csv.DictReader(open(file), fieldnames=['word', 'semantic field', 'image-concept association',\
                                                    'form-image association pattern', 'non iconic',\
                                                    'languages', 'urls', 'Localization',\
                                                    'Personification', 'Action', 'Parts/wholes'], dialect='excel')
    url_s = []
    for row in reader:
        url_s.append(row["urls"])
    url_s1 = url_s[1:]
    return url_s1


def check(list_of_urls):
    '''
    This function checks, whether the page can be opened.

    If not, we save its url and index. So that we can find

    the row with failed url by this index in our data afterwords.

    :param list_of_urls: list
    :return failed: dict
    '''

    failed = {}
    line = 0
    for url1 in list_of_urls:
        print(line)
        try:
            url_mod.urlopen(url1)
        except url_mod.HTTPError:
            failed[line + 2] = url1
        line += 1

    print(failed)


def main():
    check(urls("get_urls.csv"))


if __name__ == '__main__':
    main()
