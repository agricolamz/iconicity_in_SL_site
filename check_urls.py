#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# author: Anna Klezovich

import urllib.request as url_mod
import csv


def urls(file):

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
