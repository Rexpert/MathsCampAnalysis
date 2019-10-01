# pylint: disable=redefined-outer-name

'''
This is a module that update widget files (scores.html, confident.html, etc)
to a smaller size without all the unnecessary scripts, such as leaflet, mapbox,
etc from echarts.

Firstly, it will remove the unwanted tags from each of the widget files,
for example, remove all the script and link tags from widget <head> tag.
Then, it will remove all the corresponding folders belong to widget, such as
scores_files for scores.html, confidence_files for confidence.html and so on.
'''

import os
import stat
import re
from shutil import rmtree
from typing import List, Dict


# import path
import pathlib
from bs4 import BeautifulSoup


# ------------------- Callback ------------------- #
def remove_readonly(func, path, _):
    '''Error function for shutil's rmtree.
       Clear the read-only bit and reattempt the removal.
       This is especially important since Windows files have their own
       read-only bit
    '''
    os.chmod(path, stat.S_IWRITE)
    func(path)
# ------------------- Callback ------------------- #


def clear_existing_scripts(soup: BeautifulSoup):
    '''Clear existing <script> tags in widget file.
       Existing <script> tags included those in <head> and <body>,
       except the one storing json and htmlwidget data.

       Args:
         soup: BeautifulSoup document
    '''
    script_types_to_remain = [
        'application/json',
        'application/htmlwidget-sizing'
    ]

    for s in soup.find_all('script'):
        # skip if the script is json or htmlwidget related
        script_type = s.get('type')
        if script_type in script_types_to_remain:
            continue

        s.decompose()


def remove_head_links(soup: BeautifulSoup):
    '''Remove all <link> tags in widget <head>

       Args:
         soup: BeautifulSoup document
    '''
    for l in soup.head.find_all('link'):
        l.decompose()


def add_body_scripts(soup: BeautifulSoup, filename: str):
    '''Add <script> tags needed to widget <body>.
       All the scripts needed are included in /static/js/widgets

       Note: echarts-liquidfill.min.js is only included in liquid.html widget

       Args:
         soup: BeautifulSoup document
         filename: widget file name
    '''
    scripts_src_to_add = [
        'js/widgets/htmlwidgets.js',
        'js/widgets/echarts.min.js',
        'js/widgets/echarts4r.js',
        'js/widgets/echarts-liquidfill.min.js'
    ]

    for src in scripts_src_to_add:
        # if the file is not liquid.html
        # do not add echarts-liquidfill.min.js to it
        if src == 'js/widgets/echarts-liquidfill.min.js' \
           and filename.find('liquid') == -1:
            continue

        new_script_tag = soup.new_tag('script', src=src)
        soup.body.append(new_script_tag)


def write_changes(soups_with_filename: List[Dict]):
    '''Write the changed Beautiful Soup document back to widget file.
       In addition, prettify the soup before writing

       Note: the original widget file will be overwritten

       Args:
         soups_with_filename: List with dictionary elem that contains
                            filename and a Beautiful Soup document
    '''
    for d in soups_with_filename:
        name = d['filename']  # type: str
        prettified_soup = d['soup'].prettify()  # type: BeautifulSoup

        with open(name, 'w') as f:
            f.write(str(prettified_soup))


def delete_folders(folder_path: pathlib.Path):
    '''Delete corresponding folder with respect to each widget file
       e.g. for scores.html widget, its corresponding folder is
            scores_files

       Args:
         folder_path (pathlib_path): parent folder path for folders to delete.
    '''
    pattern = r'.*(?=\.html)'  # match filename if the extension is .html
    widget_filename_regex = re.compile(pattern)

    # remove unneeded script files and directories
    # make sure the curdir now is static/ from root
    for f in folder_path.glob('*.html'):
        filename = f.name
        basename = widget_filename_regex.search(filename)

        # remove folder with the name <basename>_files if widget is found
        if basename:
            folder_name = basename[0] + '_files'

            try:
                rmtree(folder_name, onerror=remove_readonly)
                print(f'deleted {folder_name}...')
            except FileNotFoundError:
                print(f'Folder not found: {folder_name}. Failed to delete.')


if __name__ == "__main__":
    # cd to static folder
    os.chdir(os.path.join(os.curdir, 'static'))
    print(os.path.abspath(os.curdir))

    # find all html files in curdir (static folder)
    static_dir = pathlib.Path(os.curdir)
    soups_with_filename = []

    # save all widgets' soup and their filename into soups
    for f in static_dir.glob('*.html'):
        with open(f) as html_file:
            txt = html_file.read()
            d = {
                'filename': html_file.name,
                'soup': BeautifulSoup(txt, 'lxml')
            }
            soups_with_filename.append(d)

    # clear existing script tags and links
    # then append the necessary script tags to body
    for d in soups_with_filename:
        name = d['filename']  # type: str
        soup = d['soup']
        clear_existing_scripts(soup)
        remove_head_links(soup)
        add_body_scripts(soup, name)

    print('done removing scripts and links')
    print('updating changes to files...')

    # write soups' changes to file
    write_changes(soups_with_filename)

    print('done updates...')
    print('deleting widgets folders...')

    # delete lib folders corresponding to each widget
    delete_folders(static_dir)

    print('done.')
