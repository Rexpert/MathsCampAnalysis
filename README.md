# MathsCampAnalysis

This is a repo consisting source code for analysis of **UUM Maths Camp 2019** by using `R` and its corresponding [website].

## Project Structure
Below showing the details and purposes of some important files and folders in this project:

| folder                      | description                                                                                        |
| :-------------------------- | :------------------------------------------------------------------------------------------------- |
| `assets/`                   | Folder consisting images (if available)                                                            |
| `data/`                     | Folder containing raw data files (csv, xlsx) to be processed by `R` during analysis phase          |
| `static/*.html`             | Widget files generated by using [htmlwidgets for R](https://www.htmlwidgets.org/)                  |
| `static/js/widgets/*.js`    | Widgets' dependencies                                                                              |
| `static/js/createWidget.js` | Main `js` entry                                                                                    |
| `MathCamp.R`                | Main file that contains code for the analysis                                                      |
| `index.html`                | Index file used by `Github Pages`. Also the main page of the [website]                             |
| `tidyWidgets.py`            | `Python` script to clean up all the unnecessary static `js` files generated alongside with widgets |

## Local Development
Below are some guidance for develop this project locally. Before that, there are some dependencies for both `R` and `Python` you would need to install prior for any script execution.

### Package Installation for `R`,  
In this project, we used [pacman](https://github.com/trinker/pacman) to serve as a R package management tool. Therefore, you will only need to install `pacman` once, load it using `library(pacman)` and execute the `p_load` function on the head of `MathsCamp.R`. After that, `pacman` will take care the rest for you, such as install missing packages and load packages specified.
```sh
# to install pacman initially via CRAN
install.packages("pacman")

# for other installation methods kindly visit:
# https://github.com/trinker/pacman
```


### Package Installation for `Python`,  
`Python` is needed to execute `tidyWidgets.py`. If you are using `pip`, you can install all the packages required by running the following commands on project root:
```sh
pip install -r requirements.txt
```
This will install all the dependencies needed in `tidyWidgets.py` script (e.g. `BeautifulSoup`) into your current `Python` environment.

### Get Started with Analysis
After installed `pacman`, you can now follow the steps in `MathsCamp.R` to slowly perform the data cleanup and analysis, then finally generate the corresponding widgets used in our [website].

> **Note:** at the end of the `MathsCamp.R`, there is a part labelled `system call to tidy up widgets`. The part is to perform a system call to execute `tidyWidgets.py` and clean up the excessive files generated by `htmlwidgets` `saveWidget` method.

> You might have to manually run the code to perform a clean up if you generated widgets using `saveWidget`.

### Get Started with Website
To develop the website accompanying this project locally, you can do it with any `web server`, e.g. with `python`:

```sh
# with python 3
python -m http.server 8000

# or with python 2.7 and below (not recommended)
python -m SimpleHTTPServer 8000
```
This will boot up a web server hosting on http://localhost:8000. To see the new changes, reload the page manually.

> For a better development experience, I would recommend something like `VSCode` [Live Server](https://marketplace.visualstudio.com/items?itemName=ritwickdey.LiveServer) if you are using `VSCode`, which provide a auto-reload feature for any changes.


## License

MIT © 2019 [Neoh HaiLiang](https://github.com/Rexpert)


[website]: https://rexpert.github.io/MathsCampAnalysis/