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

For `R`,
```sh
TODO
```

For `Python`,  
If you are using `pip`, you can install all the packages required by running the following commands on project root:
```sh
pip install -r requirements.txt
```
This will install all the dependencies needed in `tidyWidgets.py` script (e.g. `BeautifulSoup`) into your current `Python` environment.

### Get Started with Analysis
TODO

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