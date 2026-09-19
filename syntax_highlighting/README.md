# Syntax highlighting

The Catala language also comes with syntax highlighting to
ease program development. The syntax highlighting is done, among other
techniques, with the [Iro](https://eeyo.io/iro/) compiler that allows
writing the syntax only once, and then export it to formats
understood by various IDE.

**DISCLAIMER:**

The maintenance of the various syntax highlighting plugins is *BEST-EFFORT*.
Indeed many of the plugins were developed by the community and their creators
did not commit to long-term maintenance.

## Atom

To get Catala syntax highlighting in Atom, simply enter from
the root of the repository, depending on the language you want to use :

    make atom_fr

or

    make atom_en

You can now reload Atom and check that you have syntax highlighting on any `.catala` file.

## VSCode

To get Catala syntax highlighting in VSCode, simply enter from
the root of the repository, depending on the language you want to use :

    make vscode_fr

or

    make vscode_en

You can now reload VSCode and check that you have syntax highlighting on any `.catala` file.

## Pygments

Pygments is a Python-based versatile lexer for various programming languages. To
use a version of Pygments augmented with the Catala plugin, simply enter from
the root of the repository

    make pygments

This will setup a Python virtual environment ("venv"), and install the syntax
highlighting plugins that allow Pygments to handle Catala files. Those are
defined in `syntax_highlighting/XX/pygments/`.

Pygments is used for instance by the `minted` LaTeX package. To make sure it is
available, you need to "activate" the python venv each time using:

    . _python_venv/bin/activate

## GNU gedit

Copy the file [catala.lang](./gnu_gedit/catala.lang) to the directory below (create if it does not exist) and then restart GEdit.
```
~/.local/share/gtksourceview-4/language-specs
```
