# fman

A snapshot (2021-12-29) of the base source for the Fortran intrinsic
descriptions at fortran-lang.org.

You can contribute directly to the documents at fortran-lang.org but if
it is easier you can contribute _demo programs in particular_ here as
well and I can periodically merge them back into the repository as PR
requests, as setting up to work with the minibooks locally can require
quite a bit of infrastructure (but try that first).

They are now maintained as kramdown files (a variant of markdown).

Stylistically the files follow a formalized structure based on man-pages.
This allows for them to easily be converted to actual man-pages for
installallation in ULEs (Unix-Like Environments), and to readily be
parsed for other uses like the Fortran fman(1) program.

Since there are skeletons for the vast majority of the intrinsics in
place I think the required structure is obvious by example, but let me
know if not.

Using the markdown format allows for readily converting to other formats
(including other markdown variants) using utilities like pandoc(1); as the
particular variant currently being used (kramdown) is likely to change.

Note that each file should have a "program demo_$NAME" and "end program
demo_$NAME" line in it so the primary example program can easily be
extracted, as with fman(1). This is particularly useful for automatic
verification of the demo programs.

There was a vote on the Fortran Discourse forum, and having a web
page description that can include graphics, links, and mathematics was
important enough that the CLI interfaces such as fman(1) may suffer,
as current methods do not represent such material accurately (although
if web pages are accessible CLI web browser commands like w3m(1),
links(1), and lynx(1) might make that a mute point). This directly
lead to deciding to keep the primary source of the documentation in the
minibook-compatible format (previously, the primary source was kept in
a format used to maintain the GPF (General-Purpose Fortran) repository,
but that is no longer pertinent).

Since the kramdown files are now the primary documents utilities like
pandoc(1) are a good avenue for converting the files to other formats.

Note that to display on the fortran-lang.org the files are automatically
converted to HTML files; so there are HTML versions of each document
relatively accessible to everyone.

## Directly making PRs to fortran-lang.org

If you go to the fortran-lang.org site and see how (currently) to
directly contribute to the mini-books you can find more information.

As an example, on a Linx box something like this (it will probably need
tweaked -- see fortran-lang.org site for details) will download my PR
to the site, and set you up to modify the intrinsics files and make pull
requests to change them

```bash
#!/bin/bash
git clone https://github.com/urbanjost/fortran-lang.org
cd fortran-lang.org
sudo apt install jekyll
sudo apt install ruby-dev
sudo gem install bundler
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
bundle config set path $HOME/'.bundle'
bundle install
# Serving locally
bundle exec jekyll serve --watch &
# to view the website.
$VIEWER http://localhost:4000
```

where $VIEWER is your browser and/or xdg-open, gnome-open, cygstart,
start, open, konquerer, nautilus ...

## fman

There were other paths taken, so on a ULS with pandoc installed you can
probably build some real man-pages and some interesting HTML versions of
the documentation, but since formulas and links and graphics are being
added this will become less useful, but is the main way I look up the
routines, and man-pages can be called from inside of the vim(1) editor,
and I like to extract the example codes or other parts of the documents
straight into code files so the adventurous can play with installing
pandoc(1) and running

```bash
bash scripts/all.sh
```

which generates man-pages and the `fman` program and some files in the docs/ directory
if successful.

fman (Like `man`, but for Fortran man-pages) is a Fortran program
that displays descriptions of the Fortran intrinsics.  It is
platform-independent, so it is particularly useful for command-line use
on platforms without manpages. `fman` can

   + display a short description of each topic including category keys
   + display a description of any standard Fortran intrinsic
   + allow capture of a small working example program for most topics
   + can search the TOC (Table of Contents) or all descriptions with regular expressions

To build it requires `git`, `gfortran`, and `fpm`(Fortran Package Manager).
See [plugins](https://github.com/urbanjost/plugins).

```bash
   # TRY IT

   # if you placed the program in a directory in your command path you
   # are ready to go!
   fman --help
   fman
   fman -d sin
   fman tan
   fman --regex 'charact'
   fman manual >book.txt
   fman not -c |less -r  # if your terminal emulator supports ANSI color control sequences
```
### Using fman(1) with vim(1)

From the vim(1) editor to extract just the demo program
into the current position use
```text
   :r! fman -d date_and_time
```

### Using fman(1) with vip(1) and bash(1)

The vip(1) script lets you use the vi(1) command with pipes.  See the
bash/ directory for a sample. On ULS platforms you can start up the editor
on the fman(1) output. This is my favorite mode.

```bash
fman tan|vip
```
Add this to your ~/.bashrc file
```bash
fm(){
(
case "$*" in
*-d*) export EDITOR='vim -c "set filetype=fortran"' ;;
*)    export EDITOR='vim -c "set filetype=man"'     ;;
esac
vip fman $*
)
}
```
And you can now enter
```bash
fm tan
```
and be editing the man-page for tan(3f).

## REFERENCES

 + The fpm(1) home page is at [https://github.com/fortran-lang/fpm](https://github.com/fortran-lang/fpm).
   It includes instructions on **installing** from a binary or how to bootstrap from source.

 + For defining the man-page text, creating a CLI(command line inteface), allowing for regular
   expressions and common string functions and adding ANSI in-line color control sequences
   it uses several `fpm` dependencies:
    + [M_CLI2](https://github.com/urbanjost/M_CLI2.git)
    + [M_match](https://github.com/urbanjost/M_match.git)
    + [M_strings](https://github.com/urbanjost/M_strings.git)
    + [M_escape](https://github.com/urbanjost/M_escape.git)
  + These scripts are useful in particular with `fman`:
    + txt2man
    + vip.sh
## NOTES

Note I am having getting formulas to work using LaTex.
