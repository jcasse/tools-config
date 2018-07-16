[comment]: # (Markdown: dillinger.io/ shows a nice example of Markdown commands with a viewer.)
[comment]: # (Comments in Markdown: http://stackoverflow.com/questions/4823468/comments-in-markdown)
[comment]: # (C++ Project Structure: http://hiltmon.com/blog/2013/07/03/a-simple-c-plus-plus-project-structure/)
[comment]: # (C++ Library Creation: http://www.adp-gmbh.ch/cpp/gcc/create_lib.html)

# Tools Configuration

Configuratoin files for the various tools I use in Linux and OS X.

```sh
git clone git@github.com:jcasse/tools-config.git ~/Dropbox/tools-config
```

## Emacs

### Set up

1. In a shell,
```sh
mv ~/.emacs ~/.emacs.bak
mv ~/.emacs.d ~/.emacs.d.bak
ln -s ~/Dropbox/tools-config/emacs/.emacs.d ~/.emacs.d
```
2. Open Emacs and type the following command.
`M-x package-install RET use-package RET`

### References

+ https://www.gnu.org/software/emacs/

## Vim

Includes a pluging for LaTeX by `xuhdev`:
Type the command `:LLPStartPreview` from within vim to launch the previewer.

### Set up

```sh
mv ~/.vimrc ~/.vimrc.bak
ln -s ~/Dropbox/tools-config/.vimrc ~/.vimrc
ln -s ~/Dropbox/tools-config/.vim ~/.vim
~/Dropbox/tools-config/vim/powerline/fonts/install.sh
Copying fonts...
Resetting font cache, this may take a moment...
Powerline fonts installed to ~/.local/share/fonts
```

### References

+ https://vi.stackexchange.com/questions/5622/how-do-i-configure-the-vim-airline-plugin-to-look-like-its-own-project-screensho
+ https://github.com/xuhdev/vim-latex-live-preview.

### Set up

## Bash

### Set up (OS X)

```sh
mv ~/.bash_profile ~/.bash_profile.bak
ln -s ~/Dropbox/tools-config/.bash_profile ~/.bash_profile
```

### Set up (Linux)

```sh
mv ~/.bashrc ~/.bashrc.bak
ln -s ~/Dropbox/tools-config/.bashrc ~/.bashrc
```

### References

+ https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html

License
----

[comment]: # "A short snippet describing the license (MIT, Apache, etc.)"

[comment]: # (http://choosealicense.com/)

Copyright (C) 2018 Juan Casse

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

## TODO:

* [ ] tmux
* [ ] screen
