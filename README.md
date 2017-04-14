# Googler Mode

Emacs wrapper for [Googler](https://github.com/jarun/googler), a command line tool for performing Google searches.

## Installation

[Install Googler](https://github.com/jarun/googler#installation) and make sure Emacs can access the "googler" command. 

Then add googler-mode.el to your load path and require it:

    (add-to-list 'load-path "/path/to/containing/folder/")
    (require 'googler-mode)


## Usage

`M-x googler-search` 

Searches active region and returns results in a new buffer. If region isn't active, you'll be prompted for a query.

`M-x googler-autolink` 

Convert the active region to a link using the first result from Google. Will automatically format the link based on whether the buffer is org, HTML, or markdown.

### \*googler-results\* buffer

From the \*googler-results\* buffer, you can navigate between queries with the `n (googler-next)` or `p (googler-previous)` keys. To search an entry with the default browser, hit enter. To search with EWW, use `<C-return>`.

From the results buffer, you can insert a selected entry as a link in the buffer from which you searched. Hit `i i` to automatically detect the mode of the origin buffer and insert an appropriate link, or `i t` to insert the title of the entry. You can also insert all links from the results buffer with `i a t` for link titles, `i a o` for orgmode links, and so on.

Use `h` to see the full keymap while in the results buffer and `q` to quit.


