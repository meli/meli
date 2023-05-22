" Place this plugin in
"
" `$HOME/.vim/after/ftplugin/mail.vim` for vim
" `$HOME/.config/nvim/after/ftplugin/mail.vim` for neovim

" Don't use modelines in e-mail messages
setlocal nomodeline
setlocal textwidth=72

"                                                         *fo-a*
" a Automatic formatting of paragraphs.
"   Every time text is inserted or deleted the paragraph will be reformatted.
"                                                         *fo-w*
" w Trailing white space indicates a paragraph continues in the next line.
"   A line that ends in a non-white character ends a paragraph.
"                                                         *fo-q*
" q Allow formatting of comments with "gq".
"                                                         *fo-t*
" t Auto-wrap text using textwidth
"                                                         *fo-r*
" r Automatically insert the current comment leader after hitting <Enter> in
" Insert mode.
"                                                         *fo-c*
" c Auto-wrap comments using textwidth, inserting the current comment leader
" automatically.
"                                                         *fo-2*
" 2 When formatting text, use the indent of the second line of a paragraph for
"   the rest of the paragraph, instead of the indent of the first line.
"   This supports paragraphs in which the first line has a different indent than 
"   the rest.
"   Note that 'autoindent' must be set too.
" Example:
"         first line of a paragraph
"     second line of the same paragraph
"     third line.
" This also works inside comments, ignoring the comment leader.
setlocal formatoptions=aqtw2r

" Disable adding two spaces after '.', '?' and '!' with a join command.
setlocal nojoinspaces

" Disable smartident (meant for source code)
setlocal nosmartindent

"       *'comments'* *'com'* *E524* *E525*
" A comma-separated list of strings that can start a comment line.
"   See |format-comments|.
"   See |option-backslash| about using backslashes to insert a space.
"
"
"  The 'comments' option is a comma-separated list of parts.
"  Each part defines a type of comment string.
"  A part consists of: {flags}:{string}
"
"  {string} is the literal text that must appear.
"
"  {flags}:
"  n  Nested comment.
"  Nesting with mixed parts is allowed.
"  If 'comments' is "n:),n:>" a line starting with "> ) >" is a comment.
"
"  b Blank (<Space>, <Tab> or <EOL>) required after {string}.
setlocal comments+=nb:>

" Highlight trailing whitespace as errors.
match ErrorMsg '\s\+$'

" MAIL            *mail.vim* *ft-mail.vim*
" By default mail.vim synchronises syntax to 100 lines before the first
" displayed line.
" If you have a slow machine, and generally deal with emails with short 
" headers, you can change this to a smaller value:

let mail_minlines = 30


"          *no_mail_maps* *g:no_mail_maps*
" Disable defining mappings for a specific filetype by setting a variable,
" which contains the name of the filetype.
" For the "mail" filetype this would be:
let no_mail_maps = 1

" Local mappings:
" <LocalLeader>q   or   \\MailQuote
"   Quotes the text selected in Visual mode, or from the cursor position
"   to the end of the file in Normal mode.
"   This means "> " is inserted in each line.
