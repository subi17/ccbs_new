
iab dv DEFINE VARIABLE
iab dvi DEFINE INPUT PARAMETER
iab dvo DEFINE OUTPUT PARAMETER
iab dvb DEFINE BUFFER
iab ac AS CHARACTER NO-UNDO.
iab ai AS INTEGER NO-UNDO.
iab al AS LOGICAL NO-UNDO.
iab ade AS DECIMAL NO-UNDO.
iab ada AS DATE NO-UNDO.
iab ah AS HANDLE NO-UNDO.
iab psu PUT STREAM sLog UNFORMATTED
iab dsl DEFINE STREAM sLog.<enter>OUTPUT STREAM sLog TO <C-R>=expand("$HOME")
iab ff FIND FIRST
iab fe FOR EACH
iab nl NO-LOCK
iab ner NO-ERROR
iab nlw NO-LOCK WHERE
iab el EXCLUSIVE-LOCK
iab elw EXCLUSIVE-LOCK WHERE
iab alb VIEW-AS ALERT-BOX
iab msg MESSAGE
iab ina IF NOT AVAIL
iab ifa IF AVAIL
iab pss PUT SCREEN ROW 1 STRING().
iab psr PUT SCREEN ROW
iab tdo THEN DO:<enter>END.<C-O>O<space><space>
iab edo ELSE DO:<enter>END.<C-O>O<space><space>
iab dtt DEFINE TEMP-TABLE tt<enter>FIELD i AS INT<enter>INDEX i IS PRIMARY UNIQUE i.
iab funct FUNCTION fFoo RETURNS LOGICAL<enter>
\(iiValue AS INTEGER)
\:<enter><enter>END FUNCTION.

iab proc PROCEDURE pFoo:<enter><enter>END PROCEDURE.
iab idef <C-O>gg&IF "{&}" NE "YES" &THEN<enter>&GLOBAL-DEFINE  YES<enter>
\<C-O>G<enter>&ENDIF
"F5 New buffer with default progress comments
map <F5> :let tmpfile = tempname()<enter>
\:exe "e " tmpfile<enter>
\:exe "r " ccbspath . "tools/provim/template_procedure.p"<enter>
\:set syntax=progress<enter>
\:%s/\*\*_TODAY_\*\*/\=strftime('%d.%m.%y',localtime())/<enter>
\:%s/\*\*_USER_\*\*/\=expand("$USER")/<enter>
\:%s/\*\*_CUST_\*\*/ccbs/<enter>
\<C-O>G

execute "autocmd BufNewFile " . ccbspath . "/db/progress/migrations/*.py 0r" . ccbspath . "/tools/provim/template_migration.py"



"F6 Previous buffer
map <F6> :bprevious<enter>
"imap <F7> <C-O>:bprevious<enter>

"F7 Next buffer
map <F7> :bnext<enter>
"imap <F8> <C-O>:bnext<enter>

"F8 Close buffer
map <F8> :bd<enter>
"map <F8> <C-O>:bd<enter>

"F9 toggle highlight for search hits
map <F9> :nohl<enter>:set syntax=progress<enter>

function! Search_word(ext)
   let tmpfile = tempname()
   call system("find " . ccbspath . "/tms/ -name '*." . a:ext . "' 2>/dev/null | xargs grep -i '" . expand("<cword>") . "' > " . tmpfile)
   execute "e " tmpfile
endfunction

"F10 search word under cursor in *.p source files
map <F10> :call Search_word("p")<enter>

"F11 search word under cursor in *.i source files
map <F11> :call Search_word("i")<enter>

"F12 is database schema mapped [SHIFT-K]
nmap <F12> K
imap <F12> <C-O>K

"CTRL-F12 shows TMS codes for table.field under the cursor
nmap <C-F12> :call Print_magic()<enter>
imap <C-F12> <C-O>:call Print_magic()<enter>

