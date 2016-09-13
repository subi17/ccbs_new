set shiftwidth=3
set tabstop=3
set expandtab
set ignorecase
set foldmethod=syntax
normal zR

execute "set path+=" . ccbspath . "/tms"
set suffixes+=.r


"*******************************+
" Table description with Shift-K
"*******************************+
function! Print_tabledesc()
    execute "! pike -C " . g:ccbspath . "/tools/docgen/tbldesc <cWORD>"
endfunction
nmap K :call Print_tabledesc()<enter>

"*************************
" Omni-expansion CTRL-X o
"*************************
function! Search_db_dict(findstart, base)
    if a:findstart == 1
        let founddot = search(" ", "b", line("."))
        if founddot == 0
            return 0
        endif
        return col(".")
" alternatively:  expand("<cfile>") and match(getline(".")...) ??
    endif

    let dict = g:ccbspath . "/db/progress/store/cache/"
    let result = []
    let idfr = substitute(a:base, '\.', '\\.', 'g')
    if match(idfr, "\\.") > -1
        let output = system("grep -i '^" . idfr . "' " . dict . "dict")
    else
        let output = system("grep -i '^" . idfr . "' " . dict . "tabledict")
    endif
    let matches = split(output, "\n")
    let n = 0
    while n < len(matches)
        let entries = split(matches[n], ";")
        if len(entries) == 1
            call add(result, entries[0])
        else
            let item = {}
            let item['word'] = entries[0]
            let item['menu'] = entries[1]
            let item['icase'] = 1
            call add(result, item)
        endif
        if len(entries) == 3
            let item = {}
            let item['word'] = entries[0] . " " . entries[2]
            let item['abbr'] = " + FORMAT"
            let item['icase'] = 1
            call add(result, item)
        endif
        let n = n + 1
    endwhile
    return result
endfunction

set omnifunc=Search_db_dict
set pumheight=10

"***********************+
" Compilation (via make)
"***********************+
set autowrite
set efm=%Z**\ %f\ Could\ not\ understand\ line\ %l.\ (%.%#),
       \%E**\ Unable\ to\ understand\ %m.\ (%.%#),
       \%C**\ %m.\ (254),
       \%E**\ %m\ (%n),
       \%-Gmake:\ ***\ [%.%#]\ %.%#\,
       \%-ACompiling\ %.%#,
       \%EThere\ is\ %m.\ (1423)

execute "set makeprg=pike\\ -C\\ " . g:ccbspath . "/tms\\ %:p:s?" . g:ccbspath . "/tms/??"

nmap <F2> :make<enter><enter>

function! Run_4gl()
    let l:pfile = tempname() . ".p"

    execute "redir > " . l:pfile
    silent echo "DO ON STOP UNDO, LEAVE: "
    redir END
    execute "silent write !cat >> " . l:pfile
    execute "redir >> " . l:pfile
    silent echo " PAUSE."
    silent echo "END."
    silent echo "QUIT."
    redir END
    execute "silent ! pike terminal " . l:pfile

endfunction

nmap <F1> :call Run_4gl()<enter>
imap <F1> <C-O>:call Run_4gl()<enter>


function! Interpret_output(output, lineoffs)
    redraw
    let l:pos = match(a:output, "\\*\\* [^ ]* Could not understand line ")
    if l:pos == -1
        return a:output
    else
        let l:lineno = strpart(a:output, matchend(a:output, " Could not understand line "), 4) + 0
        let l:lineno = l:lineno + a:lineoffs
        execute "normal " . l:lineno . "G"
        let l:endpos = matchend(a:output, "Could not understand line [0-9]*\\. ([0-9]*)")
        return "Line " . l:lineno . ": " . strpart(a:output, 0,  l:pos - 1) . strpart(a:output, l:endpos)
    endif
endfunction


function! Check_syntax() range

    " Let assume that the file is .p file fisrt
    let l:extension = ".p"

    " If there is a line which begins with a word class (case-insensitive) then this is a cls file
    let l:classline = search('\c^class\s', 'nc')

    " If l:classline is something else than zero we have a class file
    if l:classline
        " Lets store the class name it is the word after class word (\c is case-insensitive mark)
        " also there must be whitespace character after the word class
        " finally we take every characters until whitespace or : character
        let l:classfullname = matchstr(getline(l:classline), '\cclass\s*\zs[^ :]*\ze')
        if empty(l:classfullname)
            throw "no class name recognized into ''" . getline(l:classline) . "''"
        endif
        let l:extension = ".cls"

        " If there is a path in class name lets get the actual class name
        let l:classname = matchstr(l:classfullname,'.*\.\zs\w*\ze')

        " Add whitespace in order to get safer substitute later on
        let l:classfullname = ' ' . l:classfullname

        if empty(l:classname)
            let l:classname = l:classfullname
        else
            let l:classname = ' ' . l:classname
        endif
    endif

    let l:saveline = line(".")
    let l:addlines = 0
    let l:tempname = tempname()

    " We need to take temppathname and tempbasename as Progress class needs
    " character name (we will add 'temp' to the temp file name)
    " Also we need to replace class name with tempbasename to the temp file
    let l:temppathname = matchstr(l:tempname,'^\zs.*/\ze[^/]*$')
    let l:tempbasename = 'temp' . matchstr(l:tempname,'.*/\zs\w*\ze')

    let l:tempfile = l:temppathname . l:tempbasename . l:extension

    " Add whitespace in order to get safer substitute later on
    let l:tempbasename = ' ' . l:tempbasename

    " If this is class file
    if l:classline
        let l:linenr = 0
        execute "redir > " . l:tempfile
        while l:linenr < line("$")
            let l:linenr += 1
            if l:linenr == l:classline
                silent echo substitute(getline(l:linenr), l:classfullname , l:tempbasename, "g")
            else
                silent echo substitute(getline(l:linenr), l:classname , l:tempbasename, "g")
            endif
        endwhile
        redir END
    " This is not a class file
    else
        " This happens if we don't have lines marked on the editor
        if a:firstline == a:lastline
            execute "silent write " . l:tempfile
        " This happens if we have lines marked on the editor. We will add
        " DEFINE lines if there are some outside of the marking.
        " This is not very good solution but might work some cases
        else
            let l:saveic = &ic
            set ic
            execute "redir > " . l:tempfile
            execute "silent 0," . a:firstline . "g /DEF\\(INE\\)\\? /p"
            silent echo ""
            redir END
            let l:deflines = system("wc -l " . l:tempfile)
            let l:deflines = substitute(l:deflines, " ", "", "g") + 1
            let l:addlines = a:firstline - l:deflines

            execute "silent " . a:firstline . "," . a:lastline . ' w >> ' . l:tempfile

            call histdel("search", -1)
            let @/ = histget("search", -1)
            if l:saveic == 0
                set noic
            endif
        endif
    endif

    let l:pfile = tempname() . ".p"
    execute "redir > " . l:pfile
    silent echo "COMPILE " . l:tempfile . "."
    redir END

    let l:output = system("pike terminalbatch " . l:pfile)
    let dummy = delete(l:tempfile)
    let dummy = delete(l:pfile)

    if l:output == ""
        redraw | echo "Syntax is OK"
        execute "normal " . l:saveline . "G"
    else
        echohl ErrorMsg
        echo  Interpret_output(l:output, l:addlines)
        echohl None
        exe "normal \<Esc>"
    endif
endfunction

nmap <F4> :call Check_syntax()<enter>
vmap <F4> :call Check_syntax()<enter>


"***********************+
" Magic number table
"***********************+

function! Print_magic()
    execute "! pike -C " . g:ccbspath . "/tools/docgen/magicnum <cWORD>"
endfunction

" TDC does not have TmsCodes
" nmap <F3> :call Print_magic()<enter>
