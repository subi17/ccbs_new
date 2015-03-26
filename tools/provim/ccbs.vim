set shiftwidth=3
set expandtab
set ignorecase
set foldmethod=syntax
normal zR

execute "set path+=" . ccbspath . "/tms"
set suffixes+=.r


"*******************************+
" Table description with Shift-K
"*******************************+
execute "set keywordprg=make\\\ -C\\\ " . ccbspath . "/tools/docgen/tbldesc"

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

set makeprg=make\ %<.r

nmap <F2> :make<enter><enter>

"***********************+
" Magic number table
"***********************+

function! Print_magic()
    execute "! make -C " . g:ccbspath . "/tools/docgen/magicnum <cWORD>"
endfunction

nmap <F4> :call Print_magic()<enter>
