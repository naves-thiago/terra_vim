" Vim indent file
" Language:	Terra
" Maintainer:	Thiago Duarte Naves
" First Author:	Thiago Duarte Naves
" Last Change:	2019 Jun 22

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetTerraIndent()

" To make Vim call GetTerraIndent() when it finds '\s*end' or '\s*until'
" on the current line ('else' is default and includes 'elseif').
setlocal indentkeys+=0=end,0=until,=with

setlocal autoindent

" Only define the function once.
"if exists("*GetTerraIndent")
"  finish
"endif

function! GetTerraIndent()
  " Find a non-blank line above the current line.
  let prevlnum = prevnonblank(v:lnum - 1)

  " Hit the start of the file, use zero indent.
  if prevlnum == 0
    return 0
  endif

  " Add a 'shiftwidth' after lines that start a block:
  " 'par/and', 'par/or', 'do', 'par do'
  let ind = indent(prevlnum)
  let prevline = getline(prevlnum)
  let midx = match(prevline, '^\s*\%(foo\>\|par\/and\>\|par\/or\>\|par\s\+do\>\|do\>\)')
  if midx == -1
    let midx = match(prevline, '\(\<do\>\|then\>\|else\>\|with\>\)')
  endif

  if midx != -1
    " Add 'shiftwidth' if what we found previously is not in a comment and
    " an "end" or "until" is not present on the same line.
    if synIDattr(synID(prevlnum, midx + 1, 1), "name") != "cComment" && prevline !~ '\<end\>\|\<until\>'
      let ind = ind + &shiftwidth
    endif
  endif

  " Subtract a 'shiftwidth' on end, with, else and until
  " This is the part that requires 'indentkeys'.
  let midx = match(getline(v:lnum), '^\s*\%(end\|else\|until\|with\)')
  if midx != -1 && synIDattr(synID(v:lnum, midx + 1, 1), "name") != "cComment"
    let ind = ind - &shiftwidth
  endif

  return ind
endfunction
