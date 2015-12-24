" Vim syntax file
" Language:    Neblen
" Maintainer:  Elben Shira <elbenshira@gmail.com>
" URL:         http://github.com/elben/neblen/resources/vim

if exists("b:current_syntax")
  finish
endif

syn keyword neblenTodo contained TODO FIXME XXX NOTE
syn match neblenComment ";.*$" contains=neblenTodo
syn keyword neblenPrimitiveKeywords fn def let
syn match neblenClosures "\v[\(\)\[\]]"

let b:current_syntax = "neblen"

hi def link neblenTodo                  Todo
hi def link neblenComment               Comment
hi def link neblenPrimitiveKeywords     Keyword
hi def link neblenClosures              Delimiter
