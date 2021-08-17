{ pkgs, config, lib, ... }:

with lib;

let

  dag = (import (import ../sources).nur { inherit pkgs; }).repos.rycee.lib.dag;

in

{

  options.mine.vim.enable = mkEnableOption "vim config";

  config.mine.userConfig = mkIf config.mine.vim.enable {

    home.activation.vimDirs = dag.entryAfter [ "linkGeneration" ] ''
      mkdir -p $HOME/.local/share/vim/{undo,swap}
    '';

    home.packages = [ pkgs.nodePackages.bash-language-server ];

    home.sessionVariables.EDITOR = "vim";

    #nixpkgs.overlays = [(self: super: {
    #  ycmd = super.ycmd.override {
    #    gocode = null;
    #    godef = null;
    #    rustracerd = null;
    #  };
    #})];

    programs.neovim = {
      enable = true;
      vimAlias = true;
      withNodeJs = true;
      plugins = with pkgs.vimPlugins; [
        undotree
        vim-nix
        editorconfig-vim
        coc-nvim
        coc-json
        coc-explorer
        haskell-vim
        gruvbox-community
        vim-gitgutter
      ];
      extraConfig = ''
        set tabstop=2
        set shiftwidth=2

        filetype on
        syntax enable
        filetype plugin indent on
        nnoremap j gj
        nnoremap k gk
        set hidden
        set termguicolors
        set showcmd
        set background=dark

        set mouse=a
        set list listchars=tab:▸▸,trail:·

        colorscheme gruvbox

        set nohlsearch
        set undolevels=10000

        set undofile
        set undodir=$HOME/.local/share/vim/undo
        set directory=$HOME/.local/share/vim/swap
        set nobackup
        set nowritebackup
        set backspace=indent,eol,start
        set clipboard=unnamedplus
        set number
        set colorcolumn=80

        set expandtab

        let mapleader = " "

        let g:netrw_home='$HOME/.cache/vim'

        let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
        let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
        let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
        let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
        let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
        let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
        let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

        " coc.nvim example config vv

        " TextEdit might fail if hidden is not set.
        set hidden

        " Some servers have issues with backup files, see #649.
        set nobackup
        set nowritebackup

        " Give more space for displaying messages.
        set cmdheight=2

        " Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
        " delays and poor user experience.
        set updatetime=300

        " Don't pass messages to |ins-completion-menu|.
        set shortmess+=c

        " Always show the signcolumn, otherwise it would shift the text each time
        " diagnostics appear/become resolved.
        if has("patch-8.1.1564")
          " Recently vim can merge signcolumn and number column into one
          set signcolumn=number
        else
          set signcolumn=yes
        endif

        " Use tab for trigger completion with characters ahead and navigate.
        " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
        " other plugin before putting this into your config.
        inoremap <silent><expr> <TAB>
              \ pumvisible() ? "\<C-n>" :
              \ <SID>check_back_space() ? "\<TAB>" :
              \ coc#refresh()
        inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

        function! s:check_back_space() abort
          let col = col('.') - 1
          return !col || getline('.')[col - 1]  =~# '\s'
        endfunction

        " Use <c-space> to trigger completion.
        if has('nvim')
          inoremap <silent><expr> <c-space> coc#refresh()
        else
          inoremap <silent><expr> <c-@> coc#refresh()
        endif

        " Make <CR> auto-select the first completion item and notify coc.nvim to
        " format on enter, <cr> could be remapped by other vim plugin
        "inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
        "                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

        " Use `[g` and `]g` to navigate diagnostics
        " Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
        nmap <silent> [g <Plug>(coc-diagnostic-prev)
        nmap <silent> ]g <Plug>(coc-diagnostic-next)

        " GoTo code navigation.
        nmap <silent> gd <Plug>(coc-definition)
        nmap <silent> gy <Plug>(coc-type-definition)
        nmap <silent> gi <Plug>(coc-implementation)
        nmap <silent> gr <Plug>(coc-references)

        " Use K to show documentation in preview window.
        nnoremap <silent> K :call <SID>show_documentation()<CR>

        function! s:show_documentation()
          if (index(['vim','help'], &filetype) >= 0)
            execute 'h '.expand('<cword>')
          elseif (coc#rpc#ready())
            call CocActionAsync('doHover')
          else
            execute '!' . &keywordprg . " " . expand('<cword>')
          endif
        endfunction

        " Highlight the symbol and its references when holding the cursor.
        autocmd CursorHold * silent call CocActionAsync('highlight')

        " Symbol renaming.
        nmap <leader>rn <Plug>(coc-rename)

        " Formatting selected code.
        " xmap <leader>f  <Plug>(coc-format-selected)
        " nmap <leader>f  <Plug>(coc-format-selected)

        augroup mygroup
          autocmd!
          " Setup formatexpr specified filetype(s).
          autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
          " Update signature help on jump placeholder.
          autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
        augroup end

        " Applying codeAction to the selected region.
        " Example: `<leader>aap` for current paragraph
        " xmap <leader>a  <Plug>(coc-codeaction-selected)
        " nmap <leader>a  <Plug>(coc-codeaction-selected)

        " Remap keys for applying codeAction to the current buffer.
        " nmap <leader>ac  <Plug>(coc-codeaction)
        " Apply AutoFix to problem on the current line.
        nmap <leader>a  <Plug>(coc-fix-current)

        " Map function and class text objects
        " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
        xmap if <Plug>(coc-funcobj-i)
        omap if <Plug>(coc-funcobj-i)
        xmap af <Plug>(coc-funcobj-a)
        omap af <Plug>(coc-funcobj-a)
        xmap ic <Plug>(coc-classobj-i)
        omap ic <Plug>(coc-classobj-i)
        xmap ac <Plug>(coc-classobj-a)
        omap ac <Plug>(coc-classobj-a)

        " Remap <C-f> and <C-b> for scroll float windows/popups.
        if has('nvim-0.4.0') || has('patch-8.2.0750')
          nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
          nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
          inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
          inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
          vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
          vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
        endif

        " Use CTRL-S for selections ranges.
        " Requires 'textDocument/selectionRange' support of language server.
        nmap <silent> <C-s> <Plug>(coc-range-select)
        xmap <silent> <C-s> <Plug>(coc-range-select)

        " Add `:Format` command to format current buffer.
        command! -nargs=0 Format :call CocAction('format')

        " Add `:Fold` command to fold current buffer.
        command! -nargs=? Fold :call     CocAction('fold', <f-args>)

        " Add `:OR` command for organize imports of the current buffer.
        command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

        " Add (Neo)Vim's native statusline support.
        " NOTE: Please see `:h coc-status` for integrations with external plugins that
        " provide custom statusline: lightline.vim, vim-airline.
        set statusline^=%{coc#status()}%{get(b:,'coc_current_function',''')}

        " Mappings for CoCList
        " Show all diagnostics.
        " nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
        " Manage extensions.
        " nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
        " Show commands.
        " nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
        " Find symbol of current document.
        nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
        " Search workspace symbols.
        " nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
        " Do default action for next item.
        " nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
        " Do default action for previous item.
        " nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
        " Resume latest coc list.
        " nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>


        " https://shapeshed.com/vim-netrw/
        let g:netrw_banner = 0
        let g:netrw_liststyle = 3
        let g:netrw_browse_split = 4
        let g:netrw_winsize = 25

        " https://github.com/weirongxu/coc-explorer#usage
        nmap <space>f :CocCommand explorer<CR>

        nmap <space>u :UndotreeToggle<CR>:UndotreeFocus<CR>

        nmap <space>l <Plug>(coc-codelens-action)

        nnoremap <C-h> <C-w>h
        nnoremap <C-j> <C-w>j
        nnoremap <C-k> <C-w>k
        nnoremap <C-l> <C-w>l
      '';

      #settings = {
      #  tabstop = 2;
      #  shiftwidth = 2;
      #};
      #plugins = with pkgs.vimPlugins; [
      #  vim-nix
      #  syntastic
      #  editorconfig-vim
      #];
    };
  };

}
