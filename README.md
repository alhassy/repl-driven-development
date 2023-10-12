<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<!-- 2023-10-12 Thu 05:40 -->
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>Editor Integrated REPLs for all languages</title>
<meta name="author" content="Musa Al-hassy" />
<meta name="generator" content="Org Mode" />
<style>
  #content { max-width: 60em; margin: auto; }
  .title  { text-align: center;
             margin-bottom: .2em; }
  .subtitle { text-align: center;
              font-size: medium;
              font-weight: bold;
              margin-top:0; }
  .todo   { font-family: monospace; color: red; }
  .done   { font-family: monospace; color: green; }
  .priority { font-family: monospace; color: orange; }
  .tag    { background-color: #eee; font-family: monospace;
            padding: 2px; font-size: 80%; font-weight: normal; }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .org-right  { margin-left: auto; margin-right: 0px;  text-align: right; }
  .org-left   { margin-left: 0px;  margin-right: auto; text-align: left; }
  .org-center { margin-left: auto; margin-right: auto; text-align: center; }
  .underline { text-decoration: underline; }
  #postamble p, #preamble p { font-size: 90%; margin: .2em; }
  p.verse { margin-left: 3%; }
  pre {
    border: 1px solid #e6e6e6;
    border-radius: 3px;
    background-color: #f2f2f2;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: auto;
  }
  pre.src:before {
    display: none;
    position: absolute;
    top: -8px;
    right: 12px;
    padding: 3px;
    color: #555;
    background-color: #f2f2f299;
  }
  pre.src:hover:before { display: inline; margin-top: 14px;}
  /* Languages per Org manual */
  pre.src-asymptote:before { content: 'Asymptote'; }
  pre.src-awk:before { content: 'Awk'; }
  pre.src-authinfo::before { content: 'Authinfo'; }
  pre.src-C:before { content: 'C'; }
  /* pre.src-C++ doesn't work in CSS */
  pre.src-clojure:before { content: 'Clojure'; }
  pre.src-css:before { content: 'CSS'; }
  pre.src-D:before { content: 'D'; }
  pre.src-ditaa:before { content: 'ditaa'; }
  pre.src-dot:before { content: 'Graphviz'; }
  pre.src-calc:before { content: 'Emacs Calc'; }
  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
  pre.src-fortran:before { content: 'Fortran'; }
  pre.src-gnuplot:before { content: 'gnuplot'; }
  pre.src-haskell:before { content: 'Haskell'; }
  pre.src-hledger:before { content: 'hledger'; }
  pre.src-java:before { content: 'Java'; }
  pre.src-js:before { content: 'Javascript'; }
  pre.src-latex:before { content: 'LaTeX'; }
  pre.src-ledger:before { content: 'Ledger'; }
  pre.src-lisp:before { content: 'Lisp'; }
  pre.src-lilypond:before { content: 'Lilypond'; }
  pre.src-lua:before { content: 'Lua'; }
  pre.src-matlab:before { content: 'MATLAB'; }
  pre.src-mscgen:before { content: 'Mscgen'; }
  pre.src-ocaml:before { content: 'Objective Caml'; }
  pre.src-octave:before { content: 'Octave'; }
  pre.src-org:before { content: 'Org mode'; }
  pre.src-oz:before { content: 'OZ'; }
  pre.src-plantuml:before { content: 'Plantuml'; }
  pre.src-processing:before { content: 'Processing.js'; }
  pre.src-python:before { content: 'Python'; }
  pre.src-R:before { content: 'R'; }
  pre.src-ruby:before { content: 'Ruby'; }
  pre.src-sass:before { content: 'Sass'; }
  pre.src-scheme:before { content: 'Scheme'; }
  pre.src-screen:before { content: 'Gnu Screen'; }
  pre.src-sed:before { content: 'Sed'; }
  pre.src-sh:before { content: 'shell'; }
  pre.src-sql:before { content: 'SQL'; }
  pre.src-sqlite:before { content: 'SQLite'; }
  /* additional languages in org.el's org-babel-load-languages alist */
  pre.src-forth:before { content: 'Forth'; }
  pre.src-io:before { content: 'IO'; }
  pre.src-J:before { content: 'J'; }
  pre.src-makefile:before { content: 'Makefile'; }
  pre.src-maxima:before { content: 'Maxima'; }
  pre.src-perl:before { content: 'Perl'; }
  pre.src-picolisp:before { content: 'Pico Lisp'; }
  pre.src-scala:before { content: 'Scala'; }
  pre.src-shell:before { content: 'Shell Script'; }
  pre.src-ebnf2ps:before { content: 'ebfn2ps'; }
  /* additional language identifiers per "defun org-babel-execute"
       in ob-*.el */
  pre.src-cpp:before  { content: 'C++'; }
  pre.src-abc:before  { content: 'ABC'; }
  pre.src-coq:before  { content: 'Coq'; }
  pre.src-groovy:before  { content: 'Groovy'; }
  /* additional language identifiers from org-babel-shell-names in
     ob-shell.el: ob-shell is the only babel language using a lambda to put
     the execution function name together. */
  pre.src-bash:before  { content: 'bash'; }
  pre.src-csh:before  { content: 'csh'; }
  pre.src-ash:before  { content: 'ash'; }
  pre.src-dash:before  { content: 'dash'; }
  pre.src-ksh:before  { content: 'ksh'; }
  pre.src-mksh:before  { content: 'mksh'; }
  pre.src-posh:before  { content: 'posh'; }
  /* Additional Emacs modes also supported by the LaTeX listings package */
  pre.src-ada:before { content: 'Ada'; }
  pre.src-asm:before { content: 'Assembler'; }
  pre.src-caml:before { content: 'Caml'; }
  pre.src-delphi:before { content: 'Delphi'; }
  pre.src-html:before { content: 'HTML'; }
  pre.src-idl:before { content: 'IDL'; }
  pre.src-mercury:before { content: 'Mercury'; }
  pre.src-metapost:before { content: 'MetaPost'; }
  pre.src-modula-2:before { content: 'Modula-2'; }
  pre.src-pascal:before { content: 'Pascal'; }
  pre.src-ps:before { content: 'PostScript'; }
  pre.src-prolog:before { content: 'Prolog'; }
  pre.src-simula:before { content: 'Simula'; }
  pre.src-tcl:before { content: 'tcl'; }
  pre.src-tex:before { content: 'TeX'; }
  pre.src-plain-tex:before { content: 'Plain TeX'; }
  pre.src-verilog:before { content: 'Verilog'; }
  pre.src-vhdl:before { content: 'VHDL'; }
  pre.src-xml:before { content: 'XML'; }
  pre.src-nxml:before { content: 'XML'; }
  /* add a generic configuration mode; LaTeX export needs an additional
     (add-to-list 'org-latex-listings-langs '(conf " ")) in .emacs */
  pre.src-conf:before { content: 'Configuration File'; }

  table { border-collapse:collapse; }
  caption.t-above { caption-side: top; }
  caption.t-bottom { caption-side: bottom; }
  td, th { vertical-align:top;  }
  th.org-right  { text-align: center;  }
  th.org-left   { text-align: center;   }
  th.org-center { text-align: center; }
  td.org-right  { text-align: right;  }
  td.org-left   { text-align: left;   }
  td.org-center { text-align: center; }
  dt { font-weight: bold; }
  .footpara { display: inline; }
  .footdef  { margin-bottom: 1em; }
  .figure { padding: 1em; }
  .figure p { text-align: center; }
  .equation-container {
    display: table;
    text-align: center;
    width: 100%;
  }
  .equation {
    vertical-align: middle;
  }
  .equation-label {
    display: table-cell;
    text-align: right;
    vertical-align: middle;
  }
  .inlinetask {
    padding: 10px;
    border: 2px solid gray;
    margin: 10px;
    background: #ffffcc;
  }
  #org-div-home-and-up
   { text-align: right; font-size: 70%; white-space: nowrap; }
  textarea { overflow-x: auto; }
  .linenr { font-size: smaller }
  .code-highlighted { background-color: #ffff00; }
  .org-info-js_info-navigation { border-style: none; }
  #org-info-js_console-label
    { font-size: 10px; font-weight: bold; white-space: nowrap; }
  .org-info-js_search-highlight
    { background-color: #ffff00; color: #000000; font-weight: bold; }
  .org-svg { }
</style>
<style>  summary:hover {background:pink;} </style>
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
          
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;
        
            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/tooltipster.bundle.min.css"/>
        
          <link rel="stylesheet" type="text/css" href="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/css/plugins/tooltipster/sideTip/themes/tooltipster-sideTip-punk.min.css" />
        
          <script type="text/javascript">
              if (typeof jQuery == 'undefined') {
                  document.write(unescape('%3Cscript src="https://code.jquery.com/jquery-1.10.0.min.js"%3E%3C/script%3E'));
              }
          </script>
        
           <script type="text/javascript"            src="https://alhassy.github.io/org-special-block-extras/tooltipster/dist/js/tooltipster.bundle.min.js"></script>
        
            <script>
                   $(document).ready(function() {
                       $('.tooltip').tooltipster({
                           theme: 'tooltipster-punk',
                           contentAsHTML: true,
                           animation: 'grow',
                           delay: [100,500],
                           // trigger: 'click'
                           trigger: 'custom',
                           triggerOpen: {
                               mouseenter: true
                           },
                           triggerClose: {
                               originClick: true,
                               scroll: true
                           }
           });
                   });
               </script>
        
          <style>
             abbr {color: red;}
        
             .tooltip { border-bottom: 1px dotted #000;
                        color:red;
                        text-decoration: none;}
          </style>
</head>
<body>
<div id="content" class="content">
<h1 class="title">Editor Integrated REPLs for all languages</h1>

<div align="center">

<h1>  Editor Integrated REPLs for all languages </h1>

<p>
<a href="https://melpa.org/#/repl-driven-development"><img alt="MELPA" src="https://img.shields.io/badge/repl--driven--development-1.0.8-green?logo=Gnu-Emacs"></img></a>
</p>

<p>
<a href="https://twitter.com/intent/tweet?text=This looks super neat (•̀ᴗ•́)و::&url=https://github.com/alhassy/repl-driven-development"><img src="https://img.shields.io/twitter/url?url=https://github.com/alhassy/repl-driven-development"></a>
<a href="https://github.com/alhassy/repl-driven-development/issues"><img src="https://img.shields.io/badge/contributions-welcome-green?logo=nil"></a>
</p>

<p>
<a href="https://alhassy.com/"><img src="https://img.shields.io/badge/author-musa_al--hassy-purple?logo=nintendo-3ds"></a>
<a href="https://www.buymeacoffee.com/alhassy"><img src="https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee"></a>
</p>

<p>
This library provides the Emacs built-in <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-e ∷ eval-last-sexp<br>Evaluate sexp before point; print value in the echo area.<br>Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument.<br>With a non ‘-’ prefix argument, print output into current buffer.<br><br>This commands handles ‘defvar’, ‘defcustom’ and ‘defface’ the<br>same way that ‘eval-defun’ does.&emsp;See the doc string of that<br>function for details.<br><br>Normally, this function truncates long output according to the<br>value of the variables ‘eval-expression-print-length’ and<br>‘eval-expression-print-level’.&emsp;With a prefix argument of zero,<br>however, there is no such truncation.<br>Integer values are printed in several formats (decimal, octal,<br>and hexadecimal).&emsp;When the prefix argument is -1 or the value<br>doesn’t exceed ‘eval-expression-print-maximum-character’, an<br>integer value is also printed as a character of that codepoint.<br><br>If ‘eval-expression-debug-on-error’ is non-nil, which is the default,<br>this command arranges for all errors to enter the debugger.<br><br>This function has :around advice: ‘ad-Advice-eval-last-sexp’.<br><br>(fn EVAL-LAST-SEXP-ARG-INTERNAL)"><kbd style="border-color: red">C-x C-e</kbd></abbr> behaviour for
arbitrary languages, provided they have a REPL shell command.
</p>
<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><b>It provides a “send line to REPL process” command, for your language.</b></td>
</tr>
</tbody>
</table>
</div>

<div align="center">
<img src="http://alhassy.com/images/rdd-workflow.png" width=400 height=300 />

<div id="orgee41048" class="figure">
<p><img src="rdd.gif" alt="rdd.gif" width="400px" />
</p>
</div>
</div>

<div id="table-of-contents" role="doc-toc">
<h2>Table of Contents</h2>
<div id="text-table-of-contents" role="doc-toc">
<ul>
<li><a href="#motivation">1. <a href="#motivation">Motivation</a></a></li>
<li><a href="#official-manual">2. <a href="#official-manual">Official Manual</a></a></li>
<li><a href="#mini-tutorial">3. <a href="#mini-tutorial">Mini-Tutorial</a></a></li>
<li><a href="#videos">4. <a href="#videos">Videos</a></a>
<ul>
<li><a href="#teaching-a-javascript-runtime-incrementally-to-be-a-web-server-">4.1. <a href="#teaching-a-javascript-runtime-incrementally-to-be-a-web-server-">REPL Driven Development :: Teaching a JavaScript runtime, incrementally, to be a web server 🍽️ 🔁 🤖</a></a></li>
</ul>
</li>
</ul>
</div>
</div>

<div id="outline-container-motivation" class="outline-2">
<h2 id="motivation"><span class="section-number-2">1.</span> <a href="#motivation">Motivation</a></h2>
<div class="outline-text-2" id="text-motivation">
<p>
Whenever reading/refactoring some code, if you can make some of it
self-contained, then you can immediately try it out! No need to
load your entire program; nor copy-paste into an external REPL.  The
benefits of Emacs' built-in <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-e ∷ eval-last-sexp<br>Evaluate sexp before point; print value in the echo area.<br>Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument.<br>With a non ‘-’ prefix argument, print output into current buffer.<br><br>This commands handles ‘defvar’, ‘defcustom’ and ‘defface’ the<br>same way that ‘eval-defun’ does.&emsp;See the doc string of that<br>function for details.<br><br>Normally, this function truncates long output according to the<br>value of the variables ‘eval-expression-print-length’ and<br>‘eval-expression-print-level’.&emsp;With a prefix argument of zero,<br>however, there is no such truncation.<br>Integer values are printed in several formats (decimal, octal,<br>and hexadecimal).&emsp;When the prefix argument is -1 or the value<br>doesn’t exceed ‘eval-expression-print-maximum-character’, an<br>integer value is also printed as a character of that codepoint.<br><br>If ‘eval-expression-debug-on-error’ is non-nil, which is the default,<br>this command arranges for all errors to enter the debugger.<br><br>This function has :around advice: ‘ad-Advice-eval-last-sexp’.<br><br>(fn EVAL-LAST-SEXP-ARG-INTERNAL)"><kbd style="border-color: red">C-x C-e</kbd></abbr> for Lisp, and Lisp's Repl
Driven Development philosophy, are essentially made possible for
arbitrary languages (to some approximate degree, but not fully).
</p>

<div align="center">
<img src="http://alhassy.com/images/rdd-benefits.png" width=250 height=250 />
</div>

<p>
Just as <kbd style="">C-u C-x C-e</kbd> inserts the resulting expression at the
current cursour position, so too all <code>repl-driven-development</code>
commands allow for a <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-u ∷ universal-argument<br>Begin a numeric argument for the following command.<br>Digits or minus sign following C-u make up the numeric argument.<br>C-u following the digits or minus sign ends the argument.<br>C-u without digits or minus sign provides 4 as argument.<br>Repeating C-u without digits or minus sign<br> multiplies the argument by 4 each time.<br>For some commands, just C-u by itself serves as a flag<br>that is different in effect from any particular numeric argument.<br>These commands include C-SPC and M-x start-kbd-macro."><kbd style="border-color: red">C-u</kbd></abbr> prefix which inserts the result.
This allows for a nice scripting experience where results
are kept for future use.
</p>

<p>
Finally, just as <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-h e ∷ view-echo-area-messages<br>View the log of recent echo-area messages: the ‘<strong>Messages</strong>’ buffer.<br>The number of messages retained in that buffer is specified by<br>the variable ‘message-log-max’."><kbd style="border-color: red">C-h e</kbd></abbr> shows you the <code>*Messages*</code> buffer
where you can see the evaluations of your Emacs Lisp via
<abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-e ∷ eval-last-sexp<br>Evaluate sexp before point; print value in the echo area.<br>Interactively, EVAL-LAST-SEXP-ARG-INTERNAL is the prefix argument.<br>With a non ‘-’ prefix argument, print output into current buffer.<br><br>This commands handles ‘defvar’, ‘defcustom’ and ‘defface’ the<br>same way that ‘eval-defun’ does.&emsp;See the doc string of that<br>function for details.<br><br>Normally, this function truncates long output according to the<br>value of the variables ‘eval-expression-print-length’ and<br>‘eval-expression-print-level’.&emsp;With a prefix argument of zero,<br>however, there is no such truncation.<br>Integer values are printed in several formats (decimal, octal,<br>and hexadecimal).&emsp;When the prefix argument is -1 or the value<br>doesn’t exceed ‘eval-expression-print-maximum-character’, an<br>integer value is also printed as a character of that codepoint.<br><br>If ‘eval-expression-debug-on-error’ is non-nil, which is the default,<br>this command arranges for all errors to enter the debugger.<br><br>This function has :around advice: ‘ad-Advice-eval-last-sexp’.<br><br>(fn EVAL-LAST-SEXP-ARG-INTERNAL)"><kbd style="border-color: red">C-x C-e</kbd></abbr>; likewise, <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-h e ∷ view-echo-area-messages<br>View the log of recent echo-area messages: the ‘<strong>Messages</strong>’ buffer.<br>The number of messages retained in that buffer is specified by<br>the variable ‘message-log-max’."><kbd style="border-color: red">C-h e</kbd></abbr> shows you the output results
of any REPL command created by  <code>repl-driven-development</code>.
</p>
</div>
</div>

<div id="outline-container-official-manual" class="outline-2">
<h2 id="official-manual"><span class="section-number-2">2.</span> <a href="#official-manual">Official Manual</a></h2>
<div class="outline-text-2" id="text-official-manual">
<p>
See <a href="http://alhassy.com/repl-driven-development">http://alhassy.com/repl-driven-development</a>
</p>

<p>
<kbd style="">C-h o repl-driven-development</kbd> also has extensive docs,
via a JavaScript server example.
</p>
</div>
</div>

<div id="outline-container-mini-tutorial" class="outline-2">
<h2 id="mini-tutorial"><span class="section-number-2">3.</span> <a href="#mini-tutorial">Mini-Tutorial</a></h2>
<div class="outline-text-2" id="text-mini-tutorial">
<p>
Often, while reading a README file, we will (1) copy a shell command, (2) open a
terminal, and (3) paste the shell command to run it.  We can evaluate arbitrary
regions in a shell in one step via <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-t ∷ bash-repl<br>Executes the selected region, if any or otherwise the entire current line,<br>and evaluates it with the command-line tool “bash-repl”.<br><br>Output is shown as an overlay at the current cursor position.<br>It is shown for ‘repl-driven-development-echo-duration’ many seconds.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ Familiar Workflow ﴿<br><br>You can execute arbitrary Lisp anywhere by pressing “C-x C-e”, you can<br>insert the result with “C-u C-x C-e”, and see the output echoed in the<br>mode-line and in the <strong>Messages</strong> buffer with “C-h e”.<br>Likewise, you can execute “bash” code by pressing “C-x C-t”, insert output<br>with “C-u C-x C-t”, and see the output echoed near your cursor and in the<br><strong>Messages</strong> buffer with “C-h e”.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u C-x C-t&emsp;≈&emsp;Insert result ﴿<br><br>With a “C-u” prefix, the output is inserted at point<br>(and not echoed in an overlay).<br><br>Since bash-repl may pretty-print its output, inserting it may result in<br>non-executable code. If you want executable code, you must specify<br>how pretty-printed output must be converted into bash-repl-executable code.<br>Do so by redefining ‘bash-repl-read’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u 0 C-x C-t&emsp; ≈&emsp;‘bash-repl-display-output’ ﴿<br><br>Sometimes it may be useful to look at a large output in a dedicated buffer.<br>However, the output of a command is also attached to the input via a<br>tooltip: Hover to see it! See also ‘tooltip-delay’.<br>Moreover, “C-h e” shows you the output in the <strong>Messages</strong> buffer.<br>See also ‘repl-driven-development-echo-output-in-modeline’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;﴾ C-u C-u C-x C-t&emsp;≈&emsp;‘bash-repl-docs-at-point’ ﴿<br><br>With a “C-u C-u” prefix, documentation is looked-up for the word at point.<br><br>This is done using ‘devdocs’, and so the documentation generally provides<br>example uses as well. Visit https://devdocs.io/ to see the list of documented<br>languages and libraries.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u -1 C-x C-t&emsp;≈&emsp;‘bash-repl-restart’ ﴿<br><br>In the event you’ve messed-up your REPL, starting from a blank slate may be<br>helpful.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ Implementation Notes ﴿<br><br>The interactive method is asynchronous: Whenever you send text for<br>evaluation, you immediately regain control in Emacs; you may send more text<br>and it will be queued for evaluation. For example, evaluating a sleep<br>command for 3 seconds does not block Emacs.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>From Lisp, consider using ‘bash-repl-submit’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ See also ﴿<br><br>See ‘repl-driven-development’ for more useful docs.<br><br>See URL ‘http://www.alhassy.com/repl-driven-development’ to learn more about<br>RDD see examples and many gifs."><kbd style="border-color: red">C-x C-t</kbd></abbr> with:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">   (<span style="color: #3a81c3; font-weight: bold;">repl-driven-development</span> [C-x C-t] <span style="color: #2d9574;">"bash"</span>)
</pre>
</div>


<p>
For example, execute <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-t ∷ bash-repl<br>Executes the selected region, if any or otherwise the entire current line,<br>and evaluates it with the command-line tool “bash-repl”.<br><br>Output is shown as an overlay at the current cursor position.<br>It is shown for ‘repl-driven-development-echo-duration’ many seconds.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ Familiar Workflow ﴿<br><br>You can execute arbitrary Lisp anywhere by pressing “C-x C-e”, you can<br>insert the result with “C-u C-x C-e”, and see the output echoed in the<br>mode-line and in the <strong>Messages</strong> buffer with “C-h e”.<br>Likewise, you can execute “bash” code by pressing “C-x C-t”, insert output<br>with “C-u C-x C-t”, and see the output echoed near your cursor and in the<br><strong>Messages</strong> buffer with “C-h e”.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u C-x C-t&emsp;≈&emsp;Insert result ﴿<br><br>With a “C-u” prefix, the output is inserted at point<br>(and not echoed in an overlay).<br><br>Since bash-repl may pretty-print its output, inserting it may result in<br>non-executable code. If you want executable code, you must specify<br>how pretty-printed output must be converted into bash-repl-executable code.<br>Do so by redefining ‘bash-repl-read’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u 0 C-x C-t&emsp; ≈&emsp;‘bash-repl-display-output’ ﴿<br><br>Sometimes it may be useful to look at a large output in a dedicated buffer.<br>However, the output of a command is also attached to the input via a<br>tooltip: Hover to see it! See also ‘tooltip-delay’.<br>Moreover, “C-h e” shows you the output in the <strong>Messages</strong> buffer.<br>See also ‘repl-driven-development-echo-output-in-modeline’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;﴾ C-u C-u C-x C-t&emsp;≈&emsp;‘bash-repl-docs-at-point’ ﴿<br><br>With a “C-u C-u” prefix, documentation is looked-up for the word at point.<br><br>This is done using ‘devdocs’, and so the documentation generally provides<br>example uses as well. Visit https://devdocs.io/ to see the list of documented<br>languages and libraries.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u -1 C-x C-t&emsp;≈&emsp;‘bash-repl-restart’ ﴿<br><br>In the event you’ve messed-up your REPL, starting from a blank slate may be<br>helpful.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ Implementation Notes ﴿<br><br>The interactive method is asynchronous: Whenever you send text for<br>evaluation, you immediately regain control in Emacs; you may send more text<br>and it will be queued for evaluation. For example, evaluating a sleep<br>command for 3 seconds does not block Emacs.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>From Lisp, consider using ‘bash-repl-submit’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ See also ﴿<br><br>See ‘repl-driven-development’ for more useful docs.<br><br>See URL ‘http://www.alhassy.com/repl-driven-development’ to learn more about<br>RDD see examples and many gifs."><kbd style="border-color: red">C-x C-t</kbd></abbr> anywhere on each line below and see results in an
overlay, right by your cursor.
</p>

<div class="org-src-container">
<pre class="src src-shell">  <span style="color: #3a81c3; font-weight: bold;">echo</span> <span style="color: #2d9574;">"It is $(date) and I am at $PWD, my name is $(whoami) and I have: $(ls)"</span>

  say <span style="color: #2d9574;">"My name is $(whoami) and I like Emacs"</span>
</pre>
</div>

<p>
Notice as each line is sent to the Bash process, the line is highlighted briefly
in yellow.  Moreover, you can hover over the text to see a tooltip with the
resulting shell output.  Finally, if you invoke <kbd style="">C-h k C-x C-t</kbd> you get help
about this new <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-t ∷ bash-repl<br>Executes the selected region, if any or otherwise the entire current line,<br>and evaluates it with the command-line tool “bash-repl”.<br><br>Output is shown as an overlay at the current cursor position.<br>It is shown for ‘repl-driven-development-echo-duration’ many seconds.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ Familiar Workflow ﴿<br><br>You can execute arbitrary Lisp anywhere by pressing “C-x C-e”, you can<br>insert the result with “C-u C-x C-e”, and see the output echoed in the<br>mode-line and in the <strong>Messages</strong> buffer with “C-h e”.<br>Likewise, you can execute “bash” code by pressing “C-x C-t”, insert output<br>with “C-u C-x C-t”, and see the output echoed near your cursor and in the<br><strong>Messages</strong> buffer with “C-h e”.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u C-x C-t&emsp;≈&emsp;Insert result ﴿<br><br>With a “C-u” prefix, the output is inserted at point<br>(and not echoed in an overlay).<br><br>Since bash-repl may pretty-print its output, inserting it may result in<br>non-executable code. If you want executable code, you must specify<br>how pretty-printed output must be converted into bash-repl-executable code.<br>Do so by redefining ‘bash-repl-read’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u 0 C-x C-t&emsp; ≈&emsp;‘bash-repl-display-output’ ﴿<br><br>Sometimes it may be useful to look at a large output in a dedicated buffer.<br>However, the output of a command is also attached to the input via a<br>tooltip: Hover to see it! See also ‘tooltip-delay’.<br>Moreover, “C-h e” shows you the output in the <strong>Messages</strong> buffer.<br>See also ‘repl-driven-development-echo-output-in-modeline’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;﴾ C-u C-u C-x C-t&emsp;≈&emsp;‘bash-repl-docs-at-point’ ﴿<br><br>With a “C-u C-u” prefix, documentation is looked-up for the word at point.<br><br>This is done using ‘devdocs’, and so the documentation generally provides<br>example uses as well. Visit https://devdocs.io/ to see the list of documented<br>languages and libraries.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u -1 C-x C-t&emsp;≈&emsp;‘bash-repl-restart’ ﴿<br><br>In the event you’ve messed-up your REPL, starting from a blank slate may be<br>helpful.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ Implementation Notes ﴿<br><br>The interactive method is asynchronous: Whenever you send text for<br>evaluation, you immediately regain control in Emacs; you may send more text<br>and it will be queued for evaluation. For example, evaluating a sleep<br>command for 3 seconds does not block Emacs.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>From Lisp, consider using ‘bash-repl-submit’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ See also ﴿<br><br>See ‘repl-driven-development’ for more useful docs.<br><br>See URL ‘http://www.alhassy.com/repl-driven-development’ to learn more about<br>RDD see examples and many gifs."><kbd style="border-color: red">C-x C-t</kbd></abbr> command, such as inserting results at point via
<kbd style="">C-u C-x C-t</kbd> or to reset/refresh the current Bash process with <kbd style="">C-u -1 C-x C-t</kbd>.
</p>

<p>
This also works for any command-line REPL; for example, for Python:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">   (<span style="color: #3a81c3; font-weight: bold;">repl-driven-development</span> [C-x C-p] <span style="color: #2d9574;">"python3"</span>)
</pre>
</div>

<p>
Then, we can submit the following Python snippets with <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-p ∷ python-repl<br>Executes the selected region, if any or otherwise the entire current line,<br>and evaluates it with the command-line tool “python-repl”.<br><br>Output is shown as an overlay at the current cursor position.<br>It is shown for ‘repl-driven-development-echo-duration’ many seconds.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ Familiar Workflow ﴿<br><br>You can execute arbitrary Lisp anywhere by pressing “C-x C-e”, you can<br>insert the result with “C-u C-x C-e”, and see the output echoed in the<br>mode-line and in the <strong>Messages</strong> buffer with “C-h e”.<br>Likewise, you can execute “python3” code by pressing “C-x C-p”, insert output<br>with “C-u C-x C-p”, and see the output echoed near your cursor and in the<br><strong>Messages</strong> buffer with “C-h e”.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u C-x C-p&emsp;≈&emsp;Insert result ﴿<br><br>With a “C-u” prefix, the output is inserted at point<br>(and not echoed in an overlay).<br><br>Since python-repl may pretty-print its output, inserting it may result in<br>non-executable code. If you want executable code, you must specify<br>how pretty-printed output must be converted into python-repl-executable code.<br>Do so by redefining ‘python-repl-read’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u 0 C-x C-p&emsp; ≈&emsp;‘python-repl-display-output’ ﴿<br><br>Sometimes it may be useful to look at a large output in a dedicated buffer.<br>However, the output of a command is also attached to the input via a<br>tooltip: Hover to see it! See also ‘tooltip-delay’.<br>Moreover, “C-h e” shows you the output in the <strong>Messages</strong> buffer.<br>See also ‘repl-driven-development-echo-output-in-modeline’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;﴾ C-u C-u C-x C-p&emsp;≈&emsp;‘python-repl-docs-at-point’ ﴿<br><br>With a “C-u C-u” prefix, documentation is looked-up for the word at point.<br><br>This is done using ‘devdocs’, and so the documentation generally provides<br>example uses as well. Visit https://devdocs.io/ to see the list of documented<br>languages and libraries.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u -1 C-x C-p&emsp;≈&emsp;‘python-repl-restart’ ﴿<br><br>In the event you’ve messed-up your REPL, starting from a blank slate may be<br>helpful.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ Implementation Notes ﴿<br><br>The interactive method is asynchronous: Whenever you send text for<br>evaluation, you immediately regain control in Emacs; you may send more text<br>and it will be queued for evaluation. For example, evaluating a sleep<br>command for 3 seconds does not block Emacs.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>From Lisp, consider using ‘python-repl-submit’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ See also ﴿<br><br>See ‘repl-driven-development’ for more useful docs.<br><br>See URL ‘http://www.alhassy.com/repl-driven-development’ to learn more about<br>RDD see examples and many gifs."><kbd style="border-color: red">C-x C-p</kbd></abbr> on each line.
</p>

<div class="org-src-container">
<pre class="src src-python">  <span style="color: #3a81c3; font-weight: bold;">sum</span>([1, 2, 3, 4])

  <span style="color: #3a81c3; font-weight: bold;">list</span>(<span style="color: #3a81c3; font-weight: bold;">map</span>(<span style="color: #3a81c3; font-weight: bold;">lambda</span> i: <span style="color: #2d9574;">'Fizz'</span>*(<span style="color: #3a81c3; font-weight: bold;">not</span> i%3)+<span style="color: #2d9574;">'Buzz'</span>*(<span style="color: #3a81c3; font-weight: bold;">not</span> i%5) <span style="color: #3a81c3; font-weight: bold;">or</span> i, <span style="color: #3a81c3; font-weight: bold;">range</span>(1,101)))
</pre>
</div>

<p>
These work fine, however there are some shortcomings of this REPL.
For example, echoing results could be prettier and it doesn't handle
multi-line input very well.  You can address these issues using the various
hooks / keyword arguments of the <code>repl-driven-development</code> macro.
</p>

<p>
However, this package comes with preconfigured REPLS for: <code>python, terminal, java, javascript</code>.
</p>

<p>
Simply use the name of these configurations:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">  (<span style="color: #3a81c3; font-weight: bold;">repl-driven-development</span> [C-x C-p] python)
</pre>
</div>

<p>
Now we can submit the following, with <abbr class="tooltip" style="border: none; text-decoration: none;" title="C-x C-p ∷ python-repl<br>Executes the selected region, if any or otherwise the entire current line,<br>and evaluates it with the command-line tool “python-repl”.<br><br>Output is shown as an overlay at the current cursor position.<br>It is shown for ‘repl-driven-development-echo-duration’ many seconds.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ Familiar Workflow ﴿<br><br>You can execute arbitrary Lisp anywhere by pressing “C-x C-e”, you can<br>insert the result with “C-u C-x C-e”, and see the output echoed in the<br>mode-line and in the <strong>Messages</strong> buffer with “C-h e”.<br>Likewise, you can execute “python3” code by pressing “C-x C-p”, insert output<br>with “C-u C-x C-p”, and see the output echoed near your cursor and in the<br><strong>Messages</strong> buffer with “C-h e”.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u C-x C-p&emsp;≈&emsp;Insert result ﴿<br><br>With a “C-u” prefix, the output is inserted at point<br>(and not echoed in an overlay).<br><br>Since python-repl may pretty-print its output, inserting it may result in<br>non-executable code. If you want executable code, you must specify<br>how pretty-printed output must be converted into python-repl-executable code.<br>Do so by redefining ‘python-repl-read’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u 0 C-x C-p&emsp; ≈&emsp;‘python-repl-display-output’ ﴿<br><br>Sometimes it may be useful to look at a large output in a dedicated buffer.<br>However, the output of a command is also attached to the input via a<br>tooltip: Hover to see it! See also ‘tooltip-delay’.<br>Moreover, “C-h e” shows you the output in the <strong>Messages</strong> buffer.<br>See also ‘repl-driven-development-echo-output-in-modeline’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;﴾ C-u C-u C-x C-p&emsp;≈&emsp;‘python-repl-docs-at-point’ ﴿<br><br>With a “C-u C-u” prefix, documentation is looked-up for the word at point.<br><br>This is done using ‘devdocs’, and so the documentation generally provides<br>example uses as well. Visit https://devdocs.io/ to see the list of documented<br>languages and libraries.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp; ﴾ C-u -1 C-x C-p&emsp;≈&emsp;‘python-repl-restart’ ﴿<br><br>In the event you’ve messed-up your REPL, starting from a blank slate may be<br>helpful.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ Implementation Notes ﴿<br><br>The interactive method is asynchronous: Whenever you send text for<br>evaluation, you immediately regain control in Emacs; you may send more text<br>and it will be queued for evaluation. For example, evaluating a sleep<br>command for 3 seconds does not block Emacs.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>From Lisp, consider using ‘python-repl-submit’.<br><br>⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾<br>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;﴾ See also ﴿<br><br>See ‘repl-driven-development’ for more useful docs.<br><br>See URL ‘http://www.alhassy.com/repl-driven-development’ to learn more about<br>RDD see examples and many gifs."><kbd style="border-color: red">C-x C-p</kbd></abbr>, with no issues:
</p>

<div class="org-src-container">
<pre class="src src-python">  <span style="color: #3a81c3; font-weight: bold;">def</span> <span style="color: #6c3163; font-weight: bold;">square</span>(x):
    <span style="color: #3a81c3; font-weight: bold;">return</span> x * x

  square(5)
</pre>
</div>

<p>
Since these new REPL commands are just Emacs functions, we can use
several at the time, alternating between them.  For example:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">  <span style="color: #a89984;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x C-e on the next two lines</span>
  (<span style="color: #3a81c3; font-weight: bold;">repl-driven-development</span> [C-x C-t] terminal)
  (<span style="color: #3a81c3; font-weight: bold;">repl-driven-development</span> [C-x C-p] python)
</pre>
</div>
<div class="org-src-container">
<pre class="src src-shell">  <span style="color: #3a81c3; font-weight: bold;">echo</span> Hello... &gt; /tmp/o       <span style="color: #a89984;"># </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x C-t here</span>
</pre>
</div>
<div class="org-src-container">
<pre class="src src-python">  <span style="color: #3a81c3; font-weight: bold;">print</span>(<span style="color: #3a81c3; font-weight: bold;">open</span>(<span style="color: #2d9574;">"/tmp/o"</span>).read()) <span style="color: #a89984;"># </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x C-p here</span>
</pre>
</div>
<div class="org-src-container">
<pre class="src src-shell">  <span style="color: #3a81c3; font-weight: bold;">echo</span> ...and bye &gt;&gt; /tmp/o    <span style="color: #a89984;"># </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x C-t again</span>
</pre>
</div>
<div class="org-src-container">
<pre class="src src-python">  <span style="color: #3a81c3; font-weight: bold;">print</span>(<span style="color: #3a81c3; font-weight: bold;">open</span>(<span style="color: #2d9574;">"/tmp/o"</span>).read()) <span style="color: #a89984;"># </span><span style="color: #2aa1ae; background-color: #ecf3ec;">C-x C-p again</span>
</pre>
</div>

<p>
Let's conclude with a GUI example in Java.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp">  <span style="color: #a89984;">;; </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Set &#8220;C-x C-j&#8221; to evaluate Java code in a background REPL.</span>
  (<span style="color: #3a81c3; font-weight: bold;">repl-driven-development</span> [C-x C-j] <span style="color: #2d9574;">"jshell"</span>)
</pre>
</div>
<div class="org-src-container">
<pre class="src src-java">  <span style="color: #a89984;">// </span><span style="color: #2aa1ae; background-color: #ecf3ec;">Select this Java snippet, then press &#8220;C-x C-j&#8221; to evaluate it</span>
  <span style="color: #3a81c3; font-weight: bold;">import</span> <span style="color: #4e3163; font-weight: bold;">javax</span>.<span style="color: #4e3163; font-weight: bold;">swing</span>.*;
  JOptionPane.showMessageDialog(<span style="color: #3a81c3; font-weight: bold;">new</span> <span style="color: #ba2f59; font-weight: bold;">JFrame</span>(){{setAlwaysOnTop(<span style="color: #4e3163; font-weight: bold;">true</span>);}}, <span style="color: #2d9574;">"Super nice!"</span>)
</pre>
</div>

<p>
We can use a preconfigured Java REPL, to remove the annoying “jshell&gt;” prompt
from overlay echos, handle multi-line input, and more.
</p>
<div class="org-src-container">
<pre class="src src-emacs-lisp">  (<span style="color: #3a81c3; font-weight: bold;">repl-driven-development</span> [C-x C-j] java)
</pre>
</div>
<div class="org-src-container">
<pre class="src src-java"> <span style="color: #a89984;">// </span><span style="color: #2aa1ae; background-color: #ecf3ec;">REPL result values are shown as overlays:</span>
 <span style="color: #a89984;">// </span><span style="color: #2aa1ae; background-color: #ecf3ec;">See a list of 23 numbers, which are attached as a tooltip to this text.</span>
 IntStream.range(0, 23).forEach(x -&gt; System.out.println(x))
</pre>
</div>

<p>
For more documentation, and examples,
see <a href="http://alhassy.com/repl-driven-development">http://alhassy.com/repl-driven-development</a>
</p>
</div>
</div>

<div id="outline-container-videos" class="outline-2">
<h2 id="videos"><span class="section-number-2">4.</span> <a href="#videos">Videos</a></h2>
<div class="outline-text-2" id="text-videos">
</div>

<div id="outline-container-teaching-a-javascript-runtime-incrementally-to-be-a-web-server-" class="outline-3">
<h3 id="teaching-a-javascript-runtime-incrementally-to-be-a-web-server-"><span class="section-number-3">4.1.</span> <a href="#teaching-a-javascript-runtime-incrementally-to-be-a-web-server-">REPL Driven Development :: Teaching a JavaScript runtime, incrementally, to be a web server 🍽️ 🔁 🤖</a></h3>
<div class="outline-text-3" id="text-teaching-a-javascript-runtime-incrementally-to-be-a-web-server-">
<div align="center">
<img src="http://alhassy.com/images/rdd-teaching-a-js-runtime-to-be-a-webserver.png" width=400 height=300 />
<a href="https://www.youtube.com/watch?v=b6Z3NQVn4lY"><img src="https://img.youtube.com/vi/b6Z3NQVn4lY/0.jpg" /></a>
</div>
</div>
</div>
</div>
</div>
<div id="postamble" class="status">
<p class="author">Author: Musa Al-hassy</p>
<p class="date">Created: 2023-10-12 Thu 05:40</p>
<p class="validation"><a href="https://validator.w3.org/check?uri=referer">Validate</a></p>
</div>
</body>
</html>