"""
On start up, first run the following code in Sublime's console to run the plugin
    view.run_command("v<verionNumber>")
In Preferences->Settings add the following to stop build results from displaying
    show_panel_on_build=false
"""
import sublime
import sublime_plugin
#must include following lines on Sublime start up
import sys
if '/Library/Frameworks/Python.framework/Versions/3.6/lib/python3.6/site-packages' not in sys.path:
    sys.path.append('/Library/Frameworks/Python.framework/Versions/3.6/lib/python3.6/site-packages')
import importlib
import requests
import urllib3
import stackexchange
import symtable
import re
import keyword
import difflib
import json
import datetime
import shutil
from bs4 import BeautifulSoup
from sumy.parsers.plaintext import PlaintextParser
from sumy.nlp.tokenizers import Tokenizer
from sumy.summarizers.luhn import LuhnSummarizer
from pydoc import help

try:
    defaultExec = importlib.import_module("Better Build System").BetterBuidSystem
except:
    defaultExec = importlib.import_module("Default.exec")

#Stack Overflow URL
urlBase = "https://stackoverflow.com"
urlSearch = "/search?q="

#store builtins
pythonKeywords = keyword.kwlist
#pythonBuiltin = dir(__builtins__) #does not work as expected within Sublime, see/use below
pythonBuiltin = ['ArithmeticError', 'AssertionError', 'AttributeError', 'BaseException', 'BlockingIOError', 'BrokenPipeError', 'BufferError', 'BytesWarning', 'ChildProcessError', 'ConnectionAbortedError', 'ConnectionError', 'ConnectionRefusedError', 'ConnectionResetError', 'DeprecationWarning', 'EOFError', 'Ellipsis', 'EnvironmentError', 'Exception', 'False', 'FileExistsError', 'FileNotFoundError', 'FloatingPointError', 'FutureWarning', 'GeneratorExit', 'IOError', 'ImportError', 'ImportWarning', 'IndentationError', 'IndexError', 'InterruptedError', 'IsADirectoryError', 'KeyError', 'KeyboardInterrupt', 'LookupError', 'MemoryError', 'ModuleNotFoundError', 'NameError', 'None', 'NotADirectoryError', 'NotImplemented', 'NotImplementedError', 'OSError', 'OverflowError', 'PendingDeprecationWarning', 'PermissionError', 'ProcessLookupError', 'RecursionError', 'ReferenceError', 'ResourceWarning', 'RuntimeError', 'RuntimeWarning', 'StopAsyncIteration', 'StopIteration', 'SyntaxError', 'SyntaxWarning', 'SystemError', 'SystemExit', 'TabError', 'TimeoutError', 'True', 'TypeError', 'UnboundLocalError', 'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeError', 'UnicodeTranslateError', 'UnicodeWarning', 'UserWarning', 'ValueError', 'Warning', 'ZeroDivisionError', '_', '__build_class__', '__debug__', '__doc__', '__import__', '__loader__', '__name__', '__package__', '__spec__', 'abs', 'all', 'any', 'ascii', 'bin', 'bool', 'bytearray', 'bytes', 'callable', 'chr', 'classmethod', 'compile', 'complex', 'copyright', 'credits', 'delattr', 'dict', 'dir', 'divmod', 'enumerate', 'eval', 'exec', 'exit', 'filter', 'float', 'format', 'frozenset', 'getattr', 'globals', 'hasattr', 'hash', 'help', 'hex', 'id', 'input', 'int', 'isinstance', 'issubclass', 'iter', 'len', 'license', 'list', 'locals', 'map', 'max', 'memoryview', 'min', 'next', 'object', 'oct', 'open', 'ord', 'pow', 'print', 'property', 'quit', 'range', 'repr', 'reversed', 'round', 'set', 'setattr', 'slice', 'sorted', 'staticmethod', 'str', 'sum', 'super', 'tuple', 'type', 'vars', 'zip']
#python3 data types
pythonDataTypes = ['int','float','complex','bool','str','bytes','list','tuple','set','dict']
pythonDataTypesEnglish = ['integer','float','complex','boolean','string','bytes','list','tuple','set','dictionary']
#syntax from http://rigaux.org/language-study/syntax-across-languages.html#StrngCSTSSASn
#first entry is python, rest are other langauges
syntaxAcrossLanguages = [['#', '//', '--', ';', '%', 'rem', "'", '"', '\\', '!', 'NB.', 'C or * in column 1'], [', (* ... *)', '%( ... %)', '/* ... */', '{ ... }', '{- ... -}', '#| ... |#', '#= ... =#', '#[ ... ]', '#if 0 ... #endif', 'comment { ... }', 'comment [ ... ]', '[ ... ]', '--[[ ... ]]'], [', " ... "', '/* ... */', '<;!-- ... -->;', '( ... )', '<pre>###...###</pre>'], [', ///', '-- |', '-- ^'], [', /** ... */', '(** ... *)'], ['<pre>class X:"""..."""def x():"""..."""</pre><a href="#4">(4)</a>', ', {-| ... -}', '(** ... *)', '/* DOCUMENT ... */', 'indexing              identifier: "...";', "someClass comment: '...'", 'rebol [ Note: "..." ]', 'func ["..." arg] ...', '(define (f para1 para2) "..." ...)', '(defun f (para1 para2) "..." ...)', '<pre>=pod...=cut</pre>', '<pre>=begin...=end</pre>', '<pre>function MYFUNCTION%MYFUNCTION the very first comment line is displayed in the help table of contents%% the remaining lines are displayed when getting help for MYFUNCTION%</pre>'], ['__file__', ', __LINE__ __FILE__', '__LINE__ __SOURCE_FILE__', '$?LINE $?FILE', '<pre>(new System.Diagnostics.StackFrame(true)).GetFileLineNumber()(new System.Diagnostics.StackFrame(true)).GetFileName()</pre>', '<pre>Thread.currentThread().getStackTrace()[1].getLineNumber();Thread.currentThread().getStackTrace()[1].getFileName();</pre>', 'system/script/header/file', 'SOURCELINE() / parse source OS . SOURCENAME', 'info frame 0', 'thisContext lineNumber / thisContext method source', 'runtime.Caller(0)'], [', case-sensitive', 'case-insensitive', 'case-sensitive: variables<br/>case-insensitive: keywords, functions, constants...', 'case-sensitive: identifiers<br/>case-insensitive: keywords', 'case-sensitive: identifiers<br/>case-insensitive: commands', 'case-sensitive: upper case disallowed'], ['CamelCase for classes, underscores for methods', ', camelCase', 'CamelCase or camelCase', 'underscores', 'dots', 'hyphens', 'underscores for functions / types, unclear for modules / constructors', 'UPPER_CASE', 'lowercasenoseparator', 'underscores, UPPER_CASE for class names', 'CamelCase for types, underscores for functions, variables, ...', 'CamelCase for methods, types and modules, underscore for functions', 'CamelCase for modules and classes, ALL_CAPS for constants, underscores for functions, variables, ...', 'CamelCase for modules and classes, ALLCAPS for macros, underscores for methods, constants and variables', 'CamelCase for modules, ALL_CAPS for constants, unclear for functions / variables', 'CamelCase for variables, underscores for predicates', 'usually lowercase or underscores, ALL_CAPS for macros', 'usually underscores', 'Camel_Case'], ['[_a-za-z][_a-za-z0-9]*', '[_a-zA-Z][_a-zA-Z0-9]*', ', [a-zA-Z][a-zA-Z0-9]*', '[a-zA-Z][_a-zA-Z0-9]*', '[a-zA-Z](_?[a-zA-Z0-9])*', '[_a-zA-Z0-9]+', '[a-zA-Z0-9]+', "[_a-zA-Z][_a-zA-Z0-9]* or '[^']*'", '[_a-zA-Z$][_a-zA-Z0-9$]*', '[$A-Za-z_\\x7f-\\uffff][$\\w\\x7f-\\uffff]*', '[a-zA-Z%][a-zA-Z0-9]*', '[_a-z][_a-zA-Z0-9]*', "[_a-z][_a-zA-Z0-9]*[!?']*", "[_a-z][_a-zA-Z0-9']*", "[_a-zA-Z][_a-zA-Z0-9']*", '[_A-Z][_a-zA-Z0-9]*', '[_a-zA-Z!0&amp;*/:<;=>;?^][_a-zA-Z!0&amp;*/:<;=>;?^0-9.+-]*', '[a-zA-Z!?@#][a-zA-Z0-9!?@#]*', '[_a-zA-Z?!.\'+*&amp;|=~-][_a-zA-Z0-9?!.\'+*&amp;|=~-]* or <br/>[^0-9[](){}":;/][^ \\n\\t[](){}":;/]*', '[a-z][a-z0-9_]*', 'anything without a space and is not a number'], [', [_a-zA-Z][_a-zA-Z0-9]*[!?]?', '[_a-z][_a-zA-Z0-9]*', '[A-Z][_a-zA-Z0-9]*', '[^ \\t\\n\\r\\f]+', '[^ \\t\\n\\r\\f/]+'], [', [A-Z]+'], [", [_A-Z][_a-zA-Z0-9']*", "[_a-z][_a-zA-Z0-9']*"], ["[_a-z][_a-za-z0-9']*", ', [A-Z][_a-zA-Z0-9]*', "[_A-Z][_a-zA-Z0-9']*", "[_a-z][_a-zA-Z0-9']*"], ['\\', ', nothing needed', '_', ',', '~', '...'], [', =', ':=', '<;-', '_', ':', '->;', 'def', 'setq', 'setf', 'set', 'SET v=...', 'set!', 'is', 'make "v e', 'e v !'], ['global v1, v2', ', =', '<;-', ':-', ':=', 'let v = e in', 'let val v = e in', 'let v = e', 'def v := e / var v := e', 'my / our / local / use vars', 'my / our / temp', 'define', 'let let*', 'letrec', 'flet labels defun defmethod defvar defparameter defsetf ..', 'local V1 = e V2 = e2 in ... end', 'global v1 v2', ':@', 'NEW v', 'v: t', 't v', 'var v t', '| v1 v2 |', 'auto v1, v2; extrn v3, v4;', 'auto', 'var', 'gvar', 'variable v', 'e value v', 'Module[{x, y = v},  ... ]', 'Block[{x, y = v}, ... ]', 'With[{c1 = v1, c2 = v2, ... }, ...]', '<;xsl:variable name="v" select="e"/>;'], [', =', ':=', 'set, variable'], [', ( ... )', '[ ... ]', 'indentation', '$ ...', 'begin ... end', 'space'], ['{ ... }', 'indentation', ', { ... }', '{ ... }', '( ... )', '[ ... ]', '"..."', 'begin ... end', '(begin ...)', 'BEGIN ... END', 'do ... end', 'do ... end', 'indentation', 'foo ... end  where foo in { if, do, ... }', 'foo ... end  where foo in { if, for, while, ... }', 'foo ... end  where foo in { if, loop, ... }', 'foo ... end foo  where foo in { if, do, ... }', '(* ... *)', '(# ... #)'], [', valof', 'do', 'proc() .. end proc'], ['is / is not', ', == !=', '= /=', '= <;>;', '= #', '= !=', '== === != !==', '=== !==', '== ~=', '== ~~', '== ~==', "= '=", '= ~= neqv', 'f= f<;>;', 'is_equal', 'eq ne', 'eq, eql', 'eq? eqv?', '.EQ. .NE.', 'is / isnot'], ['== &lt;&gt;', '== /=', '== \\=', '== \\==', '= /=', '= !=', '= <;>;', '= ~=', '== ~= eq ne isequal isequalwithequalnans', '== ~= eq ne isequal', '=@= \\=@= / = \\= / =:= =\\=', '=== =!= / == !=', '.eq', 'equal?', 'equals', 'equal', 'equalp', 'eqv', 'deep_is_equal', 'isEqual'], [', &lt; &gt; &lt;= &gt;=', '<; >; =<; >;=', "<; >; '>; '<;", '<;<; >;>; <;<;= >;>;=', '@<; / @=<; / @>; / @>;=', 'lt gt le ge', '-lt -gt -le -ge', '.LT. .GT. .LE. .GE.', 'u<; u>; u<;= u>;=', 'f<; f>;'], ['cmp', ', a <;=>; b', 'compare', 'strcmp', 'three_way_comparison', 'string compare', 'compareTo', 'strings.Compare()'], [', compare', 'compareTo'], ['min / max', ', min / max', 'min minstr / max maxstr', 'Min / Max', 'MIN / MAX', 'measure-object -min / measure-object -max', 'fmin / fmax', "Integer'min / Integer'max"], ['exec', 'exec', 'CoffeeScript.eval', 'evstr / execstr', 'dostring', 'doString', 'evaluate', 'Compiler evaluate:', 'runtime_compile / compile + execute', 'Compiler.evalExpression or Compiler.parseOzVirtualString', 'compile_string', 'interpret', 'ToExpression', 'run', 'XECUTE', 'do / reduce / compose / load', '[...]', '=..'], [', malloc', 'allocate throw', 'new'], [', free', 'free throw'], ['gc.collect()', ', doGC', 'GC.start', 'gc', 'System.gc()', 'System.gcDo', 'System.GC.Collect()', 'gc_collect_cycles', 'full_collect', 'garbage_collect', 'collectgarbage', 'Collector collect', 'VM.garbageCollect()', 'Gc.full_major()', 'Smalltalk garbageCollect', 'System.Mem.performGC', 'incremental garbage collection =>; not needed', 'recycle', 'interp.gc()', '(ext:gc)', 'runtime.GC()'], [', f(a,b,...)', 'f a b ...', 'f(a,b,...f) or f[a,b,...] depending on the version', '(f a b ...) (apply f l)', '(funcall f a b ...)', '{f a b}', '[apply f a b]', 'f[a,b,...]', 'f[a,b,...] or f.call(a,b,...)', '&amp;$f(a,b,...) or $f->;(a,b,...)', '$f.(a,b,...)', 'f a, b, ...', 'f, a, b, ...', 'v = f(a, b, ...) or call f a, b, ...', 'a b ... f', '(a,b,...)->;&amp;f or (a,b,...)->;f', 'f:a', 'f@a', 'a // f', 'a ~ f ~ b', '.. [ f, A, B, ...]', '<pre><;xsl:call-template name="f">;    <;xsl:with-param name="a" select=a/>;    <;xsl:with-param name="b" select=b/>;<;/xsl:call-template>;</pre>'], ['f()', ', f', '(f)', '(funcall f)', '{f}', 'f[]', 'f[] or f.call', '&amp;$f or $f->;()', '$f.()', 'v = f()', 'call f', 'f value', '<;xsl:call-template name="f">;/'], ['functools.partial(f, a)<a href="#31">(31)</a>', ', f a', 'f(a)', 'f(a,)', '&amp;f.assuming(var_name =>; a)', 'interp alias {} f_a {} f a'], [', f(,b)', '&amp;f.assuming(b =>; b)', 'flip f b'], [', (a >;)', '(>;) a'], [', (>; a)'], ['def f(para1, para2, ...): ...', ', sub f { ... }', 'sub f($para1, $para2, ...) { ... }', 'def f(para1, para2, ...) ... end', 'def f(para1, para2, ...) ... { ... }', 'f para1 para2 = ...', 'let f para1 para2 = ...', 'f(para1, para2, ...) = valof $( ... $)', 'f(para1, para2, ...) = ...', 'f[para1_, para2_, ...] := ... para1 ...', 'f ...  or  f: para1 ...', 'f: func [para1 para2 ...] ...', '/f { ... } def', 'f := (para1, para2, ...) ->; ...', 'f := method(para1, para2, ..., code)', 'func f(a, b, c...) { ... }', 'typ0 f(typ1 para1, typ2 para2, ...) { ... }', 'function f(para1, para2) { ... }', 'function f(para1, para2) ... code ... end', 'function f; ...; end', 'function f { ... }', 'function f { param(para1, [typ2]para2, ...) ... }', '(define (f para1 para2) ...)', '(defun f (para1 para2) ...)', 'fun { F Para1 Para2 } ... end', 'fun f para1 para2 = ...', 'proc f {para1 para2} { ... }', '<pre>function retval = f(para1, para2)retval = ...</pre>', '<pre>:- func f(typ1, typ2, ...) = typ0.f(Para1, Para2, ...) = ...</pre>', '<pre>function f(para1 : type1; para2 : typ2; ...) return retval isbegin   ...end f;</pre>', '<pre>function f para1 para2 ->; retval  arg typ1 para1; arg typ2 para2; arg rettyp retval;  ...</pre>', '<pre>function f(para1 : typ1, para2 : typ2, ...) : retval;var retval : typ0;begin  ...end</pre>', '<pre>f (para1 : typ1; para2, para3 : typ2; ...) : rettyp isdo  ...end</pre>', '<pre><;xsl:template name="f">;    <;xsl:param name="para1"/>;    <;xsl:param name="para2"/>;    ...<;/xsl:template>;</pre>', 'Function f(para1, para2)...End Function', ': f ... ;', 'f() { ... }', '<pre>f : procedure  ...return retval</pre>', '<pre>to f :para1 :para2   ...end</pre>', 'func f(para1 typ1, para2 typ2, ...) (typ3, ...) { ... }'], [', <pre>procedure f(para1 : typ1; para2, para3 : typ2);begin  ...end</pre>', '<pre>f (para1 : typ1; para2, para3 : typ2; ...) isdo  ...end</pre>', '<pre>procedure f(para1 : typ1; para2 : MODE type2; ...) isbegin   ...end f;MODE ::= | OUT | IN OUT</pre>', 'void f(typ1 para1, typ2 para2, ...) { ... }', 'let f(para1, para2, ...) be $( ... $)', 'proc { F Para1 Para2 } ... end', 'f := proc(para1, para2, ...) ... end proc', 'Sub f(para1, para2)...End Sub', '<pre>function f(para1, para2)...</pre>', '<pre>f : procedure  ...return</pre>', 'func f(para1 typ1, para2 typ2, ...) { ... }'], [', one can use overloading on different number of arguments', 'sub f { ... @_ }', 'sub f; ... $argv; end', 'f() { ... $@ }', 'f := ... ## &amp;', 'f[params___] := ... params ...', '<pre>function f(varargin)for i=1:nargin ...(varargin{i})end</pre>', '<pre>function f(varargin)for e=varargin ...(e)end</pre>', '(args...) ->; ...', '(lambda x ...) or', 'f(args ...typ0)'], [', f(Para1, Para2, ....) :- ... .'], ['lambda a, b: ...', ', sub { my ($a, $b) = @_; ... }', '{ my ($a, $b) = @_; ... }', '{ ... } (arguments are in the stack', '[ ... ]', '{ param(para1, [typ2]para2, ...) ... }', '{|a, b| ... }', '[:a :b| ... ]', '[list {a b} {...}]', 'lambda(typ1 para1, typ2, para2, ...) { ... };', '(a, b) =>; ...', '(a, b) ->; ...', 'a, b ->; ...', '->; $a, $b { ... }', '\\a b ->; ...', ':noname ...', 'fn (a, b) =>; ...', 'fun a b ->; ...', '(func(A, B) = C :- ...)', 'function(a, b) { ... }', 'function(a, b) use (&amp;$v1, $v2) { ... }', 'function(a, b) ... end', 'Function[{a, b}, ....]', 'fun(a, b) ->; ... end', 'fun {$ A B} ... end', 'func [a b ...] ...', 'def _(para1, para2, ...) ... { ... }', 'proc {|a, b| ...}', 'lambda {|a, b| ...}', '(lambda (a b) ...)', "inline('...x...y...')", 'method(a, b, ...)', 'method(a, b) ... end method', "create_function(',','...')", 'delegate(ta a, tb b) { ... }', '[](ta a, tb b) { ... }', '[](ta a, tb b) ->; typ { ... }', 'func(para1 typ1, ...) (typ2, ...) { ... }'], ['return', ', return<a href="#42">(42)</a>', 'Return', 'RETURN', 'resultis', 'return-from xxx', '^', 'Exit Function / Exit Sub', 'exit', 'output'], [', no syntax needed'], [', Result := val', '<;function name>; = val', '<;function name>; := val', '<;retvar name>; = val;'], ['__getattr__', ', AUTOLOAD', 'AUTOSCALAR, AUTOMETH, AUTOLOAD...', '__autoload', 'method_missing', 'doesNotUnderstand', '__noSuchMethod__', 'unknown', 'no-applicable-method', 'doesNotRecognizeSelector', 'TryInvokeMember', 'match [name, args] { ... }', 'the predicate fail', 'forward'], ['inspect.stack()[1]', ', caller', 'call', 'backtrace', 'debug_backtrace', "trace 'I'", "evalin('caller', ...)", 'current_predicate', 'thisContext sender', 'where(2)', 'info level', 'runtime.Caller(0)'], [', .', '~', 'o', '@', 'compose', 'Composition', '<;<;', '>;>;'], ['identity', ', id', 'identity', 'Identity', 'yourself'], ['end-of-line', ', ,', '.', ':', 'nothing, optionally ;', 'space', '(begin ...)', '(progn ...) (prog1 ...) (prog2 ...)', '>;>;'], ['if c then ...', 'if c: ...', ', if c then ...', 'if c then ... end', 'if c then ... end if', 'if c then ... fi', 'if c; then ... fi', 'if (c) then ... end', 'if c do ...', 'IF c THEN ... END', 'if (c) ...', 'if c ...', 'if (c): ... endif', 'if c {...}', 'if c [...]', 'if (c) {...}', 'IF c ...', 'c ->; ...', 'c ... if', '... if c', 'c if b1 then', '(if c ...)', '(when c ...)', 'c and ...', 'if(c, ...)', 'If[c, ...]', 'if(c) then(...)', 'c ifTrue(...)', 'c ifTrue: ...', '<;xsl:if test="c">;...<;/xsl:if>;', 'if c ... endif', 'If c Then ...', '<pre>If c  ...End If</pre>', 'if c; ... end', 'if c; ...; end', 'if c, ..., end', '<pre>if c  ...end</pre>', '<pre>if c then ; ...if c then  ...if c then do  ...end</pre>', 'c and ...', 't label', '<pre>if c    ...</pre>'], ['<pre>if c:   b1 elif c2:  b2 else:   b3</pre>', ', if c then b1 else b2', 'if c then b1 else b2 end', 'if c then b1 elsif c2 then b2 else b3 end if', 'if c then b1 elseif c2 then b2 else b3 end', 'if (c) then b1 elseif (c2) then b2 else b3 end', 'IF c THEN b1 ELSIF c2 THEN b2 ELSE b3 END', 'If c Then b1 ElseIf c2 Then b2 Else b3 End If', 'if (c) b1 else b2', 'if c b1 elsif c2 b2 b3', 'if c then b1 elseif c2 then b2 else b3', 'if c then begin b1 end else begin b2 end', 'if c b1 eif c2 b2 else b3', 'if c then b1 elif c2 then b2 else b3 end if', 'if c; then b1; elif c2; then b2; else b3; fi', 'if c; b1; else b2; end', 'if c1, b1, elseif c2, b2, else, b3, end', 'if (c) b1 elseif (c2) b2 else b3', 'if (c): b1 elseif (c2): b2 else: b3 endif', 'if (c) {b1} elsif (c2) {b2} else {b3}', 'if (c) {b1} else {b2}', '(if c b1 b2)', '(if c then b1 else b2)', '(c ->; b1 ; c2 ->; b2 ; b3)', 'c ->; b1 ; b2', 'if(c, b1, b2)', 'If[c, b1, b2]', 'if(c) then(b1) else(b2)', 'c ifTrue: b1 ifFalse: b2', 'ifelse c [b1] [b2]', 'shunt c b1 c2 b2 b3', 'either c b1 b2 / if/else c b1 b2', '(cond (c b1) (c2 b2) (t b3))', '(cond (c b1) (c2 b2) (else b3))', 'Which[c, b1, c2, b2, True, b3]', 'c ->; b1 ; c2 ->; b2 ; b3', 'case when c; b1 when c2; b2 else b3 end', 'test c then b1 or b2', 'e | c = b1 | c2 = b2 | otherwise = b3', 'c b1 b2 ifelse', 'c if b1 else b2 then', 'c ? b1 : b2', 'c ?? b1 !! b2', '$SELECT(c:b1,c2:b2,1:b3)', 'c ->; b1, b2', '(if c then b1 else b2 fi)', '<pre><;xsl:choose>;    <;xsl:when test="c">; b1 <;/xsl:when>;    <;xsl:when test="c2">; b2 <;/xsl:when>;    <;xsl:otherwise>; b3 <;/xsl:otherwise>;<;/xsl:choose>;</pre>', 'if c1 ... elseif c2 ... else ... endif', 'If c Then b1 Else b2', '<pre>If c  b1Else  b2End If</pre>', '<pre>if c  b1elsif c2  b2else  b3end</pre>', '<pre>if c  b1elseif c2  b2else  b3end</pre>', '<pre>if c then ; b1 ; else ; b2if c then  b1else  b2if c then do  b1  ...endelse do  b2  ...end</pre>', '<pre>IF c ...ELSE ...</pre>', '<pre>if c  b1else if c2  b2else  b3</pre>', 'if c {b1} else if c2 {b2} else {b3}'], [', unless', 'ifFalse', 'if(c) not then(...)'], [', <pre>switch (val) {    case v1: ...; break;    case v2: case v3: ...; break;    default: ...; }</pre>', '<pre>switch val {    case v1: ...; goto done;   case v2: case v3: ...; goto done;  } ...; done: </pre>', '<pre>switch (val) {    case v1: ...; break;    case v2: case v3: ...; break;    default: ...; break; }</pre>', '<pre>switch (val) {    match v1 { ... }    match v2 { ... }    match _ { ... }}</pre>', '<pre>switchon val     case v1: ...   case v2: ...   default: ...</pre>', "<pre>switch val  case v1    ...  case v2 v3    ...  case '*'    ...end</pre>", '<pre>switch val  case v1    ...  case {v2,v3}    ...  otherwise    ...end</pre>', '<pre>case val of   v1 : ...;    v2, v3 : ...   else ... end</pre>', '<pre>switch val {    v1 {...}     v2 - v3 {...}    default {...}}</pre>', '<pre>case val in   v1) statement1 ;;   v2|v3) statement23 ;;   *) statement_else ;;esac</pre>', '<pre>(if val    // v1 then ...    // v2 then ...     else ...    if)</pre>', '<pre>match val with | v1 ->; ... | v2 | v3 ->; ... | _ ->; ...</pre>', '<pre>case val of   v1 =>; ... | v2 =>; ... | _ =>; ...</pre>', '<pre>CASE val OF   v1 =>; ... | v2 =>; ... ELSE =>; ... END</pre>', '<pre>case val of   v1 ->; ...   v2 ->; ...   _ ->; ...</pre>', '<pre>val case   v1 of ... endof   v2 of ... endof   ...endcase</pre>', '<pre>val.   v1 ->; ...   v2 ->; ...   _ ->; ...</pre>', '<pre>(case val   ((v1) ...)   ((v2) ...)   (otherwise ...))</pre>', '<pre>(case val   ((v1) ...)   ((v2) ...)   (else ...))</pre>', '<pre>case val is   when v1 =>; ...   when v2 | v3 =>; ...   when others =>; ... end case;</pre>', '<pre>case val   when v1; ...   when v2, v3; ...   else ... end</pre>', '<pre>inspect val   when v1 then statement1   when v2, v3 =>; statement23   else statement_else end</pre>', '<pre>select (val);   when (v1) statement1;   when (v2, v3) statement23;   otherwise statement_else; end;</pre>', '<pre>X = val,(X = v1, ... ; X = v2, ... ; ...)</pre>', '<pre>my %case = (    v1 =>; sub { ... },    v2 =>; sub { ... },); if ($case{val}) { $case{val}->;() } else { ... }</pre>', '<pre>use Switch;switch ($val) {    case v1 { ... }    case v2 { ... }    else ...})</pre>', '<pre>given $val {    when v1 { ... }    when v2 { ... }    default { ... }}</pre>', '<pre>Select val    Case v1\t...    Case v2, v3\t...    Case Else\t...End Select</pre>', '<pre>switch (val) {    v1 { ... }    v2 { ... }    default { ... }  }</pre>', '<pre>switch val [    v1 [...]    v2 [...]]switch/default [    v1 [...]    v2 [...]][...]</pre>', 'val caseOf: {[v1]->;[...]. [v2]->;[...]} otherwise: ...', 'Switch[val, v1, ..., v2, ..., _, ...]', '<pre>select  when v1 ...  when v2 | v3 ...  otherwise ...end</pre>', '<pre>CASE val    WHEN v1 THEN ...    WHEN v2 THEN ...    ELSE ...END</pre>', '<pre>switch val {    case v1, v2, ...:        ...        fallthrough    case v3:        ...    default:        ...}</pre>'], [', loop', 'loop(...)', 'loop ... end loop', 'LOOP ... END', '(loop do ...)', 'cycle (# do ... #)', 'repeat', 'begin ... again', 'forever', '<pre>Do    ...Loop</pre>', '<pre>do forever  ...end</pre>', 'for {}'], ['while c do ...', 'while c: ...', ', while (c) ...', 'while c ...', 'while c loop ... end loop', 'while c do ...', 'while c do ... done', 'while c do ... end do', 'while c do ... end', 'WHILE c DO ... END', 'while c; do ...; done', 'while c; ...; end', 'while c, ..., end', 'while [c][...]', 'while c [...]', 'while(c, ...)', 'While[c, ...]', 'do.while [...] c', 'c whileTrue: ...', '(loop while c do ...)', 'loop (# while ::<; (# do c ->; value #) do ... #)', 'begin c while ... repeat', 'from until not c loop ... end', '<pre>while c    ...</pre>', '<pre>while c do    ...</pre>', '<pre>Do While c     ...Loop</pre>', 'while c ... endwhile', '<pre>do while c  ...end</pre>', 'for c {...}'], ['repeat ... until c', ', do ... until c', 'do {...} until c', 'do ... while (!c)', 'begin ... end until c', 'begin ... c until', 'REPEAT ... UNTIL c', 'loop (# until ::<; (# do c ->; value #) do ... #)', 'loop ... exit when c; end loop', '(loop do ... until c)', '... repeatuntil c', 'repeat ... until c', 'repeat ... until (c)', 'repeat, ..., c', 'until [... c]', 'until c [...]', 'do.while [...] c', 'While[...; c]', '[...] whileFalse: [c]', '<pre>Do ...Loop Until c</pre>'], ['for i in xrange(1, 11)', ', for (int i = 1; i <;= 10; i++) ...', 'for (i = 1; i <;= 10; i++) ...', 'for ($i = 1; $i <;= 10; $i++) ...', 'foreach my $i (1 .. 10) { ... }', 'foreach ($i in 1..10) { ... }', 'for (1 .. 10) ->; $i { ... }', 'for i = 1:10, ..., end', 'for i = 1, 10 do ... end', 'for i := 1 to 10 do ...', 'for i = 1 to 10 do ... done', 'For i = 1 To 10 ... Next', 'for i in 1 .. 10 loop ... end loop', 'for i in 1 .. 10 do ... done', 'for i in [1..10] ...', 'for i in (seq 10); ...; end', 'FOR I=1:1:10 ...', 'for i from 1 to 10 do ... end do', 'for [i 1 10 +1] [...]', 'for {set i 1} {$i <;= 10} {incr i} {...}', '1 1 10 ... for', '11 1 do ... loop', '(1..10).each {|i| ... }', '1.upto(10) {|i| ... }', '1 to(10) foreach(...)', '1 to: 10 do: [...]', '(loop for i from 1 to 10 do ...)', 'do label i = 1, 10', 'Do[..., {i, 1, 10}]', '<pre>do i = 1 for 10  ...end</pre>', 'for i := 1; i <;= 10; i++ {...}'], ['for i in xrange(10, 0, -1)', ', for X := 10 downto 1 do ...', 'for i = 10 downto 1 do ... done', 'for i in reverse 1 .. 10 loop ... end loop', 'for i in 10 .. -1 .. 1 do ... done', 'for (int i = 10; i >;= 1; i--) ...', 'for (my $i = 10; $i >;= 1; $i--) { ... }', 'loop (my $i = 10; $i >;= 1; $i--) { ... }', 'for (i = 10; i >;= 1; i--) ...', 'for ($i = 10; $i >;= 1; $i--) ...', 'from i := 10 until i <; 1 loop ... i := i - 1 end', 'for i = 10:-1:1, ..., end', 'for i = 10, 1, -1 do ... end', 'For i = 10 To 1 Step -1 ... Next', 'for i in `seq 10 -1 1`; do ...; done', 'for i in (seq 10 -1 1); ...; end', 'for i from 10 to 1 by -1 do ... end do', 'for [i 1 10 -1] [...]', 'FOR I=10:-1:1 ...', 'for {set i 10} {$i >;= 1} {incr i -1} {...}', '10 -1 1 ... for', '1 10 do ... -1 +loop', '1 to: 10 by: -1 do: [...]', '10 to(1) foreach(...)', '10.downto(1) {|i| ... }', '(loop for i from 1 to 10 by -1 do ...)', 'do label i = 10, 1, -1', 'Do[..., {i, 10, 1, -1}]', '<pre>do i = 10 to 1 by -1  ...end</pre>', 'for i in [10..1] ...', 'for i := 10; i >;= 1; i-- {...}'], ['for i in xrange(1, 11, 2)', ', for (int i = 1; i <;= 10; i += 2) ...', 'for (i = 1; i <;= 10; i += 2) ...', 'for ($i = 1; $i <;= 10; $i += 2) ...', 'for (my $i = 1; $i <;= 10; $i += 2) { ... }', 'loop (my $i = 1; $i <;= 10; $i += 2) { ... }', 'from i := 1 until i >; 10 loop ... i := i + 2 end', 'for i = 1:3:10, ..., end', 'for i = 1, 10, 2 do ... end', 'For i = 1 To 10 Step 2 ... Next', 'for i in 1 .. 2 .. 10 do ... done', 'for i in (seq 1 2 10); ...; end', 'for i from 1 to 10 by 2 do ... end do', 'for [i 1 10 2] [...]', 'FOR I=1:2:10 ...', 'for {set i 0} {$i <;= 10} {incr i 2} {...}', '1 2 10 ... for', '11 1 do ... 2 +loop', '1 to: 10 by: 2 do: [...]', '(1..10).step(2) {|i| ... }', '1 to (9,2) foreach(...)', '(loop for i from 1 to 10 by 2 do ...)', 'do label i = 1, 10, 2', 'Do[..., {i, 1, 10, 2}]', '<pre>do i = 1 to 10 by 2  ...end</pre>', 'for i in [1..10] by 2 ...', 'for i := 1; i <;= 10; i += 2 {...}'], [', for', 'loop', 'for ((x = 0; x <; 10; x++)); do ...; done', 'from init_code until c loop ... incr_statement end', '(loop with VAR = INITIAL-VALUE ... while CONDITION finally INCREMENT ...)'], ['return', ', return<a href="#42">(42)</a>', 'Return', 'RETURN', 'resultis', 'return-from xxx', '^', 'Exit Function / Exit Sub', 'exit', 'output'], [', goto', 'Goto', 'go throw', 'signal', 'b', 'b, bra, jmp'], [', continue / break', 'Continue / Break', 'next / last', 'next / break', '/ break', '/ break/return', '/ exit', '/ stop', 'restart / leave', '/ Exit Do, Exit For', '/ return-from xxx  or  return', 'iterate / leave', '/ leave'], [', redo/', 'redo / retry'], ['raise', ', raise', 'RAISE', 'raise ... end', 'Exception raise', 'throw', 'Throw', 'throw/name', 'die', 'return -code', 'error', 'signal', 'signal predefined_condition_name', 'cerror warn', '[NSException raise:name ...]', 'panic(v)'], ['catch', ', try: a except exn: ...', 'try a with exn ->; ...', 'try a catch (exn) ...', '<pre>try a  ...catch exn  ...</pre>', 'try { ... } catch(t $v) { ... }', 'try a catch exn then ... end', 'try a catch exn: ... end try', 'try(a) ; catch(...)', 'try { a CATCH exn { ... } }', 'TRY a EXCEPT exn =>; ... END', 'a handle exn =>; ...', 'a on: exception_name do: [:exn | ...]', 'ifCurtailed', 'rescue', 'eval {a}; if ($@) ...', 'exception when exception_name =>;', 'catch a (\\exn ->; ...)', 'catch', 'Catch', 'catch/name', 'catch(...) or catch { ... };', 'if (catch(exn)) { ... } a', 'pcall', 'with-exception-handler or guard', 'block a exception(exn) ... end', '?, shy, safe', 'handler-bind handler-case ignore-errors', 'NS_DURING a NS_HANDLER ... NS_ENDHANDLER', '<pre>try  acatch  ...end</pre>', '<pre>signal on predefined_condition_name...predefined_condition_name :  ...</pre>', 'recover()'], ['finally', 'finally', ', ensure', 'FINALLY', 'unwind-protect', 'cleanup', 'dynamic-wind'], [', retry', 'restart'], [', resume'], [', call-with-current-continuation', 'callcc'], ['n = t', 'n = t', ', typedef t n', 'type n is t', 'type n ...', 'type n = t', 'TYPE n = t', 'using n = ...', 'data n = t', 'datatype n = t', 'newtype n = t', 'n : t', "(deftype n () 't)", 'type n t'], [', :', '::', '!!', 't v', '(declare (t v))', 'v :@ t', '_t', 'var n t'], [', (t) e', 't(e)', '[t] e', 'static_cast<;t>;(e)', 'e :>; t', 'e : t', 'upcast e', 'CAST(e as t)', 'typecast(e,t)', '(t)(e)'], [', (t) e', 't(e)', 'e : t', '[t] e', 'dynamic_cast<;t>;(e)', 'e as t', 'e :?>; t', 'downcast e', 'v ?= e', 'NARROW(e, t)', 'typecast(e,t)'], ['t(e)', ', (t) e', '[t] e', 't e', 'e : t', 'e :: t', 'cast e t', '... cast t', 'make t e / to t e'], [', mutability is the default', 'val x: T', 'T ref', 'STRef a T', 'in out T'], [', const T', 'constant T', 'const x: T', 'constness is the default', 'const e t'], [', "readonly" fields', '"final" fields, parameters, local variables'], [', object.method(para)', 'object#method para', 'object:method(para)', 'object method(para)', 'object method para', 'object method: para1 method_continuation: para2', 'object <;- method(para)', '[ object method: para ]', 'object->;method(para)', 'object["method"](para)', 'object/method para', 'method object para', '(method object para)', 'method(object, para)', 'para->;method', '(send object method para)'], ['object.method()', ', object.method', 'object.property', 'object#method', 'object:method', 'object->;method', 'object->;method()', 'object/method', 'object["method"]()', 'object method', '[ object method ]', 'method object', '(method object)', 'method(object)', '(send object method)'], ['class_name(...)', ', new', 'new class_name(...)', 'new class_name ...', 'class_name.new(...)', 'class_name new', 'class_name v(...)', 'v : class_name', 'class_name.Create', '!class_name!constructor_name(...)', '&amp;', 'make-object', '(make-instance class_name ...)', '[class_name alloc]', 'make class_name! ...', 'def object_name { ... }'], ['o.clone', 'copy.copy(o)<a href="#61">(61)</a>', ', o.clone', 'o.clone', 'o.deep_clone', 'o.clone()', 'o.Clone()', 'clone $o', 'o clone', 'clone / copy or deepCopy', 'Storable::dclone', '[o copy]', 'purecopy', '{<; >;}  or  Oo.copy o', 'o2 = o', '$o2 = $o', 'o2.all := o.all', 'make o []', "o_ : T'Class := o"], ['dealloc', 'del, __del__', ', delete', 'destroy', 'DESTROY', 'dealloc', 'Dispose', '__destruct', 'Requires instantiation of Ada.Unchecked_Deallocation'], [', class', 'class c inherit p1 p2 ... feature decl decl ... end', 'defclass defstruct', 'subclass', 'struct', 'type', 'type c is tagged record ... end record', '@interface c { ... } ... @end', ':', 'type c() = class ... end', '<pre>type c() =  ...</pre>'], ['isinstance', ', isa', 'is_a? kind_of?', 'o.meta.isa', 'isKindOf', 'isKindOfClass', 'dynamic_cast', 'instanceof', 'in', 'is', 'is_a', ':?', 'Program.inherits or Program.implements', 'entry_type', 'typep', 'ISTYPE', 'object## <; classname##', 'type.accepts(object) / object =~ v : type', 'var ?= val'], ['__class__', ', class', 'getClass', 'get_class', 'GetType', 'typeid', 'typeof', 'type-of', 'type', 'ref', 'generator', 'meta', 'object_program', 'getAllegedType'], ['dir', ', methods', 'get_class_methods', 'getMethods', 'get-member', 'indices', 'o.meta.getmethods', 'slotNames', 'o.GetType().GetMethods()', 'o class selectors / o class allSelectors', 'o.__getAllegedType().getMessageTypes()'], ['class child(parent):', ', child :<; parent', 'class child : parent', 'class child <; parent end', 'class child is parent { ... }', 'class child extends parent', 'class child inherit parent end', 'parent subclass: child', 'make parent ...', 'inherit', 'def child extends makeSuperObject(parent, ...) { ... }', 'type child is new parent with record ... end record', '<pre>type child =  inherit parent  ...</pre>', '(defclass child (parent) ...)', '@interface child : parent { ... } ... @end', '@ISA = qw(parent1 parent2)', 'clone , setProtos, setProto, prependProto, appendProto', 'instance Parent Child where ...'], ['hasattr(obj, "meth")<a href="#67">(67)</a>', ', can', 'respond_to?', 'respondsTo', 'respondsToSelector', 'object->;method', "all [in object 'method function? get in object 'method]", 'find-method', 'ismethod', 'hasSlot', 'try obj.GetType().GetMethod("meth") with ...', 'obj.meth? instanceof Function', 'method_exists'], ['first parameter<a href="#68">(68)</a>', ', this', 'THIS', 'self', 'object_name if defined as: def object_name { ... }', 'Current', 'the object variable', 'dispatching parameter', 'Me', '.'], ['super(Class, self).meth(args)', ', super', 'base', 'resend', 'Precursor', '$o.SUPER::method(...)', '$o->;SUPER::method(...)', 'method(parent(dispatching-parameter))', 'parent(dispatching-parameter).method', 'parent::method', 'call-next-method', '<pre>type foo2 =  inherit foo as parent  ...  member ... = ... parent.meth</pre>'], [', inner'], [', .', ':', '::', ': ::', ':-', "'", '`', '__', '/'], ['automatically done based on the file name', ', package p;', 'namespace p { ... }', 'namespace p ...', 'namespace P;', 'namespace eval p ...', 'module p where ...', 'module P ... end', 'module P = struct ... end', '{ module "p"; ... }', ':- module(p)', 'p = module() ... end module', '(defpackage p ...)', 'package declare', 'Begin["p`"] ... End[]', 'BeginPackage["p`"] ... EndPackage[]', '<;node xmlns="namespace">; ... <;/node>;', '<pre>package p is   -- Declare public package members hereprivate   -- Declare private package members hereend p;package body p is ... -- Define package implementation hereend p;</pre>', 'package p'], ['__all__ = [ ... ]', ', module p (name1, name2, ...) where ...', '@ISA = qw(Exporter); @EXPORT = qw(name1 name2 ...);', 'package p is ... end; package body p is ... end;', 'p = module() export name1, name2, ...; ... end module', "(export 'name1 'name2)", 'attached to each name (public, private...)', 'namespace export name1', '<pre>namespace p  val name1 : type1  ...</pre>', 'append_features', '<pre>module type PType = sig val name1 : type1 ... endmodule P : PType  = struct ... end</pre>', 'all files in package directory are exported. files in /private sub-directory are not exported, but can be used by the package itself', 'Identifier is only exported if the first character of its name is an Unicode upper case letter; and the identifier is declared in the package block or it is a field name or method name.  No other identifiers are exported'], ['from p import *', ', use p', 'uses p', 'using p', 'using namespace p;', "(use-package 'p)", 'open p', 'import', 'import p', 'IMPORT p;', 'import p.*', 'import "p"', 'with p; use p;', 'namespace import p *', 'inherit c export {NONE} all end', 'include or even extend', 'do', 'addpath', '. p', 'source p', 'builtin -f /path/to/lib.so', '<;<; p`', 'Get["p`"]', 'Needs["p`"]', 'use P1\\P;', 'use P1\\P as Q;'], ['from p import name1, name2, ...', ', import p (name1, name2, ...)', 'import p.name1; import p.name2', "(import '(p:name1 p:name2))", 'use p qw(name1 name2 ...)', 'FROM p IMPORT name1, name2, ...;', 'namespace import p name1', 'using p::name1; using p::name2; ...', 'with p; use type p.type1; ...', 'with(p[name1, name2,])', 'def name := <;import:p.name>;', ':- use_module(name1, name2, ...)'], [', import p', 'use p;', 'require p', 'require "p"', 'require, "p"', "(require 'p)", 'with p;', 'with(p)', 'package require p', 'automatically done', 'import "p"'], ['string', 'str', ', char[]', 'char const[]', 'string', 'string!', 'String', 'STRING', 'Str', 'NSString *', 'CHAR, VARCHAR(size)', 'Sequence'], ['char', ', char', 'char!', 'Char', 'Character', 'CHARACTER', 'rune'], [", 'z'", '"z"', '"z', '$z', '#\\z', '#"z"', '&amp;z', '?z', 'char z, [char] z'], ['"""..."""', '"...', '[[ ... ]]', 'R"[ ... ]"', "<;<;'MARK' ... MARK", "<;<;<;'MARK' ... MARK", '{...{...}...}', '(...)', 'q(...(...)...), q[...], q{...}, q<;...>;, q/.../', '%q(...(...)...), %q[...], %q{...}, %q<;...>;, %q/.../', 'q(...(...)...)', '@"...""..."', 's"..."', '@"..."'], ['"... %(v)s ..." % vars()', ', ...', '"... $v ..."', '"... {v} ..."', '"... #{v} ..." "... #$v ..." "... #@v ..." "... #@@v ..."', '<;<;MARK ... $v ... MARK', '<;<;MARK ... #{v} ... MARK', '<;<;<;MARK ... $v ... MARK', 'qq(...(... $v ...)...), qq[...], qq{...}, qq<;...>;, qq/.../', '%Q(...(... #{v} ...)...), %Q[...], %Q{...}, %Q<;...>;, %Q/.../', 'qq(...(... {v} ...)...)', '"... #{v} ..." interpolate'], ['"\\n"', ', \\n', '"*n"', '"%N"', '"^/"', '"~%"', '"[lf]"', 'vb_nl', '<;N>;'], ['\'\'\'...\'\'\', """..."""', ', all strings allow multi-line strings', '"...", {...}', '@"..."', '[[ ... ]]', '{...}', '<pre>"...\\n""...\\n"</pre>', '<pre>... "...\\n\\    \\...\\n"</pre>', '<pre>"...","..."</pre>', '<pre>"...%N%%...%N"</pre>', '""" ... """', '`...`'], ['string', 'str, `e`, repr', ', show', 'to_s, to_str, inspect, String()', 'to_string', 'tostring', 'toString', 'ToString', 'String', 'perl', 'Dumper', '"" . e', '"" ~ e', '"" + e', 'string', 'out', 'cvs', "T'Image(e)", 'asString', 'printString', 'as(<;string>;, e)', '(string) e', 'convert(e,string)', "(coerce e 'string)", 'prin1-to-string', 'to string! / to-string / to ""', 'description', 'pr1', 'unneeded, all values are strings', 'string(e)'], ['pickle.dump<a href="#81">(81)</a>', ', export-clixml', 'serialize', 'Marshal.to_string', 'Marshal.dump', 'Data.Binary.encode', 'BinaryFormatter.Serialize', 'storeBinaryOn', 'Storable::store', '(with-standard-io-syntax (write obj stream))', "T'Output"], ['pickle.load', ', import-clixml', 'unserialize', 'Marshal.from_string', 'Marshal.load', 'Data.Binary.decode', 'BinaryFormatter.Deserialize', 'readBinaryFrom', '(with-standard-io-syntax (read obj stream))', 'Storable::store', 'doString', "T'Input"], ['printf', '%', ', sprintf', 'printf', 'format', 'format', 'Format', 'putFormat', 'stringWithFormat', 'expandMacrosWith', 'Storable::retrieve', 'fmt.Sprintf'], [', puts', 'print', 'write', 'putStr', 'print_string', 'console', 'writeln', 'write-string', 'putStrLn', 'Put_Line', 'display', 'message', 'put_string', 'show', 'print_endline', 'println', 'put_chars', 'echo', 'type', 'putText', 'say', 'p or i', 'fmt.Print', 'echom'], [', print', 'say', 'puts', 'puts -nonewline'], ['p', 'print e,', ', print', 'Print', 'println', 'prin', 'Put', 'p', 'puts', 'display', 'write', 'writeln', 'print', 'printOn', 'princ prin1', 'print_any', 'WriteLine', 'nothing - just remove ";" at the end of the expression, and it will print it', 'disp'], [', printf', 'write', 'WriteLine', 'putFormat', 'format', 'fmt.Printf'], ['= !=', '== &lt;&gt;', ', eq ne', 'strcmp', '== !=(Vimscript: whether or not == and != are case-sensitive depends on user settings.)', '==? !=?', '==# !=#', '== !==', '== ~=', '= \\=', 'isEqualToString', '== /=', '== \\=', '= !=', '= /=', '= \\=', '= <;>;', '= ~=', '== \\== or = <;>; \\=', '=== =!= / == !=', '== ~=', 'equal?', 'equals', 'equal, equalp', 'is_equal', 'isEqual'], ['length', 'len', ', length', 'LENGTH', "'Length", 'length?', 'size', 'Length', 'strlen', 'string length', 'string-length', 'StringLength', 'sizeof', 'count', 'bytes chars', 'CHARACTER_LENGTH', 'atom_length', 'wc -c', '#', '${#v}', 'dup'], ['cat', ', +', '.', '..', ',', '~', '&amp;', '^', '_', '||', '++', '$a$b', 'concatenate', 'string-append', 'StringJoin', 'cat', 'Cat', 'strcat', 'concat', 'append', 'stringByAppendingString', '[string1 string2]', 'word'], [', *', 'x', 'times', 'repeat', 'repeated', 'str_repeat', 'string repeat', 'strrep', 'repmat', 'insert/dup', 'COPIES', 'cat(s$n)', 'concat $ replicate', 'strings.Repeat'], ['toupper / tolower', 'upper / lower<a href="#90">(90)</a>', ', upcase / downcase', 'uc / lc', 'toUpper / toLower', 'to_upper / to_lower', 'To_Upper / To_Lower', 'toUpperCase / toLowerCase', 'upper_case / lower_case', 'uppercase / lowercase', 'strupper / strlower', 'strtoupper / strtolower', 'ToUpper / ToLower', 'toupper / tolower', 'string toupper / string tolower', 'asLowercase / asUppercase', 'upCase / lowCase', 'uppercase form / lowercase form', 'char-upcase / char-downcase', 'char_type(C_, to_upper(C)), char_type(C_, to_lower(C))', '\\U / \\L / \\C', 'unicode.ToUpper / unicode.ToLower'], ['upper / lower', 'upper / lower / capitalize', ', upcase / downcase', 'upper / lower', 'uppercase/lowercase', 'upcase_atom/downcase_atom', 'toUpperCase / toLowerCase', 'ToUpperCase / ToLowerCase', 'ToUpper / ToLower', 'to_upper / to_lower', 'toupper / tolower', 'uc / lc', 'UpperCase / LowerCase', 'StringTools[UpperCase] / StringTools[LowerCase] / StringTools[Capitalize]', 'uppercaseString / lowercaseString / capitalizedString', 'UCase / LCase', 'strtoupper / strtolower', 'strupper / strlower', 'string toupper / string tolower / string totitle', 'string-upcase / string-downcase', 'asLowercase / asUppercase / asUppercaseFirst', 'asLowercase / asUppercase / makeFirstCharacterUppercase', 'upcase_atom / downcase_atom', 'makeLC / makeUC', 'parse upper var in_var out_var / parse lower var in_var out_var', 'strings.ToUpper / strings.ToLower / strings.Title'], [', chr', 'chr$', 'char', 'format %c $c', 'toChar', 'strchar', 'from_integer', 'fromCharCode', 'FromCharacterCode', 'character', 'Character value: c', 'asCharacter', 'code-char', 'integer->;char', "'Val", '(char) c', 'to char! / to-char', 'X2C, D2C', '$CHAR(s)', 'char_code', 'ascii', 'StringTools[Char]', 'utf8.DecodeRuneInString(s)'], [', ord', 'asc', 'getNumericValue', 'charCodeAt', 'asciiValue', 'code', 'char-code', 'char->;integer', 's[0]', 's 0 get', 's at(0)', 'scan $s %c', 'strbyte', 'toInteger', "'Pos", 'number', '(int) c', 'to integer! / to-integer', 'ToCharacterCode', 'C2X, C2D', '$ASCII(s)', '(done automatically when applying mathematical operations on char, such as +)', 'char', 'char_code', 'StringTools[Ord]', 'string(c)'], [', s[n]', 's(n)', 's:n', 's.[n]', 's !! n', 's @ n', 's/:n', 'string index s n', 'sub', 'char, aref, schar, svref', 'GetChar', 's at(n)', 'at', 'aref', 'char(s, i)', 'charAt', 'characterAtIndex', 'n ->; s.inxGet', 'string-ref', 'StringTake[s, {n}]', '$EXTRACT(s, n)', 'item', 'over n chars + c@', 's/.{n}(.).*/\\1/'], ['substring', 's[n:m+1]', ', s[n..m]', 's.[n..m]', 's(n..m)', 's(n:m)', 's(n,m+1)', 's[n,len]', 's n len', 'strndup(s + n, len)', 'substring', 'Substring', 'substr', 'SUBSTR', 'sub', 'SUB', 'subseq', 'slice', 'mid$', 'string range', 'StringTake[s, {n, m}]', 'strpart(s, n, m)', 'copy/part at s n len', 'copy/part at s n at s m', 's copyFrom: n to: m', '(n,m)->;s.sub', '[s substringWithRange:NSMakeRange(n, len)]', 'SUBSTRING(s FROM n len)', '$EXTRACT(s, n, m)', 'sub_string / sub_atom', '(take len . drop n) s', 'over n chars + len', 's/.{n}(.{len}).*/\\1/'], ['find', 'find', 'indexOf', 'IndexOf', 'indexOfString', 'startOf', 'search', 'StringTools[Search]', 'StringPosition', 'strstr strchr', 'findSeq', 'findSubstring', 'strfind', 'strpos', '$FIND', 'index_non_blank / find_token', 'substring_index', 'rangeOfString', 'POS', 'POSITION(needle IN s)', 'sub_string / sub_atom', 'string first', 'strings.Index'], ['rfind', 'rfind', 'find/last', 'strrchr', 'index(Going =>; Backward)', 'lastStartOf', 'lastIndexOf', 'last_index_of', 'LastIndexOf', 'lastIndexOfString', 'string last', '(search substring string :from-end t)', '[string rangeOfString:substring options:NSBackwardsSearch]', 'LASTPOS', 't=strfind(s,p), t(end)', 'strrpos', 'StringTools[SearchAll](s,p)[-1]', 'strings.LastIndex'], ['bool', 'bool', ', Bool', 'Boolean', 'boolean', 'BOOLEAN', 'logic!', 'logical'], ['n', '{}', ', false', 'FALSE', 'false()', '#f', 'n', 'nil', 'no', 'No', 'none', 'null', 'NULL', 'off', 'undef', 'undefined', 'fail', 'FAIL', 'array containing at least one false value', 'exit status different from 0', '0.0', 'NaN', '"0"', "''", "'\\0'", 'array()'], ['t', 'anything not false', ', TRUE', 'true', 'true()', 't', '#t', 'y', 'yes', 'Yes', 'on', 'exit status 0', '1', 'non zero number', 'non-zero-numbers', '-1'], ['not', 'not<a href="#94">(94)</a>', ', !', 'Not', 'NOT', '~', '^', "'", '\\', '=0'], ['| / &amp;', 'or / and', ', || / &amp;&amp;', '| / &amp;', 'OR / AND', 'or / &amp;', 'any / all', 'orelse / andalso', 'orelse / andthen', 'or else / and then', '; / ,', '&amp; / !'], ['or / and', ', | / &amp;', 'or / and', 'Or / And', '\\/ / /\\', '?| /'], ['list', 'list', ', seq', 'a list', '[a]', 'a[]', 'List', 'Array or List', 'ARRAY or LINKED_LIST', 'Array or OrderedCollection', 'ARRAY', 'array', 'cell', 'vector', 'Containers.Vectors.Vector or Ada.Containers.Doubly_Linked_Lists.List', '[]a'], [',', ', +', ',', '@', '~', '&amp;', '++', '|||', 'array_merge', 'merge', 'concat', 'concatenate', 'nconc', 'append', 'Append', 'appendSeq', 'arrayByAddingObjectsFromArray', 'sentence', 'Join'], ['flatten', ', concat', 'flatten', 'Flatten', 'eval concat', 'ListTools[FlattenOnce]', '{*}$l', '"$l"'], [', flatten', 'ListTools[Flatten]', 'Flatten'], ['{ a, b, c }', ', [ a, b, c ]<a href="#97">(97)</a>', '( a, b, c )', '{ a, b, c }', '#(a, b, c)', '#(a b c)', '{ a. b. c }', '[ a ; b ; c ]', '[ a b c ]', '({ a, b, c })', "'(a b c)", '<;<; a, b, c >;>;', 'list(a, b, c)', 'list', 'array(a, b, c)', 'new t[] { a, b, c }', 'new[] { a, b, c }', 'new List<;t>; { a, b, c}', 'Array(a, b, c)', '[NSArray arrayWithObjects:a, b, c, nil]', 'set l a b c', '<pre>  - a  - b  - c</pre>', '[]t{a, b, c}'], ['at', ', a[i]', 'a*[i] or a!i or a*(i) depending on the version', 'a[[i]]', 'a[i]:default', 'a(i)', 'a:i', 'a/:i', 'a.(i)', 'a.[i]', 'a !! i', 'a @ i', 'a i get', 'a at(i)', 'at', 'lindex', 'nth', 'Nth', 'aref', 'nth0 / nth1', 'list-ref / vector-ref', 'element', 'slice', 'node[i]', 'objectAtIndex', 'item', 'a i cells + @ (for write access: o a i cells + !'], [', :', '::', '|', '[ e | l ]', '[e l[]]', '[e l]', 'cons', 'pair', 'fput', 'Prepend'], [', unshift', 'prepend', 'push_front', 'addFirst', 'insert', 'put_first', 'push', 'array_unshift', 'PrependTo'], [', linsert l i e', 'Insert'], ['a.insert(i, e)', ', [a insertObject:e atIndex:i]', 'a insertAt(e, i)', 'a add: e beforeIndex: i / a add: e afterIndex: i', 'splice(@a, $i, 0, $e)'], [', [l e]', 'push', 'arrayByAddingObject', 'lput', 'linsert l end e', 'Append'], ['append', ', push', 'push_back', 'AppendTo', 'lappend', '+=', 'add', 'put_last', 'array_push', 'addObject'], ['head', ', head', 'Head', 'hd', 'car', 'first', 'First', 'First_Element'], [', head', 'begin', 'First'], ['l[1:]', ', tail', 'Tail', 'tl', 'cdr', 'Rest', 'butfirst', 'allButFirst', 'a(2:end)', 'L = [_|ButFirst]', 'lrange l 1 end'], ['a[-1]', ', last', 'Last', 'lastObject', 'a(end)', 'node[last()]', '(car (last l))', 'lindex l end', 'Last_Element', 'l[len(l)-1:]'], [', Last'], [', Most'], [', shift', 'shift!', 'pop', 'removeFirst', 'array_shift'], [', pop', 'pop!', 'array_pop', 'removeLast', 'dequeue'], ['forall', 'for v in l: ...', ', each', 'for v in l ...', 'for v in l; do ...; done', 'for v in l do ...', 'for v in l; ...; end', 'for (v in l) ...', 'for (var v in l) { ... }', 'For Each v in l...Next', 'for v in range loop .. end loop', 'for', 'foreach', 'foreach (t v in l) ...', 'foreach (v in l) ...', 'foreach ($v in l) ...', 'foreach(t v, l, { ... })', 'l foreach(v, ...)', 'for_each', 'for-each', 'forall', 'ForAll', 'iter', 'do', 'do_all', 'app', 'mapc', 'mapM_', 'Scan', '(dolist (v l) ...)  (loop for v in l do ...)  mapc', 'list.iterate (# do current ... #)', 'l.Iterate(...)', 'Iterate', 'for i, v := range l {...}'], ['[ f(x) for x in l ]<a href="#104">(104)</a>', 'Map', 'mapcar', 'maplist', 'sublist', 'map / map.se', 'for-each', 'foreach or selected', 'collect', 'transform', 'array_map', '/@', '[ f x | x <;- l ]', 'magical: sin(x) computes sin on each element'], ['map', ', map2', 'zipWith', 'Zip', 'map.se', 'mapcar', 'maplist2', 'l1 with: l2 collect: ...', 'transform', 'ListPair.map', 'magical: a binary function or operator is appliied on each element'], ['index', ', find', 'Find', 'find_if', 'find-if', 'first', 'detect', 'search', 'ListTools[Search]', 'lsearch -exact', 'indexOf', 'indexOfObject, indexOfObjectIdenticalTo', 'find(a == 3)', 'Position'], ['[ x for x in l if p(x) ]<a href="#104">(104)</a>', ', find_all', 'filter!', 'Filter', 'grep', 'where', 'select', 'Select / Case', 'selectInPlace', 'remove-if-not delete-if-not', 'choose', 'array_filter', '[ x | x <;- l, p x ]', 'a(a == 3)'], [', remove-if delete-if', 'reject'], [', partition', 'partition!', 'Partition'], [', break', 'span'], [', split-sequence', 'ListTools[Split]'], [', group', 'Split'], [', groupBy', 'Split'], ['member', 'in', ', member?', 'include?', 'mem', 'member', 'Member', 'MemberQ', 'memq memv', 'memberp / member?', 'contains', 'containsObject', 'in_array', 'includes', 'elem', 'has', 'has_value', 'ismember', '/elt/'], ['exists', ', any<a href="#31">(31)</a>', 'any?', 'anySatisfy', 'exists', 'exists?', 'some', 'Some', 'ormap', 'detect'], [', all<a href="#31">(31)</a>', 'All', 'all?', 'allSatisfy', 'every', 'every?', 'for_all', 'andmap'], [', min / max', 'Min / Max', 'minimum / maximum', 'minimum-of / maximum-of', 'min minstr / max maxstr', 'min_element / max_element'], ['s.join(l)', ', join(s, l)', 'String.Join(s, l)', 'l.join(s)', 'l asStringWith: s', 'join l s', 'implode(s, l)', 'ListTools[Join]', 'rjoin', 'join', 'concat', 'strcat', 'concat_atom', 'l * s', "(mapconcat 'identity l s)", 'componentsJoinedByString', 'intercalate', 'StringJoin @@ Riffle[l, s]', 'strings.Join'], ['length', 'len', ', size', 'sizeof', 'length', 'Length', 'length?', 'llength', '$LENGTH', 'elems', 'getn', 'count', 'numel', 'scalar @l', 'nops', '#', 'len(l)'], ['enumerate(l)', ', each_with_index', 'foreach($l as $i =>; $v)', 'a foreach(i, e, ...)', 'for i =>; v in l', 'for (v in l, i from 0) ... end', 'forAllInd', 'foreachi', 'foreach(l; typ0 i; typ1 v) { ... }', 'withIndexDo', 'iteri', 'IterateIndexed', 'MapIndexed', '(loop for v in l as i upfrom 0 do ...)', 'for v,i in l ...', 'for i, v := range l {...}'], ['set', ', uniq', 'uniq!', 'uniq2', 'unique', 'nub', 'array_unique', 'ListTools[MakeUnique]', 'delete-duplicates', 'delete-duplicates!', 'remove-duplicates', 'lsort -unique', 'toset', 'distinct', 'Union'], ['sorted', 'sort!', 'Sort', 'sort_by', 'sortBy', 'order by', 'lsort', 'asort', 'sort-object', 'sortedArrayUsingSelector, sortUsingSelector', 'predsort / keysort / mergesort'], ['l[::-1]', 'Reverse', 'reverse_copy', 'rev', 'Reverse_Elements', 'lreverse', 'array_reverse', 'ListTools[Reverse]', 'fliplr flipud...'], ['transpose', 'zip', ', combine', 'pairlis', 'transpose', 'Transpose', '[a b]'], ['unzip', 'zip(*l)', ', split', 'unzip', 'unzip2', 'transpose', 'Transpose', 'a(1,:), a(2,:)'], [', lookup', 'assoc', 'assq', 'assv', 'get_assoc', 'select', 'a.(e)', 'a[e]', 'gprop', '/.'], ['list', ', to_a', 'toArray', 'asArray', 'to_list', 'map-as(<;list>;, bag)', '[a.(:)]', 'array get'], ['fold', 'reduce<a href="#109">(109)</a>', ', foldl', 'FoldL', 'fold_left', 'fold', 'Fold', 'inject', 'inject into'], [', foldr', 'FoldR', 'fold-right', 'fold_right', 'foldBack', 'rreduce', "(reduce f '(e1 e2 ... en) :from-right t :initial-value init)", 'reverseReduce'], ['tuple', ', typ1 * ... * typn', '(typ1, ..., typn)', 'typ1, ..., typn', 'tuple!', 'Tuple[Typ1, Typ2, Typ3]', 'tuple<; typ1, ..., typn >;'], [', a, b, c', '( a, b, c )', '{ a. b. c }', '{ a, b, c }', '[ a, b, c ]', 'a . b . c', '(cons a b)'], [', ()', '[]', '{}', '#()', 'Nothing'], ['tuple([a])', ', a or [a]', '(a)', '((a))', '{a}'], ['*t', ', t', 't{:}', 'f @@ t', 'L =.. [ F | Args ], call(L)'], [', &amp;', '\\', 'AddressOf', 'addr', '@', 'lv', 'ref', 'newSTRef', 'NewCell', 'variable', "'access", ':>; :>;>;', "''"], [', *', '$ @ % &amp;', '->;[...] ->;{...} ->;(...)', '->;', '^', '!', 'rv', 'readSTRef', 'Access', '.[all]', '@', 'eval', '(reg)'], [', writeSTRef', 'Assign', ':=', '!'], ['null', 'None', ', 0', '0 nullptr', 'NULL', 'nil', 'null', 'Null', 'undef', 'NONE', 'Nothing', 'Void', '#f ()', '(empty) / ~ / null'], ['some v', ', v', '*v', 'Just v', 'Some v', 'SOME v'], [', option', 'Maybe'], ['or', ', COALESCE', '?:', '||', '//', '??', 'if(a, b)'], [', struct { typ1 n1; typ2 n2; ... }', 'type typ = { n1 : typ1; n2 : typ2 }', 'data Typ = N0 { n1, n2 :: typ1, n3 :: typ3, ... }', '<pre>type Typ is record  N1 : Typ1;  N2 : Typ2 := default_val;  ...end record;</pre>', 'type typ struct{ n1 typ1; n2 typ2; }'], [', .', '::', '%', "'", '^', 'r { field }', 'r:field', 'field r', '->;', 'r[field]', 'r["field"]', '#field r', 'normal function call'], [', union { typ1 n1; typ2 n2; ... }', 'data Typ = N1 typ1 | N2 typ2 | ...', 'type typ = N1 of typ1 | N2 of typ2 | ...', 'datatype typ = N1 of typ1 | N2 of typ2 | ...', '<pre>type Typ (Choice : Discrete_Type) is record  case Choice is     when Choice_1 =>;         N1 : Typ1;         ...     when Choice_2 | Choice_3 =>;         ...     when others =>;        ...  end case;end record;</pre>'], [', enum typ { n1; n2; ... }', '<pre>Enum typ   n1   n2End Enum</pre>', '(n1, n2, ...)', 'type typ is', 'data Typ = N1 | N2 | ...', 'type typ = N1 | N2 | ...', 'datatype typ = N1 | N2 | ...'], ['hash', 'dict', ', map', 'Map', 'std::map', 'Dictionary', 'dictionary', 'Hash', 'HASH', 'HASH_TABLE', 'HashTable', 'Hashtbl', 'Hashtbl.t', 'struct', 'table', 'array', 'Data.Map, Data.HashTable', 'Containers.Ordered_Maps.Map'], ['{ a: b, c: d }', ', [ a =>; b, c =>; d ]', 'array( a =>; b, c =>; d )', '{ a =>; b, c =>; d }', '{ a, b, c, d }', '{ a: b; c: d }', '$[ a: b, c: d ]', '{ a->;b. c->;d }', '{ a = b, c = d }', '@{ a = b; c = d }', '([ a:b, c:d ])', '([a]=b [c]=d)', '<;<; a b c d >;>;', 'struct(a, b, c, d)', 'Hash[ a, b, c, d ]', 'Map.of_list [a, b; c, d]', 'Hashtbl.of_list [a, b; c, d]', 'table([a=b, c=d])', 'define table foo a =>; b; c =>; d end', 'dict create a b c d', 'new t { {a, b}, {c, d} }', 'fromList', '[NSDictionary dictionaryWithObjectsAndKeys:b, a, d, c, nil]', '<pre>  a: b  c: d</pre>', 'map[typ0]typ1{ a: b, c: d }'], [', h[k]', '$h{k}', '%h{k} or %h<;s>;', 'h(k)', 'h.[k]', 'h.k', 'h:k', 'h["k"] or h->;k', '(gethash k h)'], ['h.get(k, returned_value_when_k_unfound)', ', h k get', 'find', 'fetch', 'get', 'dict get', 'at', 'h@k or h.at(k)', 'h[k]:default', '${h[k]}', 'objectForKey', 'lookup', 'Element'], [', h k o put', 'put', 'add, replace', 'store', 'dict set', 'h[k]', 'atPut', 'h at: k put: o', '[h setObject:o forKey:k]', 'insert', 'Replace_Element'], ['in', 'k not in h', ', exists $h{k}', 'exists', 'dict exists', 'has', 'haskey', 'hasKey', 'has_key?, include?, key?, member?', 'Contains', 'containsKey', 'includesKey', 'in', 'mem', 'member', 'isfield', 'find', 'h[k]', '(gethash k h)', 'maps', 'known', 'isset(h[k]), array_key_exists(k, h)', 'v, exists := h[k]'], ['remove', 'del h[k]', ', delete $h{k}', 'unset(h[k])', 'remove', 'Remove', 'dict remove', 'removeAt', 'removeKey', 'remhash', 'delete', 'Delete', 'erase', 'm_delete', 'removeObjectForKey', 'undef', 'rmfield', 'delete(h, k)'], [', keys', ', keys', 'dict keys', 'keySet', 'allKeys', 'AllKeys', 'indices', 'current_keys', 'getKeys', 'array_keys', 'fieldnames', 'findall(Key, item(Key, _), Keys)', '${!h[@]}'], [', values', 'dict values', 'getValues', 'content', 'array_values', 'struct2cell', 'entries', 'elems', '${h[@]}'], ['update<a href="#121">(121)</a>', ', merge', 'array_merge', 'union', 'putAll', 'insert', '(%h1, %h2)'], ['range', ', a .. b', 'a:b', '[ a .. b ]', 'to', 'seq a b / jot - a b', '{a..b}', 'range', 'range(from: a, to: b, by: step)', 'Range', 'Range with', 'List.number A B Step', 'numlist / between', 'iseq', 'k, v := range h'], ['range', ', a ... b', 'a ..! b'], ['int', 'int, long<a href="#124">(124)</a>', ', short, int, long', 'int', 'Int', 'Int, uInt, Int8, Int16...', 'integer', 'INTEGER, INT, SMALLINT', 'INTEGER, INTEGER_8, NATURAL_8...', 'int8, uint8, int16, uint16, ...64', 'int8_t, uint8_t, int16_t, uint16_t, ...64', 'int, int8, uint8, int16, uint16, int32, uint32, int64, uint64, bigint, bignum', 'Int, Integer, Int8, Int16, Int32, Int64', 'Integer, FixNum, BigNum', 'Integer, SmallInteger, LargeInteger', 'Integer', 'type T is range Low...High;', 'number'], ['float', 'float, decimal.Decimal', ', float, double, long double', 'float, double', 'float', 'Float', 'float, float32', 'Float, Float32, Float64', 'NUMERIC, DECIMAL, DOUBLE PRECISION', 'Rat', 'DOUBLE, REAL', 'single, double', 'Float, Double, Ratio', 'Float, Double, Fraction, FixedPoint', 'Real, Rational', 'Number', 'type T is digits N range Low..High;', 'type T is delta S digits N range Low..High;', 'float32, float64'], [', 1000', '1000, 1000.', '1000, 1000., 1000.0', "1000, '1000'D", '1000, 1E3'], ['07, 0xf', ', 0b1, 07, 0xf', '0b1, 0o7, 0xf', '0xf', '07', '0o7, 0xf', '1b', '2#1#, 8#7#, 16#f#', '2#{1}, #{F}', '#b1, #o7, #xf', '2^^1, 8^^7, 16^^f', '2r1, 8r7, 16rf', '#2r1, #8r7, #16rf', '1b, Fh', "'1'B, 'F'X", "B'1', X'F'", '$f'], [', 1_000, 10_00, 100_0', "1'000, 10'00, 100'0", '1_000'], [', 1000., 1E3', '1000., 1E3, 1,0', '1000., 1.E3', '1000.0, 1E3', '1000.0, 1.0E3', '1000, 1000.0, 1E3', '1000., 1*^3, 1000`', '1000e, 1e3'], [', + / - / * / /', '+ / - / * or nothing / /', '+ +. / - -. / * *. / / /.', 'sum / difference / product / quotient', 'add / sub / mul / idiv div', 'f+ / f- / f* / f/'], ['^', 'pow', '^', '*', '**, ^', '^^', '**, ^ and ^^', 'f**', 'Pow', 'power', 'exp', 'expt', 'raisedTo', 'math.Pow'], [', -', '- -.', '~', 'neg', 'negate', 'fnegate', 'minus'], ['random', 'random', ', rand', '$RANDOM', 'randomR', 'Random', 'Random.int', 'Random, RandomReal, RandomInteger', 'Random value', 'Random new nextInteger', '<pre>r: RANDOMcreate r.maker.startr.item</pre>', '<pre>Random ran = new Random();ran.Next(...);</pre>', '<pre>let r = System.Random()r.Next()</pre>', 'mt_rand', 'rand.Read'], ['random.seed', ', srand', 'set_seed', 'Random setSeed', 'Random.init, Random.self_init', "rand('state',...)", 'rerandom', 'RandomTools[MersenneTwister][SetState]', 'Random new setSeed', 'SeedRandom', 'mkStdGen', 'make-random-state', 'Reset', 'rand.Seed'], [', mathematical', 'same priorities'], [', mathematical', 'negation first'], ['/ exp / abs', ', sqrt / exp / abs', 'sqrt realsqrt / exp / abs', 'sqrt / exp /', 'Sqrt / Exp / Abs', 'sqrt / / abs', 'Sqrt / / ABS', '/ exp / abs', 'sqrt / /', 'square-root / exp / abs or absolute', 'Sqrt / Exp / ABS', 'sqrt,isqrt / exp / abs', 'fsqrt / fexp / abs,fabs', 'math.Sqrt'], [', sin / cos / tan', 'Sin / Cos / Tan', 'sin / cos /', 'sine / cosine / tangent', 'radsin / radcos / radtan', 'fsin / fcos / ftan', 'math.Sin / math.Cos / math.Tan'], ['arcsin / arccos / arctan', ', asin / acos / atan<a href="#134">(134)</a>', 'Asin / Acos / Atan', 'ASin / ACos / ATan', 'arcsin / arccos / arctan', 'arcSin / arcCos / arcTan', 'ArcSin / ArcCos / ArcTan', 'arcsine / arccosine / arctangent', 'arc_sine / arc_cosine / arc_tangent', ' / / atan', ' / / radarctan', 'fasin / facos / fatan', 'math.Asin / math.Acos / math.Atan'], ['log', 'log', ', ln', 'Log', 'log 10', 'log-e', 'fln', 'math.Log'], [', log10', 'Log10', 'log', 'log: 10', 'log-10', 'log[10]', 'Log[10, val]', 'logBase 10', 'Log(X =>; val, Base =>; 10.0)', '(log x 10)', 'flog', 'math.Log10'], ['log(val, 2)', ', log2', 'log-10 / log-2', 'Log(X =>; val, Base =>; 2.0)', 'Log[2, val]', 'frexp', 'math.Log2'], [', divmod', 'divMod', 'div ldiv lldiv', 'IntInf.quotrem', 'floor', '/mod', 'Div'], ['mod', ', %', '%%', '\\\\', 'mod', 'Mod', 'MOD', 'modulo', 'rem'], [', %', '#', 'mod', 'remainder', 'rem', '//', '\\\\'], ['/ round / floor / ceil', 'int / round / floor / ceil', ', trunc / round / floor / ceil', 'truncate / round / floor / ceiling', 'int / round / /', 'to_i, Integer() / round / floor / ceil', 'TRUNC / FORMAT / Floor / Ceil', "Float'Truncation / Float'Rounding / Float'Floor / Float'Ceiling", '/ round / floor / ceil', '/ Round / Floor / Ceiling', '/ Round / Floor / Ceil', '/ round / floor / ceiling', '/ ROUND / FLOOR / CEILING', '/ rounded / floor / ceiling', 'int / / floor / ceil', 'int_of_float / / floor / ceil', ' / / floor / ceil', 'IntegerPart / Round / Floor / Ceiling', ' / Rounding / Floor / Ceiling', 'to-integer / / /', '/ fround / /', 'math.Trunc / / math.Floor / math.Ceil'], ['&amp; / |', ', &amp; / | / ^', '&amp; / | / ~', '&amp; / |', '+&amp; / +| / +^', '.&amp;. / .|. / xor', '&amp;&amp;&amp; / ||| / ^^^', 'and / or / xor', 'land / lor / lxor', 'logand / logior / logxor', 'bitand / bitor / bitxor', 'BITAND / BITOR / BITXOR', 'BitAnd / BitOr / BitXor', 'bitAnd / bitOr / bitXor', 'bitwiseAnd / bitwiseOr / bitwiseXor', '/\\ / \\/ / xor'], ['bitnot', ', ~', '~~~', 'not', 'lnot', 'lognot', 'bitnot', 'BitNot', 'complement', 'bitcmp', 'bitInvert', 'bitwiseComplement', 'invert', '\\'], ['bitshift', '&lt;&lt; / &gt;&gt;', ', <;<; / >;>; / >;>;>;', '<;<;<; / >;>;>;', '|<;<; / |>;>;', 'lsl / lsr or asr', 'bitshift', 'bitShift', 'ashift lshift', 'lshift / rshift', 'shiftL / / shiftR', 'shiftLeft / shiftRight', 'Shift_Left / Shift_Right / Shift_Right_Arithmetic / Rotate_Left / Rotate_Right', '(ash x positive-integer) / (ash x negative-integer) /'], [', class class_name(threading.Thread) {[override run method] }', 'task task_name is [entry entry_name[(parameter ...)]...] end task_name', 'task type task_type_name is [entry entry_name[(parameter ...)]...] end task_type_name', 'class class_name extends Thread {[override run method] }', 'thread ...', '<pre>parallel [threads nb_threads] [mini mini_threshold] [maxi maxi_threshold] [active]   ...   task     parallel_instructions   [post     sequential_instructions]   ...</pre>', '[NSThread detachNewThreadSelector:mainFunction toTarget:target withObject:obj]'], [', object t=Thread.Thread(f)', 'set t [thread create {code}]', 'Thread createThread(...)'], [', MyTask : task_type_name;', 'class_name MyThread = new class_name()', 'p :=  [ ... ] newProcess.', 'p :=  [ ... ] fork.'], [', start() / stop()', 'resume / suspend / terminate', 'Tasks are started when created / call Stop entry or "abort task-object-name"', 'thread send $t {script}'], [', call an entry with parameters', 'call any public method', 'common variables are copied at thread creation, in abscence of a "share" statement', 'use messages, parameters or shared variables'], [', select task_entry_call; or delay timeout_limit; end select;'], [', thread::mutex', '<pre>protected Object_Name is [entry entry_name(Parameter : [in out] is type [...]);procedure procedure_name(Parameter : [in out] is type [...]);function function_name return type;privateshared data declarationend Object_Name;</pre>', 'synchronize (this){ ... }', 'SharedQueue, Semaphore critical: [...], Future, LazyValue'], [', Object_Name.Entry_Name(Parms)<br/>Object_Name.Procedure_Name(Parms)', 'Object_Name.SetMethod(Parms)'], [', Object_Name.Function_Name', 'Object_Name.GetMethod()'], [', Objectg_Name.Entry_Name(Parms)'], ['OtherThread.join()', ', Call task entry serviced just before task termination'], [', Call a task entry on the other thread'], [', pragma Locking_Policy(Ceiling_Locking);'], [', pragma Priority(expression);'], [', Set_Priority(Priority_Value);', 'setPriority(newPriority);', 'p priority: n'], [', pragma Atomic(Object_Name);'], [', pragma Volatile(Object_Name);']]

#terms associated with words
terms = (
    ('length','get'),
    ('string','convert to'),
)

#stores possible cause of syntax error
offendingLine = ''

#grabs the original compiler error message
def getCompilerMessage(self):
    #get console output
    output = self.output_view.substr(sublime.Region(0, self.output_view.size()))
    #output original compiler error
    print("From compiler:")
    print(output)
    return output

#extracts the error message from the output (compiler error message)
def getErrorMessage(output):
    #split output by lines
    lines = output.split("\n")
    #check that output is error message
    #find where error message is amongst output
    if (len(lines) < 2):
        return -1
    lineNum = -1
    for line in lines:
        if "Error: " in line:
            lineNum = lines.index(line)
    #get line containing error type
    errorName = lines[lineNum]
    return errorName

#gets the type of the error message
def getErrorType(error):
    #extract error type
    endErrorName = error.find(':')
    if endErrorName != -1:
        return error[:endErrorName]
    return -1

#gets the 
def getErrorLine(error):
    #start of line number
    beg = '", line '
    #extract error line
    startErrorNum = error.find(beg) + len(beg)
    if (startErrorNum == -1):
        return -1
    endErrorNum = error[startErrorNum:].find('\n')
    #check is valid
    if ((startErrorNum != -1) or (endErrorNum != -1)):
        error = error[startErrorNum:]
        error = error[:endErrorNum]
        error = int(''.join(x for x in error if x.isdigit()))
        return error
    return -1

#gets the file name where the error originates
def getFileName(error):
    #start of file name
    beg = 'File "'
    startErrorNum = error.find(beg) + len(beg)
    if (startErrorNum == -1):
        return -1
    end = '", line '
    endErrorNum = error[startErrorNum:].find(end)
    #check is valid
    if ((startErrorNum != -1) or (endErrorNum != -1)):
        error = error[startErrorNum:]
        return error[:endErrorNum]
    return -1

#returns the specified files contents
def getCode(fileName):
    with open(fileName, 'r') as file:
        code = file.read()
        file.close()
        return code
    return ""

#preps the query to include necessary filters and meet URL format
def filterQuery(query):
    #ensure questsions have at least one answer
    query = "answers:1.. " + query
    #add python to the search
    if ("python" not in query):
        query = "[python] " + query
    #convert any spaces to '+'
    query = query.replace(" ","+")
    return query

#generates the url for the query
def getQuery(error):
    #get error type
    errorType = getErrorType(error)
    #remove quotation marks for specific errors
    if (errorType == "KeyError"):
        #check for quotation marks which will contain code specific data for specific error
        while ("'" in error):
            start = error.find("'")
            end = error[start+1:].find("'")+start+2
            error = error.replace(error[start:end],'')
        while ('"' in error):
            start = error.find('"')
            end = error[start+1:].find('"')+start+2
            error = error.replace(error[start:end],'')
    return urlBase+urlSearch+filterQuery(error)

#searches data from website or data types
def searchTranslate(word,lsts=None):
    word = word.lower()
    if lsts != None:
        #first check data types
        for i in range(0,len(pythonDataTypes)):
            if word in pythonDataTypes[i]:
                return pythonDataTypesEnglish[i]
        #thhen search through provided list
        for lst in lsts:
            if word in lst:
                return lst[0]
        #if no match, find containing
        for lst in lsts:
            for l in lst:
                if word in l:
                    return lst[0]
    else:
        for i in range(0,len(pythonDataTypes)):
            if word in pythonDataTypes[i]:
                return pythonDataTypesEnglish[i] 
    return None

#returns action word associated with input
def actionWord(search1=None,search2=None):
    if ((search1 == None) and (search2 == None)):
        return None
    try:
        tmpContent = open("pythonTasks.txt", 'rb').read().decode('utf-8', errors='ignore')
        tmpContent = tmpContent.split('\n')
    except:
        return
    # action - object - preposition
    content = []
    #clean data
    i = 0
    for line in tmpContent:
        content.append([])
        lst = line.split("] ")
        for item in lst:
            item = item.strip(" []\n\r")
            content[i].append(item)
        i = i + 1
    #search by two words, frequency analysis by action, additional constraints
    counter = []
    actions = []
    for line in content[1:len(content)-1]:
        if (((search1 == None) and (search2 in line[2])) or ((search2 == None) and (search1 in line[1])) or (((search1 != None) and (search2 != None)) and ((search1 in line[1]) and (search2 in line[2])))):
            if (line[0] not in actions):
                actions.append(line[0])
                counter.append(1)
            else:
                counter[actions.index(line[0])] = counter[actions.index(line[0])] + 1;
    if counter == []:
        return None
    #find max amongst results
    maxAction = actions[counter.index(max(counter))]
    return maxAction

#converts the annoying help() format to an easy to use list
def helpToList(path,search):
    f = open(path,"w")
    sys.__stdout__ = sys.stdout
    sys.stdout = f
    help(search)
    f.close()
    sys.stdout = sys.__stdout__
    lines = None
    with open(path) as f:
        lines = f.read().splitlines()
    return lines

#gets help from the Python help()
def getHelp(search,libraries,datatypes):
    changed = False
    path = 'output.txt'
    lines = helpToList(path,search)
    if ("No Python documentation found for" in lines[0]):
        lines = []
    else:
        lines = helpToCode(search,lines)
    if lines == []:
        for library in libraries:
            try:
                lib = importlib.import_module(library)
                searchTerm = lib.__name__+'.'+search
                lines = helpToList(path,searchTerm)
                if lines != []:
                    break
            except:
                pass
        if (lines == []):
            for types in datatypes:
                lines = helpToList(path,types+'.'+search)
                if lines != None:
                    break
    else:
        changed = True
    if ((changed == False) and (lines != [])):
        if ("No Python documentation found for" in lines[0]):
            lines = []
        else:
            lines = helpToCode(search,lines)
    return lines

#extracts the code from a list of help() data
def helpToCode(search,lines):
    res = []
    cont = True;
    start = False
    stop = False;
    if (len(lines) <= 2):
        return res
    if (('class '+search) in lines[2]):
        i = 3;
        while (lines[i].strip(" |") != ''):
            res.append(lines[i].strip(" |"))
            i+=1
    elif ((search+' = ') in lines[2]):
        res.append(lines[3].strip(" |"))
    if ((len(res) != 0) and (res[0] == '')):
        del res[0]
    return res

#extracts the libraries that were included in the file being inspected
def getLibraries(code):
    libraries = []
    for line in code:
        if '#' not in line:
            match = re.match(r"(from)\s+\w+\s+(import)",line)
            if (match == None):
                match = re.match(r"(import)\s+",line)
            if (match != None):
                line = line.replace(match.group(),"")
                match = re.search(r"\s*(as)\s+\w+",line)
                if (match != None):
                    line = line.replace(match.group(),"")
                line = line.lstrip()
                libraries.append(line)
    return libraries

def checkFunctions(word, _tuple):
    _word = word.lstrip()
    _word = _word.replace("'",'')
    _word = _word.lower()
    tmp = searchTranslate(_word,syntaxAcrossLanguages)
    return tmp

def extract(message):
    variables = []
    while ("'" in message):
        start = message.find("'")
        end = message[start+1:].find("'")+start+2
        word = message[start:end]
        variables.append(word)
        message = message.replace(message[start:end],'')
    return variables

def convert(tmpvariables):
    variables = []
    for var in tmpvariables:
        newWord = checkFunctions(var.lower(),syntaxAcrossLanguages) #originally functions
        if newWord:
            #word = newWord
            variables.append(newWord)
        else:
            variables.append(var)
    return variables

def getAttrErr(message):
    variables = convert(extract(message))
    error = ''
    for var in variables:
        error = error + ' ' + var
    #convert any spaces to '+'
    error = error.replace(" ","+")
    error = error.replace("'","")
    return urlBase+urlSearch+filterQuery(error)

def getIndentationError(message):
    errorType = message.split(' ', 1)[0]
    return urlBase+urlSearch+filterQuery(message.replace(errorType,''))

def getIndexError(message):
    error = message.replace("IndexError: ","index error ")
    toRemove = " cannot be "
    if (toRemove in error):
        error = message.replace(toRemove,"")
    #convert any spaces to '+'
    error = error.replace(" ","+")
    return urlBase+urlSearch+filterQuery(error)

def getNameError(message):
    variables = []
    query = ''
    #if '' in list, use to search
    if "'" in message:
        variables = convert(extract(message))
        toAdd = None #checkFunctions(variables[0],terms)
        if (len(variables) > 1):
            toAdd = actionWord(variables[0],variables[1])
        else:
            toAdd = actionWord(variables[0])
        if (toAdd == None):
            return urlBase+urlSearch+filterQuery("NameError")
        query = query + toAdd + ' to ' + variables[0]
        #convert any spaces to '+'
        #query = query.replace(" ","+")
        #quety = filterQuery(query)
        return urlBase+urlSearch+filterQuery(query)
    #else generic nameerror search
    else:
        return urlBase+urlSearch+filterQuery("NameError")

def getSyntaxError(message):
    #print(offendingLine)
    #get possbilities for each word in offendingline
    #first count to ensure equal
    single = offendingLine.count("'")
    double = offendingLine.count('"')
    if ((single+double) % 2 == 1):
        #search for quote errors
        return urlBase+urlSearch+filterQuery("quotation marks")
    #then count brackets
    openB = offendingLine.count("(")+offendingLine.count("[")+offendingLine.count("{")
    closeB = offendingLine.count(")")+offendingLine.count("]")+offendingLine.count("}")
    if (openB != closeB):
        #search for quote errors
        return urlBase+urlSearch+filterQuery("bracket meanings")
    #split offendingline and remove symbols
    tokens = re.split(r'[!@#$%^&*_\-+=\(\)\[\]\{\}\\|~`/?.,<>:; ]',offendingLine)
    #remove strings/quotes
    for token in tokens:
        if (("'" in token) or ('"' in token)):
            #remove
            tokens.remove(token)
    #then find possibilites for each word
    possibilites = []
    for token in tokens:
        possible = []
        possible.extend(difflib.get_close_matches(token.lower(),pythonKeywords,3,0.6))
        possible.extend(difflib.get_close_matches(token.lower(),pythonBuiltin,3,0.6))
        #if exact match, only keep that word
        flag = False
        for word in possible:
            if word == token:
                possibilites.append(word)
                flag = True
        if flag == False:
            possibilites.extend(possible)
    #need to check tokens
    isFor = False
    isWhile = False
    isIf = False
    isDef = False
    for word in possibilites:
        if word is 'for':
            isFor = True
        elif word is 'while':
            isWhile = True
        elif word is 'def':
            isDef = True
        elif ((word is 'if') or (word is 'else')):
            isIf = True
    if (isFor):
        return urlBase+urlSearch+filterQuery("for loop")
    elif (isWhile):
        return urlBase+urlSearch+filterQuery("while loop")
    elif (isDef):
        #ADD HERE
        pass
    elif (isIf):
        return urlBase+urlSearch+filterQuery("else if syntax")
    else:
        #else generic syntaxerror search
        return urlBase+urlSearch+filterQuery("SyntaxError: invalid syntax")

def getTabError(message):
    errorType = message.split(' ', 1)[0]
    return urlBase+urlSearch+filterQuery(message.remove(errorType,''))

def getTypeError(message):
    #lot to do here
    errorType = message.split(' ', 1)[0]
    if ("the first argument must be callable" in message):
        return urlBase+urlSearch+filterQuery("must have first callable argument")
    elif ("not all arguments converted during string formatting" in message):
        return urlBase+urlSearch+filterQuery(message.remove(errorType,''))
    else:
        #else generic search
        return urlBase+urlSearch+filterQuery(message)

def getQuestions(query):
    #get the results page
    page = requests.get(query) 
    soup = BeautifulSoup(page.content,"html5lib")
    #find a attributes on page (these may contain links)
    links = soup.findAll("a")
    answers = [];
    for i in range(0,len(links)-1):
        #locate all links
        if (links[i].has_attr('href')):
            temp=links[i]['href']
            #filter out links that do not correspond to answers to question
            if (("/questions/" in temp) and ("https://" not in temp) and ("tagged" not in temp)):
                answers.append(temp)
    return answers

def getLink(answers):
    urlAnswer = []
    #return url
    for link in answers:
        urlAnswer.append(urlBase + link)
    return urlAnswer

def postIDs(urls):
    ids = []
    #start of line number
    beg = 'https://stackoverflow.com/questions/'
    for url in urls:
        #extract error line
        startErrorNum = url.find(beg) + len(beg)
        if (startErrorNum != -1):
            url = url[startErrorNum:]
            endErrorNum = url.find('/')
            if (endErrorNum != -1):
                url = url[:endErrorNum]
                ids.append(int(url))
            else:
                ids.append(-1)
        else:
            ids.append(-1)
    return ids

def getVotes(link):
    #get the results page
    page = requests.get(link) 
    soup = BeautifulSoup(page.content,"html5lib")
    #find a attributes on page (these may contain links)
    temp = soup.findAll("div",{"class": "vote"})
    votes = []
    for div in temp:
        votes.append(int(div.find("span",{"class":"vote-count-post "}).text))
    return votes

def getAnswers(link):
    page = requests.get(link)
    soup = BeautifulSoup(page.content,"html5lib")
    #collect all answers on page
    answerSections = soup.findAll("div", {"class": "answercell"})
    #answerText will hold all lines of answers individually
    answerText = []
    #extract lines of answers
    for sec in soup.findAll("div", {"class": "answercell"}):
        foo_descendants = sec.descendants
        for ans in foo_descendants:
            if ans.name == 'div' and ans.get('class', '') == ['post-text']:
                for aLine in ans.text.split("\n"):
                    if aLine.strip() != '':
                        answerText.append(aLine)
                        answerText.append('.')
                #answerText.append(ans.text)
    return answerText

def getSummary(sentences):
    #convert sentences to single string -> not good for code
    #text = ''.join(sentences)
    #parse text
    parser = PlaintextParser.from_string(sentences, Tokenizer("english"))
    #get length of answer(s)
    numSentences = len(parser.document.sentences)
    #halve length and round up
    length = 4
    summariser = LuhnSummarizer()
    #summarise text
    summary = summariser(parser.document,length)
    #return summary
    return summary

def prettyPrinter(summary):
    newSummary = summary
    while ('\n\n' in newSummary):
        newSummary = newSummary.replace('\n\n','\n') 
    return newSummary

def blank(num):
    for i in range(0,num):
        print('\n')
    return

def errorLine(message):
    if '^' in message:
        lines = message.split("\n")
        #remove first and last lines
        del lines[3]
        del lines[0]
        #set max length, if first token ends at the same position as ^ then the error is on the previous line
        maxLength = len(lines[1].split("^")[0])+1
        tempLength = 0
        tmp = re.split(r'[!@#$%^&*_\-+=\(\)\[\]\{\}\\|~`/?.,<>:; ]',lines[0])
        i = 0
        while (tmp[i] == ''):
            i += 1
        tempLength = i + len(tmp[i])
        #take previous line
        if (tempLength >= maxLength):
            return 2
    #accept compiler line
    return 1

def getSymTable(code,lineNum,numToRemove,isSyntaxError,message):
    global offendingLine
    codeLines = code.split("\n")
    #if not SyntaxError assume compiler has identified the correct line
    if (isSyntaxError == False):
        offendingLine = codeLines[int(lineNum)]
    tmpCode = code
    #remove code after error line
    pos = tmpCode.replace('\n', '\t', int(lineNum)).find('\n')
    code = code[:pos]
    #adjust numToRemove
    if (lineNum-numToRemove < 1):
        numToRemove = int(-((lineNum-numToRemove)/lineNum))
    #remove comments from user code
    newCode = ''
    skip = False
    for i in range(0,int(lineNum)):
        tmpLine = codeLines[i].lstrip()
        if (tmpLine.isspace() or (tmpLine == '')):
            del tmpLine
        elif ((len(tmpLine) > 0) and (tmpLine[0] == '#')):
            del tmpLine
        elif ((len(tmpLine) > 2) and (tmpLine[:3] == '"""')):
            del tmpLine
            if (skip == True):
                skip = False
            else:
                skip = True
        elif ((len(tmpLine) > 2) and (tmpLine[:3] == "'''")):
            del tmpLine
            if (skip == True):
                skip = False
            else:
                skip = True
        elif skip == False:
            newCode = newCode+codeLines[i]+'\n'
    #remove newline char
    newCode = newCode[:len(newCode)-1]
    if (isSyntaxError):
        toSkip = errorLine(message)
        offendingLine = newCode.splitlines()[-toSkip]
        numToRemove = toSkip
    newCode = "\n".join(newCode.split("\n")[:-numToRemove])
    codeLines = newCode.split("\n")
    #get whitespace at start of line
    final = len(codeLines)-1
    indent = codeLines[final][:-len(codeLines[final].lstrip())]
    if (newCode == ''):
        return None
    if (codeLines[final][len(codeLines[final])-1] == ':'):
        if (indent == ''):
            indent = '  '
        else:
            indent = indent+indent
    code = newCode + '\n' + indent + "import sys"+'\n'+ indent + "sys.exit()"+'\n'
    symTable = symtable.symtable(code,"userCode","exec")
    return symTable

def swapCode():
    pass

def indentifyCode(text):
    #define tags containing code
    startTag = "<code>"
    endTag = "</code>"
    #list to hold code positions
    pos = []
    newText = text
    if startTag in text:
        for i,c in enumerate(text):
            if c == '<':
                if startTag == text[i:i+len(startTag)]:
                    pos.append([])
                    pos[len(pos)-1].append(i+len(startTag))
                    if (text[i-5:i] == "<pre>"):
                        pos[len(pos)-1].append(1)
                    else:
                        pos[len(pos)-1].append(0)
                if endTag == text[i:i+len(endTag)]:
                    pos[len(pos)-1].append(i)

        for i in range(0,len(pos)):
            tmp = pos[i][2]
            pos[i][2] = pos[i][1]
            pos[i][1] = tmp
    return pos

def removeCode(text,pos,maxLength,removeBlocks):
    newText = text
    for i in range(0,len(pos)):
        toRemove = text[pos[len(pos)-1-i][0]:pos[len(pos)-1-i][1]]
        if ((removeBlocks) and (bool(pos[len(pos)-1-i][2]))):
            newText = newText.replace(toRemove,'')
        elif (len(toRemove) > maxLength):
            newText = newText.replace(toRemove,'')
    return newText

def removeTags(text):
    text = text.replace("<pre>","*pre*")
    text = text.replace("</pre>","*pre*")
    cleaner = re.compile('<.*?>')
    cleanText = re.sub(cleaner, '', text)
    return cleanText

def replaceCode(text,pos,message):
    newText = text
    noTags = removeTags(text)
    noTagsLines = noTags.split('\n')
    errorLines = message.split('\n')
    errorType = None
    for line in errorLines:
        if "Error: " in line:
            errorType = line.split(' ',1)[0]
    compilerErrorLine = errorLines[1]
    QAOffendingLine = None #Syntax Error only
    QAErrorLine = None
    QACorrectLine = None
    correctLine = None
    #check for compiler text in question/answer
    #match = re.search(r'(File|Traceback)[\s\S]+[\n|\n\s\S]+[\n][\s]+[^][\n][A-za-z]+[Error:][ \S]+',noTags)
    match = re.search(r'(File|Traceback)(.+)\n(.+)((\n)|(\n( |\t)+\^))\n(Arithmetic|FloatingPoint|Overflow|ZeroDivision|Assertion|Attribute|Buffer|EOF|Import|ModuleNotFound|Lookup|Index|Key|Memory|Name|UnboundLocal|OS|BlockingIO|ChildProcess|Connection|BrokenPipe|ConnectionAborted|ConnectionRefused|ConnectionReset|FileExists|FileNotFound|Interrupted|IsADirectory|NotADirectory|Permission|ProcessLookup|Timeout|Reference|Runtime|NotImplemented|Recursion|Syntax|Indentation|Tab|System|Type|Value|Unicode|UnicodeDecode|UnicodeEncode|UnicodeTranslate)(Error:)(.+)',noTags)
    if (match):
        QAErrorLine = match.group(0).split('\n')[1]
        #ALSO CHECK QUESTION?
    #if SyntaxError we may need to handle differently
    if ((errorType == 'SyntaxError:') and (QAErrorLine)):
        if (compilerErrorLine != offendingLine):
            for i in range(0,len(pos)):
                previous = None
                if ((bool(pos[i][2])) and (match.group(0) not in text[pos[i][0]:pos[i][1]])):
                    for line in text[pos[i][0]:pos[i][1]].split('\n'):
                        if (line == QAErrorLine):
                            QAOffendingLine = previous
                        previous = line
            #check previous line
            #if previous line of code, swap and exit
            if (QAOffendingLine):
                QAOffendingLine = QAOffendingLine.strip()
            if (compilerErrorLine):
                compilerErrorLine = compilerErrorLine.strip()
            if (QAErrorLine):
                QAErrorLine = QAErrorLine.strip()
            
            #print(QAOffendingLine)
            #print(compilerErrorLine)
            #print(QAErrorLine)
            #print(offendingLine)
            
            for i in reversed(pos):
                x=pos.index(i)
                if (QAOffendingLine in text[pos[x][0]:pos[x][1]]):
                    newText = newText[:pos[x][0]] + compilerErrorLine + newText[pos[x][1]:]
                elif (QAErrorLine in text[pos[x][0]:pos[x][1]]):
                    newText = newText[:pos[x][0]] + offendingLine + newText[pos[x][1]:]
            return newText
    if (QAErrorLine == None):
        tmpQAErrorLine = difflib.get_close_matches(offendingLine,noTagsLines,1,0.4)
        if (tmpQAErrorLine == []):
            return text
        else:
            QAErrorLine = tmpQAErrorLine[0]
    if ((QAErrorLine == None) or (QAErrorLine == '')):
        return text
    #print(QAErrorLine)
    #if exists, check for similar lines
    possibleLines = []
    if (QAErrorLine == None):
        possibleLines = difflib.get_close_matches(QAErrorLine,noTagsLines,3,0.4)
        #SequenceMatcher(None,line,line2).ratio()
        #print(possibleLines)
    #if exists, substitute variables
    if ((len(possibleLines) > 0) or QAErrorLine):
        #tokenise similar to before, may have to group
        userVariables = []
        userBuiltin = []
        tokens = re.split(r'[!@#$%^&*_\-+=\(\)\[\]\{\}\\|~`/?.<>:; ]',compilerErrorLine)
        for x in reversed(tokens):
            if ((x not in pythonKeywords) and (x not in pythonBuiltin)):
                userVariables.append(x)
            else:
                userBuiltin.append(x)
        userVariables = list(reversed(userVariables))
        userBuiltin = list(reversed(userBuiltin))
        #split
        QALine = None
        if (QAErrorLine):
            QALine = QAErrorLine
        else:
            QALine = possibleLines[0]
        if (',' in QALine):
            while ((', ' in QALine) or (' ,' in QALine)):
                QALine = QALine.replace(", ",",")
                QALine = QALine.replace(" ,",",")
        QAVariables = []
        tokens = re.split(r'[!@#$%^&*_\-+=\(\)\[\]\{\}\\|~`/?.<>:; ]',QALine)
        for x in reversed(tokens):
            if ((x not in pythonKeywords) and (x not in pythonBuiltin)):
                QAVariables.append(x)
        QAVariables = list(reversed(QAVariables))

        for word in reversed(QAVariables):
            if (word == ''):
                QAVariables.remove(word)

        for word in reversed(userVariables):
            if (word == ''):
                userVariables.remove(word)

        #print(QAVariables)
        #print(userVariables)

        newQALine = QALine
        if (len(userVariables) == len(QAVariables)):
            for word,i in enumerate(QAVariables):
                newQALine.replace(word,userVariables[i])


    #else leave
    #if SyntaxError, check for similar line to offendingLine
    #if (errorType == 'SyntaxError:'):
        #if (compilerErrorLine != offendingLine):
            #if exists, substitute variables
            #pass

    #else leave

    return newText

def holder():
    i = 0
    return i

def takeInput(self):
    input = sublime.Window.show_input_panel(sublime.active_window(),"Enter feedback", "test", None, None, None)
    return input

class ExecCommand(defaultExec.ExecCommand):
    #run on build
    def finish(self, proc):
        #variables to write
        date = list(datetime.datetime.now().timetuple())
        compilerOutput = None
        pluginOutput = None
        questionID = None
        answerID = None
        userCode = None
        negativeDownVotes = False
        noResults = False
        fileNotFound = False
        errorLineNotFound = False
        anError = False
        noOutput = False
        super(ExecCommand, self).finish(proc)
        #open file to capture data
        dataFile = open("/Users/emilliethiselton/Library/Application Support/Sublime Text 3/Packages/User/data.json","r+")
        #increment = int(dataFile.readline())+1
        i = 0
        for i, l in enumerate(dataFile):
            pass
        #incremenet the increment variable
        from_file = open("/Users/emilliethiselton/Library/Application Support/Sublime Text 3/Packages/User/data.json") 
        #line = from_file.readline()
        line = str(i)+'\n'
        #dataFile.write(line)
        #shutil.copyfileobj(from_file, dataFile)
        #define variables to store data
        dataToWrite = {}
        increment = line
        dataToWrite[increment] = []
        #taking input
        message = getCompilerMessage(self)
        if (message != ''):
            #get error message
            error = getErrorMessage(message)
            if ((error != -1) and (error != '')):
                #get error line
                lineNum = getErrorLine(message)
                if ((lineNum != -1) and (lineNum != '')):
                    #get file name
                    fileName = getFileName(message)
                    if ((fileName != -1) and (fileName != '')):
                        code = getCode(fileName)
                        #build lookup table
                        num = 1
                        errorType = error.split(' ', 1)[0]
                        isSyntaxError = False
                        if (errorType == "SyntaxError:"):
                            num = 2
                            isSyntaxError = True

                        #SymTable = getSymTable(code,lineNum,num,isSyntaxError)
                        SymTable = getSymTable(code,lineNum,num,isSyntaxError,message)
                        #storing libraries from code
                        libraries = getLibraries(code.split("\n"))
                        #store python doc info
                        pyDocInfo = None
                        usingPyDoc = False
                        #now need to analyse error by type to formula correct query
                        #getting query
                        if (errorType == "AttributeError:"):
                            query = getAttrErr(error)
                            search = convert(extract(message))[1]
                            search = search.replace("'","")
                            tmpSearch=checkFunctions(search,syntaxAcrossLanguages) #originally words
                            if (tmpSearch != None):
                                search = tmpSearch
                            pyDocInfo = getHelp(search,libraries,pythonDataTypes)
                            #usingPyDoc = True
                        elif (errorType == "IndentationError:"):
                            query = getIndentationError(error)
                        elif (errorType == "IndexError:"):
                            query = getIndexError(error)
                        elif (errorType == "NameError:"):
                            query = getNameError(error)
                            search = convert(extract(message))[0]
                            tmpSearch=checkFunctions(search,syntaxAcrossLanguages) #originally words
                            if (tmpSearch != None):
                                search = tmpSearch
                            pyDocInfo = getHelp(search,libraries,pythonDataTypes)
                            #usingPyDoc = True
                        elif (errorType == "SyntaxError:"):
                            query = getSyntaxError(error)
                        elif (errorType == "TabError:"):
                            query = getTabError(error)
                        else:
                            #general/default
                            query = getQuery(error)
                        print(query)
                        #now we have all necessary info
                        #getting questions/results
                        question = getQuestions(query)
                        #getting URL
                        links = getLink(question)
                        #if no results, exit
                        if (len(links) > 1):
                        #if (links[0] != 'https://stackoverflow.com/questions/ask'):
                            #store selected question/answer
                            chosenQuestionID = None
                            #connect to SO API with key
                            key = '0gxVKQSpJejEvn)i*pFYmg(('
                            so = stackexchange.Site(stackexchange.StackOverflow,key)
                            so.include_body = True
                            #get question ids
                            questionID = postIDs(links)
                            answerNum = 1;
                            theAnswer = None
                            #getting votes to check answers have no negative votes
                            while ((theAnswer is None) and (answerNum < len(questionID))):
                                #get first question and its answer
                                tmp = so.question(questionID[answerNum])
                                attri = dir(tmp)
                                theAnswer = None
                                if 'accepted_answer' in attri:
                                    acceptedAnswerID = tmp.accepted_answer_id
                                    acceptedAnswer = so.answer(acceptedAnswerID)
                                    theAnswer = acceptedAnswer
                                elif (tmp.answer_count > 0):
                                    answers = tmp.answers
                                    bestAnswerPos = 0
                                    for answer in answers:
                                        if answer.score > answers[bestAnswerPos].score:
                                            bestAnswerPos = answers.index(answer)
                                    if answers[bestAnswerPos].score > 0:
                                        theAnswer = answers[bestAnswerPos]
                                answerNum += 1
                            if (theAnswer):
                                answerLines = theAnswer.body.split("\n")
                                #need to remove large code blocks
                                #need to remove tags
                                pos = indentifyCode(theAnswer.body)
                                text = theAnswer.body
                                #text = removeCode(theAnswer.body,pos,1000,True)
                                #text = prettyPrinter(removeTags(removeCode(theAnswer.body,100,True)))
                                tester = replaceCode(theAnswer.body,pos,message)
                                tester = removeTags(tester)
                                #summarise here
                                summary = None
                                if (len(tester.split('\n')) <= 4):
                                    summary = tester
                                else:
                                    tmpSummary = getSummary(tester)
                                    summary = []
                                    tmpTest = tester.replace(". ",'\n')
                                    for line in tmpSummary:
                                        for sec in tmpTest.split('\n'):
                                            if ((sec.lstrip() != '') and (sec.lstrip() in str(line))):
                                                if ("*pre*" in str(line)):
                                                    sec = sec.replace("*pre*","")
                                                    exists = False
                                                    for i in summary:
                                                        if (sec == i):
                                                            exists = True
                                                    if (exists == False):
                                                        sec = sec.replace("&gt;",">")
                                                        sec = sec.replace("&lt;","<")
                                                        summary.append(sec)
                                                else:
                                                    exists = False
                                                    for i in summary:
                                                        if (str(line) == i):
                                                            exists = True
                                                    if (exists == False):
                                                        newLine = str(line)
                                                        newLine = newLine.replace("&gt;",">")
                                                        newLine = newLine.replace("&lt;","<")
                                                        summary.append(newLine)
                                    summary = '\n'.join(summary)+'\n'
                                print()
                                print(summary)
                                #print pydoc info
                                if usingPyDoc:
                                    print("From PyDocs")
                                    if (pyDocInfo != []):
                                        print('\n'.join(pyDocInfo))
                                #get feedback
                                #input = takeInput(self)
                                #save data
                                compilerOutput = message
                                pluginOutput = summary
                                questionID = theAnswer.question.id
                                answerID = theAnswer.id
                                userCode = code
                                #dataToWrite[increment].append({'date':list(datetime.datetime.now().timetuple()),'compilerOutput':message,'PluginOutput':summary,'questionID':theAnswer.question.id,'answerID':theAnswer.id,'userCode':code})
                            else:
                                print("All relevant results have negative down votes!")
                                negativeDownVotes = True
                        else:
                            print("No Results!")
                            noResults = True
                    else:
                        print("File/Path not found!")
                        fileNotFound = True
                else:
                    print("Error line not found!")
                    errorLineNotFound = True
            else:
                print("No compiler error message!")
                anError = True
        else:
            print("No compiler output!")
            noOutput = True
        dataToWrite[increment].append({'date':date,'compilerOutput':compilerOutput,'PluginOutput':pluginOutput,'questionID':questionID,'answerID':answerID,'userCode':userCode,'negativeDownVotes':negativeDownVotes,'noResults':noResults,'fileNotFound':fileNotFound,'errorLineNotFound':errorLineNotFound,'anError':anError,'noOutput':noOutput})
        json.dump(dataToWrite,dataFile)
        dataFile.write("\n")
        dataFile.close()
