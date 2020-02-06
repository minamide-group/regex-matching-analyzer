Language: [English](README.md) | [日本語](README_ja.md)

# regex-matching-analyzer
This is an analyzer to determine the time complexity of regular expression matching.
For a given regular expression, it determines the order of the worst case time complexity to the length of input strings.

The conditions of matching below are assumed.

- The matching algorithm is based on backtracking.
- It performs substring matching and terminates when one successful matching was found.


## Usage
We recommend to use sbt.
The commands below should be input to the sbt shell.

There are two ways to run the code.
- Input expressions from a command line
```
run [options]
```
- Input expressions from a file
```
run <input file> [options]
```

example：
```
run path/to/your_file.txt --style PCRE --timeout 5
```

### Options
|Option|Argument|Default||
|:----|:----|:----|:----|
| `--style` | `raw` , `PCRE` | `raw` |Specifying a [style of regular expressions](#Style-of-Regular-Expressions)|
| `--timeout` |Integer| `10` |Specifying time limit (second) of analysis (specify `<= 0` to disable timeout)|
| `--debug` |(no argument)|disabled|Enable debug mode|


## Inputs
### Style of Regular Expressions
- `raw`  
Write expressions directly.  
e.g.) `^a*|b`
- `PCRE`  
Write expressions in `/.../` .
Some [modifiers](#Modifiers-in-PCRE-Style) are available.  
e.g.) `/^a*|b/s`  
Using other characters as delimiters are supported.
It also supports bracket style delimiters such as `(...)` , `{...}` , `[...]` , and `<...>` .

### Input File Format
The analyzer reads expressions line by line.
Thus, expressions must be separated by line breaks in your input file.


## Outputs
The possible results are follows:
- `constant`
- `linear`
- `polynomial, degree = n`
- `exponential`
- `timeout`
- `skipped` : A given expression has unsupported features.
- `error` : Error, mainly a parse error.

The output also contains following information:
- A witness of polynomial or exponential order.
- Execution time taken to analyze

### Output Files
If the analysis is performed on expressions from an input file, the output files are generated in the directory `output/<input file>_<timestamp>` ,
which contains the following files:
- `summary.txt` : The summary of results
- `list.txt` : A list of all analyzed expressions
- `result.txt` : A list of results
- `<result>/...` : A list of expressions and results whose result is `<result>` (In `polynomial/...` , files for each degree of polynomial will also be generated.)
- `approximated/<result>/...` : A list of expressions and results whose result is `<result>` by [overapproximation](#Overapproximation).


## Regular Expression Parser
Here is the list of supported features.
All characters that do not appear in the following list will become a expression just matches the character itself.
- `∅` : Never matches to any character.
- `ε` : The empty string
- `r1|r2` : `r1` or `r2`
- `\unnnn` : A character with hexadecimal code `nnnn`
- `\xnn` : A character with hexadecimal code `nn`
- `\nnn` : A character with octal code `nnn`
- `.` : Any one character except for a newline character
- Repetition
  + `r*` : 0 or more times
  + `r+` : 1 or more times
  + `r?` : 0 or 1 times
  + `r{n}` : `n` times
  + `r{n,}` : More than or equal to `n` times
  + `r{,m}` : Less than or equal to `m` times
  + `r{n,m}` : More than or equal to `n` , and less than or equal to `m` times
- Groups
  + `(r)` : Capturing group
  + `(?:r)` : Non-capturing group
  + `(?<name>r)` , `(?'name'r)` , `(?P<name>r)` : Named capturing group
- Character classes
  + `[...]` : One of specified characters
  + `[^...]` : One of not specified characters
- Special characters
  + `\a` : Alarm ( `\u0007` )
  + `\e` : Escape ( `\u001B` )
  + `\f` : Formfeed ( `\u000C` )
  + `\n` : Newline ( `\u000A` )
  + `\r` : Carriage return ( `\u000D` )
  + `\t` : Tab ( `\u0009` )
- Predefined character classes
  + `\d` : Digits ( `[0-9]` )
  + `\D` : Any character that is not digits ( `[^0-9]` )
  + `\h` : Horizontal whitespace ( `[\u0009]` )
  + `\H` : Any character that is not horizontal whitespace ( `[^\u0009]` )
  + `\s` : Any whitespace character ( `[ \t\n\r\f]` )
  + `\S` : Any character that is not whitespace character ( `[^ \t\n\r\f]` )
  + `\v` : Vertical whitespace ( `[\u000B]` )
  + `\V` : Any character that is not vertical whitespace ( `[^\u000B]` )
  + `\w` : Any word character ( `[a-zA-Z0-9_]` )
  + `\W` : Any non-word character ( `[^a-zA-Z0-9_]` )
  + `\R` : Line breaks ( `[\r\n]` )
- anchors
  + `^` : Start of string
  + `$` : End of string
  + `\b` : word boundary
- Lookahead/Lookbehind
  + `(?=r)` : Positive lookahead
  + `(?!r)` : Negative lookahead
  + `(?<=r)` : Positive lookbehind
  + `(?<!r)` : Negative lookbehind
- Back references
  + `\1`,`\2` , ...: Reference with index
  + `(?P=name)` , `\k<name>` , `\k'name'` , `\k{name}` : Reference with name
- `(?(r)r1)` , `(?(r)r1|r2)` : Conditional

### Escaping
You can escape any characters except for alphanumeric by a backslash `\` and then it will become a expression just matches the character itself.
The following characters must be escaped:
+ Outside of character classes  
`∅` , `ε` , `.` , `|` , `*` , `+` , `?` , `^` , `$` , `(` , `)` , `[` , `]` , `\`
+ Inside of character classes  
`]` , `\`

### Repetition
A repetition expression matches greedy by default.
Thus, it tries to match as much as possible.
But if it is followed by `?`, then it becomes lazy and matches the minimum number of times possible.

### Character classes
In character classes, the following forms are supported:
- `char` : Single character
- `char-char` : Range notation
- Special characters
- Predefined character classes

A hyphen `-` put at the head or tail of brackets, or immediately before or after predefined character classes represents just a hyphen as a symbol.

The following special character can be used only in character classes:
- `\b` : backspace ( `\u0008` )

### Backslashes Followed by Digits
They will be parsed according to the following specification basically.  
https://www.php.net/manual/en/regexp.reference.escape.php
- If the head is `0`  
Pattern A
- If the head is not `0`  
Parse digits as a decimal number.
  + If it is less than or equal to 9, or less than or equal to the number of capturing groups (99 maximum)  
  Pattern B
  + Otherwise  
  Pattern A

- Pattern A: Read up to 3 characters of digits which are less than or equal to 7, and then parse it as an octal code. The rest of digits are parsed as just expressions that represent the numeral.
- Pattern B: Parse it as back references. If the specified index is larger then the number of capturing groups in the whole expressions, then an error occurs.

### Modifiers in PCRE Style
The following modifiers are supported:
- `i` : Ignore the case of letters
- `s` : Make `.` to match newline characters
- `U` : Reverse greediness of repetitions


## Supported Expressions
The expressions below are unsupported for analyzer, and its result will be `skipped` .
- Conditional expressions
- Groups whose name are duplicated
- Lookbehind with unbounded matching length
- lookbehind/back reference/word boundary in lookahead
- back reference with cyclic dependency

### Overapproximation
If the given expression contains lookbehind, back references or word boundary,
the analyzer performs overapproximation.
Thus, the result will be an upper bound of the true matching complexity.
