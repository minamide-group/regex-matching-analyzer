Language: [English](README.md) | [日本語](README_ja.md)

# regex-matching-analyzer
Analyzer to determine time complexity of backtracking regular expression matching.
For a given regular expression, it determines the order of worst case time complexity to the length of input strings.

## Usage
We recommend to use sbt.
The commands below should be input to the sbt shell.

There are two ways to run the code.
- Input expressions from command line
```
run [options]
```
- Input expressions from a file
```
run <input file> [options]
```

### Options
|Option|argument|default||
|:----|:----|:----|:----|
|`--style`|`raw`,`PCRE`|`raw`|Specifying a [style of regular expressions](#Style-of-Regular-Expressions)|
|`--method`|`Lookahead`,`SubsetPrune`,`Nondeterminism`,`Exhaustive`|`Lookahead`|Specifying an [algorithm to simulate backtracking](#Algorithm-to-Simulate-Backtracking)|
|`--timeout`|integer|`10`|Specifying time limit (second) of analysis (specify `<= 0` to disable timeout)|
|`--debug`|(no argument)|disable|Enable debug mode|


## Inputs
### Style of Regular Expressions
- `raw`  
Write expression directly.  
e.g.) `^a*|b`
- `PCRE`  
Write expression in `/.../`.
Some [modifiers](#Modifiers-in-PCRE-Style) are available.  
e.g.) `/^a*|b/s`  
We support to use other characters as delimiters.
We also support bracket style delimiters such as `(...)`,`{...}`,`[...]`, and `<...>`.

### Format of Input Files
The analyzer reads expressions line by line.
Thus, expressions must be separated by line breaks in your input file.

## Outputs
The possible results are follows:
- `constant`
- `linear`
- `polynomial, degree = n`: Polynomial time of degree `n`. (`n >= 2`)
- `exponential`
- `timeout`
- `skipped`: A given expression has unsupported features.
- `error`: Error, mainly a parse error.

The output also contains following information.
- a witness of  (only if the result is `polynomial` or `exponential`. Currently, this feature is supported only when using algorithm `Lookahead`.)
- Execution time taken to analyze.

### Output Files
If the analysis is performed on expressions from an input file, the output files are generated in directory `output/<input file>_<timestamp>`.
This contains the following files:
- `summary.txt`: The summary of results
- `list.txt`: A list of all analyzed expressions
- `result.txt`: A list of results
- `<result>/...`: A list of expressions and results
whose result is `<result>`(In `polynomial/...`, files for each degree of polynomial will be generated.)


## Regular Expression Parser
This is a list of supported features.
All characters that do not appear in the following list will be a expression just matches the character itself.
- `∅`: The empty set
- `ε`: The empty string
- `r1|r2`: `r1` or `r2`
- `\unnnn`: A character with hexadecimal code `nnnn`
- `\xnn`: A character with hexadecimal code `nn`
- `\nnn`: A character with octal code `nnn`
- `.`: Any character except for a newline character
- Repetitions
  + `r*`: 0 or more times
  + `r+`: 1 or more times
  + `r?`: 0 or 1 times
  + `r{n}`: `n` times
  + `r{n,}`: `n` or more times
  + `r{,m}`: `m` or less times
  + `r{n,m}`: `n` to `m` times
- Groups
  + `(r)`: Capturing group
  + `(?:r)`: Non-capturing group
  + `(?<name>r)`,`(?'name'r)`,`(?P<name>r)`: Named group
- Character classes
  + `[...]`: One of specified characters
  + `[^...]`: One of not specified characters
- Special characters
  + `\a`: Alarm (`\u0007`)
  + `\e`: Escape (`\u001B`)
  + `\f`: Formfeed (`\u000C`)
  + `\n`: Newline (`\u000A`)
  + `\r`: Carriage return (`\u000D`)
  + `\t`: Tab (`\u0009`)
- Predefined character classes
  + `\d`: Digits (`[0-9]`)
  + `\D`: Any character that is not digits (`[^0-9]`)
  + `\h`: Horizontal whitespace (`[\u0009]`)
  + `\H`: Any character that is not horizontal whitespace (`[^\u0009]`)
  + `\s`: Any whitespace character (`[ \t\n\r\f]`)
  + `\S`: Any character that is not whitespace character (`[^ \t\n\r\f]`)
  + `\v`: Vertical whitespace (`[\u000B]`)
  + `\V`: Any character that is not vertical whitespace (`[^\u000B]`)
  + `\w`: Any word character (`[a-zA-Z0-9_]`)
  + `\W`: Any non-word character (`[^a-zA-Z0-9_]`)
  + `\R`: Line breaks (`[\r\n]`)
- Anchors
  + `^`: Start of string
  + `$`: End of string

The parser is also able to handle the following features,
but they are unsupported for analyzer and its result will be `skipped`.
- Lookahead/Lookbehind
  + `(?=r)`: Positive lookahead
  + `(?!r)`: Negative lookahead
  + `(?<=r)`: Positive lookbehind
  + `(?<!r)`: Negative lookbehind
- `(?(r)r1)`,`(?(r)r1|r2)`: Conditional
- Back references
  + `\1`,`\2`, ...: reference with index
  + `(?P=name)`,`\k<name>`,`\k'name'`,`\k{name}`: reference with name

### Escaping
You can escape any characters except for alphanumeric by backslash `\` and then it will become a expression just matches the character itself.
The following characters must be escaped:
+ Outside of character classes  
`∅`,`ε`,`.`,`|`,`*`,`+`,`?`,`^`,`$`,`(`,`)`,`[`,`]`,`\`
+ Inside of character classes  
`]`,`\`

### Repetitions
Repetitions matches greedy by default.
Thus, they try to match as much as possible.
But If they are followed by `?`, then it becomes lazy and matches the minimum number of times possible.

### Character classes
In character classes, the following forms are supported.
- `char`: Single character
- `char-char`: range
- Special characters
- Predefined character classes

A hyphen `-` put at the head or tail of bracket, or immediately before or after a predefined character class represents just a hyphen as symbol.

The following special character can be used only in a character classes.
- `\b`: backspace (`\u0008`)

### Anchors
`^` and `$` are supported only when they appear in the head and the tail of expressions, respectively.

### Backslashes Followed by Digits
They will be parsed according to this specification basically.
https://www.php.net/manual/en/regexp.reference.escape.php
- If the head is `0`  
Pattern A
- If the head is not `0`  
Parse digits as decimal number.
  + If it is less than or equal to 9, or less than the number of capturing groups (99 maximum)  
  Pattern B
  + Otherwise  
  Pattern A

- Pattern A: Read up to 3 characters of digits which is less than or equal to 7, and then parse it as octal code.
The rest of digits are parsed as just expressions that represent the numeral.
- Pattern B: Parse as back references. If the specified index is larger then the number of capturing groups in the whole expressions, then an error occur.

### Modifiers in PCRE Style
The following modifiers are supported.
- `i`: Ignore the case of letters
- `s`: `.` become to match newline characters
- `U`: Reverse greediness of repetitions


## Algorithm to Simulate Backtracking
- `Lookahead`:
  + https://github.com/minamide-group/group-only/blob/master/tsukuba-thesis/nakagawa-master-thesis.pdf
- `SubsetPrune`:
  + https://link.springer.com/chapter/10.1007/978-3-319-40946-7_27
  + https://github.com/NicolaasWeideman/RegexStaticAnalysis
- `Nondeterminism`:
  + https://www.jalc.de/issues/2018/issue_23_1-3/jalc-2018-019-038.php
- `Exhaustive`: Performs no backtracking

- If you specify `Exhaustive`, the analyzer determines time complexity of exhaustive matching and its result might be different from that obtained by other algorithm.

- If you specify `Nondeterminism`, expressions which has constant matching time will be determined to be `linear` instead of `constant`.
