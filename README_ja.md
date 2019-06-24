Language: [English](README.md) | [日本語](README_ja.md)

# regex-matching-analyzer
与えられた正規表現に対してマッチングを行った場合に，マッチング対象文字列の長さに対して最悪時に要する時間計算量のオーダを判定します．


## Usage
sbtで実行する想定です．以下，コマンドはすべてsbtを起動した状態で入力してください．
正規表現をコマンドラインから入力する方式とファイルから入力する形式をサポートしています．
- コマンドラインから入力する場合
```
run [options]
```
- ファイルから入力する場合
```
run <input file> [options]
```

### オプション
|オプション|引数|デフォルト値||
|:----|:----|:----|:----|
|`--style`|`raw`,`PCRE`|`raw`|[正規表現の形式](#正規表現の形式)指定|
|`--method`|`Lookahead`,`SubsetPrune`,`Nondeterminism`,`Exhaustive`|`Lookahead`|[バックトラックを模倣するアルゴリズム](#バックトラックを模倣するアルゴリズム)の指定|
|`--timeout`|整数|`10`|タイムアウト秒数の指定（0以下の値を指定するとタイムアウト無効）|
|`--debug`|なし|非表示|デバッグ情報の表示|


## 入力
### 正規表現の形式
- `raw`  
正規表現を直に記述します．  
e.g.) `^a*|b`
- `PCRE`  
正規表現を`/.../`で囲って記述します．[いくつかの修飾子](#PCRE形式での修飾子)をサポートしています．  
e.g.) `/^a*|b/s`  
また，`/`以外の記号で囲ったり，`(...)`,`{...}`,`[...]`,`<...>`で囲う形式もサポートしています．

### 入力ファイル形式
改行を区切りとして正規表現を読み込みます．検査したい正規表現を1行ずつ記述してください．


## 出力
結果は以下のいずれかを出力します．
- `constant`: 定数時間
- `linear`: 線形時間
- `polynomial, degree = n`: 次数`n`の多項式時間（`n >= 2`）
- `exponential`: 指数時間
- `timeout`: タイムアウト
- `skipped`: サポートしていない表現が存在
- `error`: エラー（主に正規表現のパースの失敗）

また，同時に以下の内容も出力されます．
- 判定結果の証拠となる文字列（`polynomial`,`exponential`と判定された場合のみ．現在はアルゴリズム`Lookahead`のみ対応．）
- 判定に要した時間

### 出力ファイル
ファイルから入力する形式の場合は結果をディレクトリ`output/<input file>_<timestamp>`に出力します．これには以下のファイルが含まれます．

- `summary.txt`: 検査結果のサマリー
- `list.txt`: 検査された正規表現の一覧
- `result.txt`: 検査結果の一覧
- `<result>/...`: 検査結果が`<result>`であった正規表現の一覧とその検査結果の一覧（`polynomial/...`についてはさらに次数ごとにまとめたものも出力されます）


## 正規表現パーサ
入力正規表現は以下をサポートしています．これ以外のすべての文字はその文字そのものにマッチする表現になります．
- `∅`: 空集合
- `ε`: 空文字列
- `r1|r2`: `r1`または`r2`
- `\unnnn`: 16進`nnnn`で表される文字
- `\xnn`: 16進`nnnn`で表される文字
- `\nnn`: 8進`nnn`で表される文字
- `.`: 改行以外の任意の1文字
- 量指定子
  + `r*`: 0回以上の繰り返し
  + `r+`: 1回以上の繰り返し
  + `r?`: 0回または1回
  + `r{n}`: `n`回の繰り返し
  + `r{n,}`: `n`回以上の繰り返し
  + `r{,m}`: `m`回以下の繰り返し
  + `r{n,m}`: `n`回以上`m`回以下の繰り返し
- グループ
  + `(r)`: キャプチャされるグループ
  + `(?:r)`: キャプチャされないグループ
  + `(?<name>r)`,`(?'name'r)`,`(?P<name>r)`: 名前付きグループ
- 文字クラス
  + `[...]`: 指定した文字のどれか1文字
  + `[^...]`: 指定した文字以外の1文字
- 特殊文字
  + `\a`: ベル文字 (`\u0007`)
  + `\e`: エスケープ文字 (`\u001B`)
  + `\f`: 改ページ (`\u000C`)
  + `\n`: 改行 (`\u000A`)
  + `\r`: 復帰 (`\u000D`)
  + `\t`: タブ (`\u0009`)
- 定義済み文字クラス
  + `\d`: 数字 (`[0-9]`)
  + `\D`: 数字以外 (`[^0-9]`)
  + `\h`: 水平タブ (`[\u0009]`)
  + `\H`: 水平タブ以外 (`[^\u0009]`)
  + `\s`: 空白文字 (`[ \t\n\r\f]`)
  + `\S`: 空白文字以外 (`[^ \t\n\r\f]`)
  + `\v`: 垂直タブ (`[\u000B]`)
  + `\V`: 垂直タブ以外 (`[^\u000B]`)
  + `\w`: 単語構成文字 (`[a-zA-Z0-9_]`)
  + `\W`: 単語構成文字以外 (`[^a-zA-Z0-9_]`)
  + `\R`: 改行/復帰 (`[\r\n]`)
- アンカー
  + `^`: 先頭にマッチ
  + `$`: 末尾にマッチ

以下の表現はパーサとしてはサポートしていますが，検査をサポートしておらず結果としては`skipped`を出力します．
- 先読み/後読み
  + `(?=r)`: 肯定先読み
  + `(?!r)`: 否定先読み
  + `(?<=r)`: 肯定後読み
  + `(?<!r)`: 否定後読み
- `(?(r)r1)`,`(?(r)r1|r2)`: 条件分岐
- 後方参照
  + `\1`,`\2`, ...: インデックスによる参照
  + `(?P=name)`,`\k<name>`,`\k'name'`,`\k{name}`: 名前による参照

### エスケープ
英数字以外の文字は，バックスラッシュ`\`でエスケープすることによってその文字そのものにマッチする表現になります．以下に示す文字は，必ずエスケープする必要があります．
+ 文字クラスの外  
`∅`,`ε`,`.`,`|`,`*`,`+`,`?`,`^`,`$`,`(`,`)`,`[`,`]`,`\`
+ 文字クラスの中  
`]`,`\`

### 量指定子
量指定子は標準で貪欲に，つまりなるべく多い回数でマッチします．それぞれ後ろに`?`をつけることで非貪欲になり，なるべく少ない回数でマッチするようになります．

### 文字クラス
文字クラス内では以下の形式をサポートしています．
- `char`: 単一の文字
- `char-char`: 範囲
- 特殊文字
- 定義済み文字クラス

先頭や末尾，定義済み文字クラスの前後に置かれたハイフン`-`は文字としてのハイフンを表します．

文字クラスの中でのみ，以下の特殊文字が使用できます．
- `\b`:後退 (`\u0008`)

### アンカー
正規表現の先頭以外に出現する`^`，末尾以外に出現する`$`はサポートしていません．

### バックスラッシュの後に数字が来る場合の処理
基本的にこちらの仕様に従っています．  
https://www.php.net/manual/ja/regexp.reference.escape.php
- 先頭が`0`の場合  
パターンA
- 先頭が`0`以外の場合  
バックスラッシュの後に続く数字を10進数として読んだものが
  + 9以下，またはキャプチャグループの数（最大99）以下  
  パターンB
  + それ以外  
  パターンA

- パターンA: 7以下の数字を最大3文字読み，8進表現としてパースする．その後に続く数字は文字としての数字としてパースする．
- パターンB: 後方参照としてパースする．ただし，指定した値が表現中のグループの数よりも大きい場合はエラー．

### PCRE形式での修飾子
以下の修飾子をサポートしています．
- `i`: 英字の大文字小文字を無視する
- `s`: `.`が改行にマッチするようにする
- `U`: 量指定子の貪欲/非貪欲を反転する


## バックトラックを模倣するアルゴリズム
- `Lookahead`:
  + https://github.com/minamide-group/group-only/blob/master/tsukuba-thesis/nakagawa-master-thesis.pdf
- `SubsetPrune`:
  + https://link.springer.com/chapter/10.1007/978-3-319-40946-7_27
  + https://github.com/NicolaasWeideman/RegexStaticAnalysis
- `Nondeterminism`:
  + https://www.jalc.de/issues/2018/issue_23_1-3/jalc-2018-019-038.php
- `Exhaustive`: バックトラックの模倣を行わない

- `Exhaustive`を指定すると全探索でマッチングした場合の計算量を判定するため，他のアルゴリズムを指定した場合とは異なる結果が出力される可能性があります．
- `Nondeterminism`を指定した場合，定数時間でマッチングを完了できる場合でも`constant`ではなく`linear`と判定されます．