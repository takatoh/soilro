# soilro

地盤の非線形性状を示す G/G0～γ、h～γ 曲線を作成するプログラム。ROモデルとHDモデルに対応しています。
Haskell製。

## Installation

GitHub からクローンします。

    > git clone git@github.com:takatoh/soilro.git

そして `make build` します。

    > make build

これで、実行ファイル soilro.exe ができます。

## Usage

### 入力ファイル

次のような入力ファイルを作ります。

```
*MODEL
  RO
*GAMMA0.5  // %
  0.150
*HMAX
  0.200
*PLOT      // %
  0.0001
  0.0002
  0.0005
  0.001
  0.002
  0.005
  0.01
  0.02
  0.05
  0.1
  0.2
  0.5
  1.0
  2.0
  5.0
 10.0
*END
```
`//` から行末まではコメントです。
`*MODEL` には `RO`（ROモデル）か `HD`（HDモデル）を指定します。
`*GAMMA0.5` と `*HMAX` はモデルのパラメータです。`*PLOT` には計算させたいひずみを列挙します。
最後に `*END` が必要です。

### 実行

入力ファイル名をパラメータとして実行ファイルに渡します。
結果は標準出力に出力されるので、適当なファイルにリダイレクトしてください。

    > soilro.exe example.dat　> result.txt

### コマンドラインオプション

コマンドラインオプションについては `--help` オプションでヘルプを参照してください。

## License

MIT License
