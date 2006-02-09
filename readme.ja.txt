-------------------------------------------------------------------------------

                                     RLDEV

                  非公認の AVG2000・RealLive 作成ツールキット

-------------------------------------------------------------------------------
          QUICK START GUIDE (WIN32用バイナリ・ディストリビューション)
-------------------------------------------------------------------------------

〔インストール〕

  バイナリのインストールは自動的ではありません。全てのファイルを適当な場所に
  unzip して、そして数環境変RLDEVの値をインストールされたディレクトリ名に
  セットして下さい。


〔操作方法〕

  kprl --help、rlc --help、vaconv --help、rlxml --help というコマンドを実行すれ
  ば、操作方法が表示されます。英語が読めればマニュアルが適当に詳しいですが、現在
  のところは日本語版がありません。

  ～やさしい例え～

  客コンパイル
    kprl -d seen.txt 9030

  コンパイル
    rlc SEEN9030.ke
    kprl -a seen.txt SEEN9030.TXT

  絵変換
    vaconv IMAGE.g00
    vaconv -g2 IMAGE.png -m IMAGE.xml
    
  アニメ変換
    rlxml ANIME.gan
    rlxml ANIME.ganxml

〔ライセンス〕

  Program and documentation are copyright (C) 2006 Peter Jolly (Haeleth).

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation; either version 2 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.

  詳しいライセンスはCOPYINGにあります。

-------------------------------------------------------------------------------
