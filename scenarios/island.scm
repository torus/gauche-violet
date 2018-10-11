(package
 (title "アイランド・ストーリー")
 (desc "Adventure Bookshelves のサンプルシナリオです。定義されている機能が一通り使われています。オチはない。")
 (version "1.0.7")
 (credit "詠み人知らず"))

(stage
 (name "home")
 (label "家"))

(stage
 (name "smith")
 (label "鍛冶屋"))

(stage
 (name "mountain")
 (label "山"))

(character
 (name "girl")
 (label "わたし")
 (image "http://4.bp.blogspot.com/-7ArmuhZRYmE/Wn1Zk_UUyHI/AAAAAAABKNg/uXtlYDHauv8RVt54J0qxKbEGS8jYYCCgACLcBGAs/s800/character_girl_normal.png"))

(character
 (name "mom")
 (label "おかあさん")
 (image "http://3.bp.blogspot.com/-7QNkW1q8Q00/VZ-TiS4FtWI/AAAAAAAAvRQ/I6bIWDKq-lo/s800/obasan03_smile.png"))

(character
 (name "smith")
 (label "おじさん")
 (image "http://2.bp.blogspot.com/-O9HwTlFMdFI/UWgWd04Hq1I/AAAAAAAAQDw/zlpLB4khUjc/s1600/hige_busyo.png"))

(character
 (name "postman")
 (label "郵便屋さん"))

(scene
 (name "1-1-1-home")
 (stage "home")
 (desc
  "ここは山間の村です。"
  "この物語の主人公の女の子はお母さんと二人暮らしをしています。")
 (speak
  "girl"
  "おはよう、おかあさん")
 (speak
  "mom"
  "おはよう。朝ごはんをおあがり。")
 (desc
  "トントン、とドアを叩く音がします。"
  "郵便屋さんが来たようです。")
 (speak
  "postman"
  "おはよう。おじょうちゃんにお手紙だよ。"
  "浜辺に住むきみのお爺さんからだ。")
 (action
  (label "手紙を受け取る")
  (after
   (speak
    "girl"
    "ありがとう、郵便屋さん。")
   (link "1-1-2-home"))))

(scene
 (name "1-1-2-home")
 (stage "home")
 (desc "おかあさんに手紙を読んでもらいました。")
 (speak
  "mom"
  "おじいちゃんがいつも使っている包丁が壊れてしまったので、新しいのを作って持って来て欲しいそうよ。"
  "鍛冶屋さんに包丁を作ってもらって。"
  "それを海沿いの村のおじいちゃんの家にとどけてちょうだい。")
 (link "1-2-smith"))
