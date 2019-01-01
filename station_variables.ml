(* ekimei_t *)
(*  station - 駅を表すレコード 
    name - 駅名
    kana - ひらがなになった駅名
    romaji - ローマ字になった駅名
    belonging_route - 所属路線 
*)
type station = {
  name   : string; 
  kana   : string; 
  romaji : string;
  belonging_route : string;
};;

(* ekikan_t *)
(*  station_interval - 駅間を表すレコード
    starting_point - 開始駅
    end_point - 終了駅
    via - 経由駅
    distance - 距離  
    time - 開始駅から終了駅に行くまでの時間 
*)
type station_interval = {
  starting_point  : string;
  end_point       : string;
  via             : string;
  distance        : float;
  time            : int;
};;

(* eki_t *)
(*  dijkstra_station - ダイグストラアルゴリズム用の駅レコード
    d_name - 駅名
    d_shortest_distance - その駅が持つ最短経路の距離
    d_name_list - その駅が、アルゴリズムが始まった時お開始駅から辿ってきた駅のリスト
*)
type dijkstra_station = {
  d_name      : string;
  d_shortest_distance  : float;
  d_name_list : string list;
};;

(* ekikan_tree_t *)
(* station_tree - 駅をツリー構造にしたバリアント
   Empty - 空を示す
   Node - ２分木の枝
     station_tree(左) - 自己参照
     string - 駅名
     (string * float)list - [("Bの駅名" * "AからBの距離"), ("Cの駅名" * "AからCの距離")]
     station_tree(右) - 自己参照
*)
type station_tree =  
  Empty | 
  Node of station_tree  *
          string * 
          (string * float) list *
          station_tree
;;
                              
let all_station =
  [ 
    {name="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; belonging_route="千代田線"}; 
    {name="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; belonging_route="千代田線"}; 
    {name="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; belonging_route="千代田線"}; 
    {name="表参道"; kana="おもてさんどう"; romaji="omotesandou"; belonging_route="千代田線"}; 
    {name="乃木坂"; kana="のぎざか"; romaji="nogizaka"; belonging_route="千代田線"}; 
    {name="赤坂"; kana="あかさか"; romaji="akasaka"; belonging_route="千代田線"}; 
    {name="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidomae"; belonging_route="千代田線"}; 
    {name="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; belonging_route="千代田線"}; 
    {name="日比谷"; kana="ひびや"; romaji="hibiya"; belonging_route="千代田線"}; 
    {name="二重橋前"; kana="にじゅうばしまえ"; romaji="nijuubasimae"; belonging_route="千代田線"}; 
    {name="大手町"; kana="おおてまち"; romaji="otemachi"; belonging_route="千代田線"}; 
    {name="新御茶ノ水"; kana="しんおちゃのみず"; romaji="shin-ochanomizu"; belonging_route="千代田線"}; 
    {name="湯島"; kana="ゆしま"; romaji="yushima"; belonging_route="千代田線"}; 
    {name="根津"; kana="ねづ"; romaji="nedu"; belonging_route="千代田線"}; 
    {name="千駄木"; kana="せんだぎ"; romaji="sendagi"; belonging_route="千代田線"}; 
    {name="西日暮里"; kana="にしにっぽり"; romaji="nishinippori"; belonging_route="千代田線"}; 
    {name="町屋"; kana="まちや"; romaji="machiya"; belonging_route="千代田線"}; 
    {name="北千住"; kana="きたせんじゅ"; romaji="kitasenjyu"; belonging_route="千代田線"}; 
    {name="綾瀬"; kana="あやせ"; romaji="ayase"; belonging_route="千代田線"}; 
    {name="北綾瀬"; kana="きたあやせ"; romaji="kitaayase"; belonging_route="千代田線"}; 
    {name="浅草"; kana="あさくさ"; romaji="asakusa"; belonging_route="銀座線"}; 
    {name="田原町"; kana="たわらまち"; romaji="tawaramachi"; belonging_route="銀座線"}; 
    {name="稲荷町"; kana="いなりちょう"; romaji="inaricho"; belonging_route="銀座線"}; 
    {name="上野"; kana="うえの"; romaji="ueno"; belonging_route="銀座線"}; 
    {name="上野広小路"; kana="うえのひろこうじ"; romaji="uenohirokoji"; belonging_route="銀座線"}; 
    {name="末広町"; kana="すえひろちょう"; romaji="suehirocho"; belonging_route="銀座線"}; 
    {name="神田"; kana="かんだ"; romaji="kanda"; belonging_route="銀座線"}; 
    {name="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; belonging_route="銀座線"}; 
    {name="日本橋"; kana="にほんばし"; romaji="nihonbashi"; belonging_route="銀座線"}; 
    {name="京橋"; kana="きょうばし"; romaji="kyobashi"; belonging_route="銀座線"}; 
    {name="銀座"; kana="ぎんざ"; romaji="ginza"; belonging_route="銀座線"}; 
    {name="新橋"; kana="しんばし"; romaji="shinbashi"; belonging_route="銀座線"}; 
    {name="虎ノ門"; kana="とらのもん"; romaji="toranomon"; belonging_route="銀座線"}; 
    {name="溜池山王"; kana="ためいけさんのう"; romaji="tameikesannou"; belonging_route="銀座線"}; 
    {name="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; belonging_route="銀座線"}; 
    {name="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyamaicchome"; belonging_route="銀座線"}; 
    {name="外苑前"; kana="がいえんまえ"; romaji="gaienmae"; belonging_route="銀座線"}; 
    {name="表参道"; kana="おもてさんどう"; romaji="omotesando"; belonging_route="銀座線"}; 
    {name="渋谷"; kana="しぶや"; romaji="shibuya"; belonging_route="銀座線"}; 
    {name="渋谷"; kana="しぶや"; romaji="shibuya"; belonging_route="半蔵門線"}; 
    {name="表参道"; kana="おもてさんどう"; romaji="omotesandou"; belonging_route="半蔵門線"}; 
    {name="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyama-itchome"; belonging_route="半蔵門線"}; 
    {name="永田町"; kana="ながたちょう"; romaji="nagatacho"; belonging_route="半蔵門線"}; 
    {name="半蔵門"; kana="はんぞうもん"; romaji="hanzomon"; belonging_route="半蔵門線"}; 
    {name="九段下"; kana="くだんした"; romaji="kudanshita"; belonging_route="半蔵門線"}; 
    {name="神保町"; kana="じんぼうちょう"; romaji="jinbocho"; belonging_route="半蔵門線"}; 
    {name="大手町"; kana="おおてまち"; romaji="otemachi"; belonging_route="半蔵門線"}; 
    {name="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; belonging_route="半蔵門線"}; 
    {name="水天宮前"; kana="すいてんぐうまえ"; romaji="suitengumae"; belonging_route="半蔵門線"}; 
    {name="清澄白河"; kana="きよすみしらかわ"; romaji="kiyosumi-shirakawa"; belonging_route="半蔵門線"}; 
    {name="住吉"; kana="すみよし"; romaji="sumiyoshi"; belonging_route="半蔵門線"}; 
    {name="錦糸町"; kana="きんしちょう"; romaji="kinshicho"; belonging_route="半蔵門線"}; 
    {name="押上"; kana="おしあげ"; romaji="oshiage"; belonging_route="半蔵門線"}; 
    {name="中目黒"; kana="なかめぐろ"; romaji="nakameguro"; belonging_route="日比谷線"}; 
    {name="恵比寿"; kana="えびす"; romaji="ebisu"; belonging_route="日比谷線"}; 
    {name="広尾"; kana="ひろお"; romaji="hiro"; belonging_route="日比谷線"}; 
    {name="六本木"; kana="ろっぽんぎ"; romaji="roppongi"; belonging_route="日比谷線"}; 
    {name="神谷町"; kana="かみやちょう"; romaji="kamiyacho"; belonging_route="日比谷線"}; 
    {name="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; belonging_route="日比谷線"}; 
    {name="日比谷"; kana="ひびや"; romaji="hibiya"; belonging_route="日比谷線"}; 
    {name="銀座"; kana="ぎんざ"; romaji="ginza"; belonging_route="日比谷線"}; 
    {name="東銀座"; kana="ひがしぎんざ"; romaji="higashiginza"; belonging_route="日比谷線"}; 
    {name="築地"; kana="つきじ"; romaji="tsukiji"; belonging_route="日比谷線"}; 
    {name="八丁堀"; kana="はっちょうぼり"; romaji="hacchobori"; belonging_route="日比谷線"}; 
    {name="茅場町"; kana="かやばちょう"; romaji="kayabacho"; belonging_route="日比谷線"}; 
    {name="人形町"; kana="にんぎょうちょう"; romaji="ningyomachi"; belonging_route="日比谷線"}; 
    {name="小伝馬町"; kana="こでんまちょう"; romaji="kodemmacho"; belonging_route="日比谷線"}; 
    {name="秋葉原"; kana="あきはばら"; romaji="akihabara"; belonging_route="日比谷線"}; 
    {name="仲御徒町"; kana="なかおかちまち"; romaji="nakaokachimachi"; belonging_route="日比谷線"}; 
    {name="上野"; kana="うえの"; romaji="ueno"; belonging_route="日比谷線"}; 
    {name="入谷"; kana="いりや"; romaji="iriya"; belonging_route="日比谷線"}; 
    {name="三ノ輪"; kana="みのわ"; romaji="minowa"; belonging_route="日比谷線"}; 
    {name="南千住"; kana="みなみせんじゅ"; romaji="minamisenju"; belonging_route="日比谷線"}; 
    {name="北千住"; kana="きたせんじゅ"; romaji="kitasenju"; belonging_route="日比谷線"}; 
    {name="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; belonging_route="丸ノ内線"}; 
    {name="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; belonging_route="丸ノ内線"}; 
    {name="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; belonging_route="丸ノ内線"}; 
    {name="後楽園"; kana="こうらくえん"; romaji="korakuen"; belonging_route="丸ノ内線"}; 
    {name="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; belonging_route="丸ノ内線"}; 
    {name="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; belonging_route="丸ノ内線"}; 
    {name="淡路町"; kana="あわじちょう"; romaji="awajicho"; belonging_route="丸ノ内線"}; 
    {name="大手町"; kana="おおてまち"; romaji="otemachi"; belonging_route="丸ノ内線"}; 
    {name="東京"; kana="とうきょう"; romaji="tokyo"; belonging_route="丸ノ内線"}; 
    {name="銀座"; kana="ぎんざ"; romaji="ginza"; belonging_route="丸ノ内線"}; 
    {name="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; belonging_route="丸ノ内線"}; 
    {name="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidomae"; belonging_route="丸ノ内線"}; 
    {name="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; belonging_route="丸ノ内線"}; 
    {name="四ツ谷"; kana="よつや"; romaji="yotsuya"; belonging_route="丸ノ内線"}; 
    {name="四谷三丁目"; kana="よつやさんちょうめ"; romaji="yotsuyasanchome"; belonging_route="丸ノ内線"}; 
    {name="新宿御苑前"; kana="しんじゅくぎょえんまえ"; romaji="shinjuku-gyoemmae"; belonging_route="丸ノ内線"}; 
    {name="新宿三丁目"; kana="しんじゅくさんちょうめ"; romaji="shinjuku-sanchome"; belonging_route="丸ノ内線"}; 
    {name="新宿"; kana="しんじゅく"; romaji="shinjuku"; belonging_route="丸ノ内線"}; 
    {name="西新宿"; kana="にししんじゅく"; romaji="nishi-shinjuku"; belonging_route="丸ノ内線"}; 
    {name="中野坂上"; kana="なかのさかうえ"; romaji="nakano-sakaue"; belonging_route="丸ノ内線"}; 
    {name="新中野"; kana="しんなかの"; romaji="shin-nakano"; belonging_route="丸ノ内線"}; 
    {name="東高円寺"; kana="ひがしこうえんじ"; romaji="higashi-koenji"; belonging_route="丸ノ内線"}; 
    {name="新高円寺"; kana="しんこうえんじ"; romaji="shin-koenji"; belonging_route="丸ノ内線"}; 
    {name="南阿佐ヶ谷"; kana="みなみあさがや"; romaji="minami-asagaya"; belonging_route="丸ノ内線"}; 
    {name="荻窪"; kana="おぎくぼ"; romaji="ogikubo"; belonging_route="丸ノ内線"}; 
    {name="中野新橋"; kana="なかのしんばし"; romaji="nakano-shimbashi"; belonging_route="丸ノ内線"}; 
    {name="中野富士見町"; kana="なかのふじみちょう"; romaji="nakano-fujimicho"; belonging_route="丸ノ内線"}; 
    {name="方南町"; kana="ほうなんちょう"; romaji="honancho"; belonging_route="丸ノ内線"}; 
    {name="四ツ谷"; kana="よつや"; romaji="yotsuya"; belonging_route="南北線"}; 
    {name="永田町"; kana="ながたちょう"; romaji="nagatacho"; belonging_route="南北線"}; 
    {name="溜池山王"; kana="ためいけさんのう"; romaji="tameikesanno"; belonging_route="南北線"}; 
    {name="六本木一丁目"; kana="ろっぽんぎいっちょうめ"; romaji="roppongiitchome"; belonging_route="南北線"}; 
    {name="麻布十番"; kana="あざぶじゅうばん"; romaji="azabujuban"; belonging_route="南北線"}; 
    {name="白金高輪"; kana="しろかねたかなわ"; romaji="shirokanetakanawa"; belonging_route="南北線"}; 
    {name="白金台"; kana="しろかねだい"; romaji="shirokanedai"; belonging_route="南北線"}; 
    {name="目黒"; kana="めぐろ"; romaji="meguro"; belonging_route="南北線"}; 
    {name="市ヶ谷"; kana="いちがや"; romaji="ichigaya"; belonging_route="南北線"}; 
    {name="飯田橋"; kana="いいだばし"; romaji="idabashi"; belonging_route="南北線"}; 
    {name="後楽園"; kana="こうらくえん"; romaji="korakuen"; belonging_route="南北線"}; 
    {name="東大前"; kana="とうだいまえ"; romaji="todaimae"; belonging_route="南北線"}; 
    {name="本駒込"; kana="ほんこまごめ"; romaji="honkomagome"; belonging_route="南北線"}; 
    {name="駒込"; kana="こまごめ"; romaji="komagome"; belonging_route="南北線"}; 
    {name="西ヶ原"; kana="にしがはら"; romaji="nishigahara"; belonging_route="南北線"}; 
    {name="王子"; kana="おうじ"; romaji="oji"; belonging_route="南北線"}; 
    {name="王子神谷"; kana="おうじかみや"; romaji="ojikamiya"; belonging_route="南北線"}; 
    {name="志茂"; kana="しも"; romaji="shimo"; belonging_route="南北線"}; 
    {name="赤羽岩淵"; kana="あかばねいわぶち"; romaji="akabaneiwabuchi"; belonging_route="南北線"}; 
    {name="西船橋"; kana="にしふなばし"; romaji="nishi-funabashi"; belonging_route="東西線"}; 
    {name="原木中山"; kana="ばらきなかやま"; romaji="baraki-nakayama"; belonging_route="東西線"}; 
    {name="妙典"; kana="みょうでん"; romaji="myoden"; belonging_route="東西線"}; 
    {name="行徳"; kana="ぎょうとく"; romaji="gyotoku"; belonging_route="東西線"}; 
    {name="南行徳"; kana="みなみぎょうとく"; romaji="minami-gyotoku"; belonging_route="東西線"}; 
    {name="浦安"; kana="うらやす"; romaji="urayasu"; belonging_route="東西線"}; 
    {name="葛西"; kana="かさい"; romaji="kasai"; belonging_route="東西線"}; 
    {name="西葛西"; kana="にしかさい"; romaji="nishi-kasai"; belonging_route="東西線"}; 
    {name="南砂町"; kana="みなみすなまち"; romaji="minami-sunamachi"; belonging_route="東西線"}; 
    {name="東陽町"; kana="とうようちょう"; romaji="touyoucho"; belonging_route="東西線"}; 
    {name="木場"; kana="きば"; romaji="kiba"; belonging_route="東西線"}; 
    {name="門前仲町"; kana="もんぜんなかちょう"; romaji="monzen-nakacho"; belonging_route="東西線"}; 
    {name="茅場町"; kana="かやばちょう"; romaji="kayabacho"; belonging_route="東西線"}; 
    {name="日本橋"; kana="にほんばし"; romaji="nihonbashi"; belonging_route="東西線"}; 
    {name="大手町"; kana="おおてまち"; romaji="otemachi"; belonging_route="東西線"}; 
    {name="竹橋"; kana="たけばし"; romaji="takebashi"; belonging_route="東西線"}; 
    {name="九段下"; kana="くだんした"; romaji="kudanshita"; belonging_route="東西線"}; 
    {name="飯田橋"; kana="いいだばし"; romaji="iidabashi"; belonging_route="東西線"}; 
    {name="神楽坂"; kana="かぐらざか"; romaji="kagurazaka"; belonging_route="東西線"}; 
    {name="早稲田"; kana="わせだ"; romaji="waseda"; belonging_route="東西線"}; 
    {name="高田馬場"; kana="たかだのばば"; romaji="takadanobaba"; belonging_route="東西線"}; 
    {name="落合"; kana="おちあい"; romaji="ochiai"; belonging_route="東西線"}; 
    {name="中野"; kana="なかの"; romaji="nakano"; belonging_route="東西線"}; 
    {romaji="shinkiba"; kana="しんきば"; name="新木場"; belonging_route="有楽町線"}; 
    {romaji="tatsumi"; kana="たつみ"; name="辰巳"; belonging_route="有楽町線"}; 
    {romaji="toyosu"; kana="とよす"; name="豊洲"; belonging_route="有楽町線"}; 
    {romaji="tsukishima"; kana="つきしま"; name="月島"; belonging_route="有楽町線"}; 
    {romaji="shintomityou"; kana="しんとみちょう"; name="新富町"; belonging_route="有楽町線"}; 
    {romaji="ginzaittyoume"; kana="ぎんざいっちょうめ"; name="銀座一丁目"; belonging_route="有楽町線"}; 
    {romaji="yuurakutyou"; kana="ゆうらくちょう"; name="有楽町"; belonging_route="有楽町線"}; 
    {romaji="sakuradamon"; kana="さくらだもん"; name="桜田門"; belonging_route="有楽町線"}; 
    {romaji="nagatacho"; kana="ながたちょう"; name="永田町"; belonging_route="有楽町線"}; 
    {romaji="koujimachi"; kana="こうじまち"; name="麹町"; belonging_route="有楽町線"}; 
    {romaji="ichigaya"; kana="いちがや"; name="市ヶ谷"; belonging_route="有楽町線"}; 
    {romaji="iidabashi"; kana="いいだばし"; name="飯田橋"; belonging_route="有楽町線"}; 
    {name="江戸川橋"; kana="えどがわばし"; romaji="edogawabasi"; belonging_route="有楽町線"}; 
    {name="護国寺"; kana="ごこくじ"; romaji="gokokuji"; belonging_route="有楽町線"}; 
    {name="東池袋"; kana="ひがしいけぶくろ"; romaji="higasiikebukuro"; belonging_route="有楽町線"}; 
    {name="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; belonging_route="有楽町線"}; 
    {name="要町"; kana="かなめちょう"; romaji="kanametyou"; belonging_route="有楽町線"}; 
    {name="千川"; kana="せんかわ"; romaji="senkawa"; belonging_route="有楽町線"}; 
    {name="小竹向原"; kana="こたけむかいはら"; romaji="kotakemukaihara"; belonging_route="有楽町線"}; 
    {name="氷川台"; kana="ひかわだい"; romaji="hikawadai"; belonging_route="有楽町線"}; 
    {name="平和台"; kana="へいわだい"; romaji="heiwadai"; belonging_route="有楽町線"}; 
    {name="営団赤塚"; kana="えいだんあかつか"; romaji="eidanakakuka"; belonging_route="有楽町線"}; 
    {name="営団成増"; kana="えいだんなります"; romaji="eidannarimasu"; belonging_route="有楽町線"}; 
    {name="和光市"; kana="わこうし"; romaji="wakousi"; belonging_route="有楽町線"}; 
  ] 
;;

let all_station_interval =
  [
    {starting_point="代々木上原"; end_point="代々木公園"; via="千代田線"; distance=1.0; time=2}; 
    {starting_point="代々木公園"; end_point="明治神宮前"; via="千代田線"; distance=1.2; time=2}; 
    {starting_point="明治神宮前"; end_point="表参道"; via="千代田線"; distance=0.9; time=2}; 
    {starting_point="表参道"; end_point="乃木坂"; via="千代田線"; distance=1.4; time=3}; 
    {starting_point="乃木坂"; end_point="赤坂"; via="千代田線"; distance=1.1; time=2}; 
    {starting_point="赤坂"; end_point="国会議事堂前"; via="千代田線"; distance=0.8; time=1}; 
    {starting_point="国会議事堂前"; end_point="霞ヶ関"; via="千代田線"; distance=0.7; time=1}; 
    {starting_point="霞ヶ関"; end_point="日比谷"; via="千代田線"; distance=1.2; time=2}; 
    {starting_point="日比谷"; end_point="二重橋前"; via="千代田線"; distance=0.7; time=1}; 
    {starting_point="二重橋前"; end_point="大手町"; via="千代田線"; distance=0.7; time=1}; 
    {starting_point="大手町"; end_point="新御茶ノ水"; via="千代田線"; distance=1.3; time=2}; 
    {starting_point="新御茶ノ水"; end_point="湯島"; via="千代田線"; distance=1.2; time=2}; 
    {starting_point="湯島"; end_point="根津"; via="千代田線"; distance=1.2; time=2}; 
    {starting_point="根津"; end_point="千駄木"; via="千代田線"; distance=1.0; time=2}; 
    {starting_point="千駄木"; end_point="西日暮里"; via="千代田線"; distance=0.9; time=1}; 
    {starting_point="西日暮里"; end_point="町屋"; via="千代田線"; distance=1.7; time=2}; 
    {starting_point="町屋"; end_point="北千住"; via="千代田線"; distance=2.6; time=3}; 
    {starting_point="北千住"; end_point="綾瀬"; via="千代田線"; distance=2.5; time=3}; 
    {starting_point="綾瀬"; end_point="北綾瀬"; via="千代田線"; distance=2.1; time=4}; 
    {starting_point="浅草"; end_point="田原町"; via="銀座線"; distance=0.8; time=2}; 
    {starting_point="田原町"; end_point="稲荷町"; via="銀座線"; distance=0.7; time=1}; 
    {starting_point="稲荷町"; end_point="上野"; via="銀座線"; distance=0.7; time=2}; 
    {starting_point="上野"; end_point="上野広小路"; via="銀座線"; distance=0.5; time=2}; 
    {starting_point="上野広小路"; end_point="末広町"; via="銀座線"; distance=0.6; time=1}; 
    {starting_point="末広町"; end_point="神田"; via="銀座線"; distance=1.1; time=2}; 
    {starting_point="神田"; end_point="三越前"; via="銀座線"; distance=0.7; time=1}; 
    {starting_point="三越前"; end_point="日本橋"; via="銀座線"; distance=0.6; time=2}; 
    {starting_point="日本橋"; end_point="京橋"; via="銀座線"; distance=0.7; time=2}; 
    {starting_point="京橋"; end_point="銀座"; via="銀座線"; distance=0.7; time=1}; 
    {starting_point="銀座"; end_point="新橋"; via="銀座線"; distance=0.9; time=2}; 
    {starting_point="新橋"; end_point="虎ノ門"; via="銀座線"; distance=0.8; time=2}; 
    {starting_point="虎ノ門"; end_point="溜池山王"; via="銀座線"; distance=0.6; time=2}; 
    {starting_point="溜池山王"; end_point="赤坂見附"; via="銀座線"; distance=0.9; time=2}; 
    {starting_point="赤坂見附"; end_point="青山一丁目"; via="銀座線"; distance=1.3; time=2}; 
    {starting_point="青山一丁目"; end_point="外苑前"; via="銀座線"; distance=0.7; time=2}; 
    {starting_point="外苑前"; end_point="表参道"; via="銀座線"; distance=0.7; time=1}; 
    {starting_point="表参道"; end_point="渋谷"; via="銀座線"; distance=1.3; time=1}; 
    {starting_point="渋谷"; end_point="表参道"; via="半蔵門線"; distance=1.3; time=2}; 
    {starting_point="表参道"; end_point="青山一丁目"; via="半蔵門線"; distance=1.4; time=2}; 
    {starting_point="青山一丁目"; end_point="永田町"; via="半蔵門線"; distance=1.3; time=2}; 
    {starting_point="永田町"; end_point="半蔵門"; via="半蔵門線"; distance=1.0; time=2}; 
    {starting_point="半蔵門"; end_point="九段下"; via="半蔵門線"; distance=1.6; time=2}; 
    {starting_point="九段下"; end_point="神保町"; via="半蔵門線"; distance=0.4; time=1}; 
    {starting_point="神保町"; end_point="大手町"; via="半蔵門線"; distance=1.7; time=3}; 
    {starting_point="大手町"; end_point="三越前"; via="半蔵門線"; distance=0.7; time=1}; 
    {starting_point="三越前"; end_point="水天宮前"; via="半蔵門線"; distance=1.3; time=2}; 
    {starting_point="水天宮前"; end_point="清澄白河"; via="半蔵門線"; distance=1.7; time=3}; 
    {starting_point="清澄白河"; end_point="住吉"; via="半蔵門線"; distance=1.9; time=3}; 
    {starting_point="住吉"; end_point="錦糸町"; via="半蔵門線"; distance=1.; time=2}; 
    {starting_point="錦糸町"; end_point="押上"; via="半蔵門線"; distance=1.4; time=2}; 
    {starting_point="中目黒"; end_point="恵比寿"; via="日比谷線"; distance=1.; time=2}; 
    {starting_point="恵比寿"; end_point="広尾"; via="日比谷線"; distance=1.5; time=3}; 
    {starting_point="広尾"; end_point="六本木"; via="日比谷線"; distance=1.7; time=3}; 
    {starting_point="六本木"; end_point="神谷町"; via="日比谷線"; distance=1.5; time=3}; 
    {starting_point="神谷町"; end_point="霞ヶ関"; via="日比谷線"; distance=1.3; time=2}; 
    {starting_point="霞ヶ関"; end_point="日比谷"; via="日比谷線"; distance=1.2; time=2}; 
    {starting_point="日比谷"; end_point="銀座"; via="日比谷線"; distance=0.4; time=1}; 
    {starting_point="銀座"; end_point="東銀座"; via="日比谷線"; distance=0.4; time=1}; 
    {starting_point="東銀座"; end_point="築地"; via="日比谷線"; distance=0.6; time=2}; 
    {starting_point="築地"; end_point="八丁堀"; via="日比谷線"; distance=1.; time=2}; 
    {starting_point="八丁堀"; end_point="茅場町"; via="日比谷線"; distance=0.5; time=1}; 
    {starting_point="茅場町"; end_point="人形町"; via="日比谷線"; distance=0.9; time=2}; 
    {starting_point="人形町"; end_point="小伝馬町"; via="日比谷線"; distance=0.6; time=1}; 
    {starting_point="小伝馬町"; end_point="秋葉原"; via="日比谷線"; distance=0.9; time=2}; 
    {starting_point="秋葉原"; end_point="仲御徒町"; via="日比谷線"; distance=1.; time=1}; 
    {starting_point="仲御徒町"; end_point="上野"; via="日比谷線"; distance=0.5; time=1}; 
    {starting_point="上野"; end_point="入谷"; via="日比谷線"; distance=1.2; time=2}; 
    {starting_point="入谷"; end_point="三ノ輪"; via="日比谷線"; distance=1.2; time=2}; 
    {starting_point="三ノ輪"; end_point="南千住"; via="日比谷線"; distance=0.8; time=2}; 
    {starting_point="南千住"; end_point="北千住"; via="日比谷線"; distance=1.8; time=3}; 
    {starting_point="池袋"; end_point="新大塚"; via="丸ノ内線"; distance=1.8; time=3}; 
    {starting_point="新大塚"; end_point="茗荷谷"; via="丸ノ内線"; distance=1.2; time=2}; 
    {starting_point="茗荷谷"; end_point="後楽園"; via="丸ノ内線"; distance=1.8; time=2}; 
    {starting_point="後楽園"; end_point="本郷三丁目"; via="丸ノ内線"; distance=0.8; time=1}; 
    {starting_point="本郷三丁目"; end_point="御茶ノ水"; via="丸ノ内線"; distance=0.8; time=1}; 
    {starting_point="御茶ノ水"; end_point="淡路町"; via="丸ノ内線"; distance=0.8; time=1}; 
    {starting_point="淡路町"; end_point="大手町"; via="丸ノ内線"; distance=0.9; time=2}; 
    {starting_point="大手町"; end_point="東京"; via="丸ノ内線"; distance=0.6; time=1}; 
    {starting_point="東京"; end_point="銀座"; via="丸ノ内線"; distance=1.1; time=2}; 
    {starting_point="銀座"; end_point="霞ヶ関"; via="丸ノ内線"; distance=1.0; time=2}; 
    {starting_point="霞ヶ関"; end_point="国会議事堂前"; via="丸ノ内線"; distance=0.7; time=1}; 
    {starting_point="国会議事堂前"; end_point="赤坂見附"; via="丸ノ内線"; distance=0.9; time=2}; 
    {starting_point="赤坂見附"; end_point="四ツ谷"; via="丸ノ内線"; distance=1.3; time=2}; 
    {starting_point="四ツ谷"; end_point="四谷三丁目"; via="丸ノ内線"; distance=1.0; time=2}; 
    {starting_point="四谷三丁目"; end_point="新宿御苑前"; via="丸ノ内線"; distance=0.9; time=1}; 
    {starting_point="新宿御苑前"; end_point="新宿三丁目"; via="丸ノ内線"; distance=0.7; time=1}; 
    {starting_point="新宿三丁目"; end_point="新宿"; via="丸ノ内線"; distance=0.3; time=1}; 
    {starting_point="新宿"; end_point="西新宿"; via="丸ノ内線"; distance=0.8; time=1}; 
    {starting_point="西新宿"; end_point="中野坂上"; via="丸ノ内線"; distance=1.1; time=2}; 
    {starting_point="中野坂上"; end_point="新中野"; via="丸ノ内線"; distance=1.1; time=2}; 
    {starting_point="新中野"; end_point="東高円寺"; via="丸ノ内線"; distance=1.0; time=1}; 
    {starting_point="東高円寺"; end_point="新高円寺"; via="丸ノ内線"; distance=0.9; time=1}; 
    {starting_point="新高円寺"; end_point="南阿佐ヶ谷"; via="丸ノ内線"; distance=1.2; time=2}; 
    {starting_point="南阿佐ヶ谷"; end_point="荻窪"; via="丸ノ内線"; distance=1.5; time=2}; 
    {starting_point="中野坂上"; end_point="中野新橋"; via="丸ノ内線"; distance=1.3; time=2}; 
    {starting_point="中野新橋"; end_point="中野富士見町"; via="丸ノ内線"; distance=0.6; time=1}; 
    {starting_point="中野富士見町"; end_point="方南町"; via="丸ノ内線"; distance=1.3; time=2}; 
    {starting_point="市ヶ谷"; end_point="四ツ谷"; via="南北線"; distance=1.0; time=2}; 
    {starting_point="四ツ谷"; end_point="永田町"; via="南北線"; distance=1.3; time=3}; 
    {starting_point="永田町"; end_point="溜池山王"; via="南北線"; distance=0.9; time=1}; 
    {starting_point="溜池山王"; end_point="六本木一丁目"; via="南北線"; distance=0.9; time=2}; 
    {starting_point="六本木一丁目"; end_point="麻布十番"; via="南北線"; distance=1.2; time=2}; 
    {starting_point="麻布十番"; end_point="白金高輪"; via="南北線"; distance=1.3; time=2}; 
    {starting_point="白金高輪"; end_point="白金台"; via="南北線"; distance=1.0; time=2}; 
    {starting_point="白金台"; end_point="目黒"; via="南北線"; distance=1.3; time=2}; 
    {starting_point="市ヶ谷"; end_point="飯田橋"; via="南北線"; distance=1.1 ; time=2}; 
    {starting_point="飯田橋"; end_point="後楽園"; via="南北線"; distance=1.4 ; time=2}; 
    {starting_point="後楽園"; end_point="東大前"; via="南北線"; distance=1.3 ; time=3}; 
    {starting_point="東大前"; end_point="本駒込"; via="南北線"; distance=0.9 ; time=2}; 
    {starting_point="本駒込"; end_point="駒込"; via="南北線"; distance=1.4; time=2}; 
    {starting_point="駒込"; end_point="西ヶ原"; via="南北線"; distance=1.4; time=2}; 
    {starting_point="西ヶ原"; end_point="王子"; via="南北線"; distance=1.0; time=2}; 
    {starting_point="王子"; end_point="王子神谷"; via="南北線"; distance=1.2; time=2}; 
    {starting_point="王子神谷"; end_point="志茂"; via="南北線"; distance=1.6; time=3}; 
    {starting_point="志茂"; end_point="赤羽岩淵"; via="南北線"; distance=1.1; time=2}; 
    {starting_point="西船橋" ; end_point="原木中山"; via="東西線"; distance=1.9; time=3}; 
    {starting_point="原木中山"; end_point="妙典"; via="東西線"; distance=2.1 ; time=2}; 
    {starting_point="妙典"; end_point="行徳"; via="東西線"; distance=1.3 ; time=2}; 
    {starting_point="行徳"; end_point="南行徳"; via="東西線"; distance=1.5 ; time=2}; 
    {starting_point="南行徳"; end_point="浦安" ; via="東西線"; distance=1.2 ; time=2}; 
    {starting_point="浦安" ; end_point="葛西"; via="東西線"; distance=1.9 ; time=2}; 
    {starting_point="葛西"; end_point="西葛西"; via="東西線"; distance=1.2 ; time=2}; 
    {starting_point="西葛西"; end_point="南砂町"; via="東西線"; distance=2.7 ; time=2}; 
    {starting_point="南砂町"; end_point="東陽町"; via="東西線"; distance=1.2 ; time=2}; 
    {starting_point="東陽町"; end_point="木場" ; via="東西線"; distance=0.9 ; time=1}; 
    {starting_point="木場"; end_point="門前仲町"; via="東西線"; distance=1.1 ; time=1}; 
    {starting_point="門前仲町"; end_point="茅場町"; via="東西線"; distance=1.8 ; time=2}; 
    {starting_point="茅場町"; end_point="日本橋"; via="東西線"; distance=0.5 ; time=1}; 
    {starting_point="日本橋"; end_point="大手町"; via="東西線"; distance=0.8 ; time=1}; 
    {starting_point="大手町"; end_point="竹橋"; via="東西線"; distance=1.0; time=2}; 
    {starting_point="竹橋"; end_point="九段下"; via="東西線"; distance=1.0; time=1}; 
    {starting_point="九段下"; end_point="飯田橋"; via="東西線"; distance=0.7; time=1}; 
    {starting_point="飯田橋"; end_point="神楽坂"; via="東西線"; distance=1.2; time=2}; 
    {starting_point="神楽坂"; end_point="早稲田"; via="東西線"; distance=1.2; time=2}; 
    {starting_point="早稲田"; end_point="高田馬場"; via="東西線"; distance=1.7; time=3}; 
    {starting_point="高田馬場"; end_point="落合"; via="東西線"; distance=1.9; time=3}; 
    {starting_point="落合"; end_point="中野"; via="東西線"; distance=2.0; time=3}; 
    {starting_point="新木場"; end_point="辰巳"; via="有楽町線"; distance=1.5; time=2}; 
    {starting_point="辰巳"; end_point="豊洲"; via="有楽町線"; distance=1.7; time=2}; 
    {starting_point="豊洲"; end_point="月島"; via="有楽町線"; distance=1.4; time=2}; 
    {starting_point="月島"; end_point="新富町"; via="有楽町線"; distance=1.3; time=2}; 
    {starting_point="新富町"; end_point="銀座一丁目"; via="有楽町線"; distance=0.7; time=1}; 
    {starting_point="銀座一丁目"; end_point="有楽町"; via="有楽町線"; distance=0.5; time=1}; 
    {starting_point="有楽町"; end_point="桜田門"; via="有楽町線"; distance=1.0; time=1}; 
    {starting_point="桜田門"; end_point="永田町"; via="有楽町線"; distance=0.9; time=2}; 
    {starting_point="永田町"; end_point="麹町"; via="有楽町線"; distance=0.9; time=1}; 
    {starting_point="麹町"; end_point="市ヶ谷"; via="有楽町線"; distance=0.9; time=1}; 
    {starting_point="市ヶ谷"; end_point="飯田橋"; via="有楽町線"; distance=1.1; time=2}; 
    {starting_point="飯田橋"; end_point="江戸川橋"; via="有楽町線"; distance=1.6; time=3}; 
    {starting_point="江戸川橋"; end_point="護国寺"; via="有楽町線"; distance=1.3; time=2}; 
    {starting_point="護国寺"; end_point="東池袋"; via="有楽町線"; distance=1.1; time=2}; 
    {starting_point="東池袋"; end_point="池袋"; via="有楽町線"; distance=2.0; time=2}; 
    {starting_point="池袋"; end_point="要町"; via="有楽町線"; distance=1.2; time=2}; 
    {starting_point="要町"; end_point="千川"; via="有楽町線"; distance=1.0; time=1}; 
    {starting_point="千川"; end_point="小竹向原"; via="有楽町線"; distance=1.0; time=2}; 
    {starting_point="小竹向原"; end_point="氷川台"; via="有楽町線"; distance=1.5; time=2}; 
    {starting_point="氷川台"; end_point="平和台"; via="有楽町線"; distance=1.4; time=2}; 
    {starting_point="平和台"; end_point="営団赤塚"; via="有楽町線"; distance=1.8; time=2}; 
    {starting_point="営団赤塚"; end_point="営団成増"; via="有楽町線"; distance=1.5; time=2}; 
    {starting_point="営団成増"; end_point="和光市"; via="有楽町線"; distance=2.1; time=3}; 
  ]
  ;;