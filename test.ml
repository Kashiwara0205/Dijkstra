open Metoro_network;;
open Station_variables

(* テスト用: ダイグストラアルゴリズムの駅リスト *)
let dijkstra_station1 = { d_name = "池袋";   d_shortest_distance = infinity; d_name_list= [] };;
let dijkstra_station2 = { d_name = "新大塚"; d_shortest_distance = 1.2; d_name_list = ["新大塚"; "茗荷谷"] };;
let dijkstra_station3 = { d_name = "茗荷谷"; d_shortest_distance = 0.; d_name_list = ["茗荷谷"] };;
let dijkstra_station4 = { d_name = "後楽園"; d_shortest_distance = infinity; d_name_list = [] };;
let dijkstra_station_list = [dijkstra_station1; dijkstra_station2; dijkstra_station3; dijkstra_station4;];;

(* テスト用: 小さくした駅型のリスト *)
let test_all_station =
  [ 
    {name="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; belonging_route="千代田線"}; 
    {name="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; belonging_route="千代田線"}; 
    {name="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; belonging_route="千代田線"}; 
    {name="表参道"; kana="おもてさんどう"; romaji="omotesandou"; belonging_route="千代田線"}
  ];;
  
print_endline "[ TEST開始 ] \n" ;;

(* convert_romaji_to_nameメソッドのテスト *)
(* 期待値： ローマ字から名前に駅の名前が適切に変換されること *)
print_endline("TEST: convert_romaji_to_name");;
print_endline("----------------------------------");;
let result = convert_romaji_to_name "yoyogiuehara" all_station;;
let test = result = "代々木上原";;
print_endline("should get romaji(代々木上原):" ^ string_of_bool test);;
let result = convert_romaji_to_name "kokkaigijidomae" all_station;;
let test = result = "国会議事堂前";;
print_endline("should get romaji(国会議事堂前):" ^ string_of_bool test);;
let result = convert_romaji_to_name "hoge" all_station;;
let test = result = "";;
print_endline("should get empty:" ^ string_of_bool test);;
print_endline("----------------------------------" ^ "\n");;

(* get_distanceメソッドのテスト *)
(* 期待値： 駅間の距離が適切に取得できること *)
print_endline("TEST: get_distance");;
print_endline("----------------------------------");;
let result = get_distance "日比谷" "二重橋前" all_station_interval;;
let test = result = 0.7;;
print_endline("should get distance:" ^ string_of_bool test);;
let result = get_distance "日比谷" "一重橋前" all_station_interval;;
let test = result = infinity;;
print_endline("should get infinity:" ^ string_of_bool test);;
print_endline("----------------------------------" ^ "\n");;

(* disp_distanceメソッドのテスト *)
(* 期待値： 駅間の距離が適切に表示されること *)
print_endline("TEST: disp_distance");;
print_endline("----------------------------------");;
print_endline("test disp distance_text:");;
disp_distance "yoyogiuehara" "yoyogikouen";;
disp_distance "yoyogiuehara" "hogehoge";;
print_endline("----------------------------------" ^ "\n");;

(* gen_dijkstra_station_list メソッドのテスト *)
(* 期待値： リストを生成する際に返してくるリストの内容が適切かどうか *)
print_endline("TEST: gen_dijkstra_station_list");;
print_endline("----------------------------------");;
let result = gen_dijkstra_station_list test_all_station "代々木上原"
let test = result = [
  { d_name = "代々木上原"; d_shortest_distance = 0.0; d_name_list= ["代々木上原"] };
  { d_name = "代々木公園"; d_shortest_distance = infinity; d_name_list= [] };
  { d_name = "明治神宮前"; d_shortest_distance = infinity; d_name_list= [] };
  { d_name = "表参道";     d_shortest_distance = infinity; d_name_list= [] };
];;
print_endline("should get dijkstra_station_list:" ^ string_of_bool test);;
print_endline("----------------------------------" ^ "\n");;

(* station_insert_sortメソッドのテスト *)
(* 期待値： 五十音順に並び替えられるた駅型リストが返却されるか *)
print_endline("TEST: station_insert_sort");;
print_endline("----------------------------------");;
let result = station_insert_sort test_all_station;;
let test = result = [
  {name="表参道"; kana="おもてさんどう"; romaji="omotesandou"; belonging_route="千代田線"};
  {name="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; belonging_route="千代田線"}; 
  {name="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; belonging_route="千代田線"}; 
  {name="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; belonging_route="千代田線"}; 
];;
print_endline("should get sorted dijkstra_station_list :" ^ string_of_bool test);;
print_endline("----------------------------------" ^ "\n");;

(* update_station_interval_listメソッドのテスト *)
(* 期待値： ダイグストラアルゴリズムの駅リストが適切に更新されること *)
print_endline("TEST: update_station_interval_list");;
print_endline("----------------------------------");;
let result = update_station_interval_list dijkstra_station3 dijkstra_station_list all_station_interval;;
let test = result = [
  { d_name = "池袋";   d_shortest_distance = infinity; d_name_list= [] };
  { d_name = "新大塚"; d_shortest_distance = 1.2; d_name_list = ["新大塚"; "茗荷谷"] };
  { d_name = "茗荷谷"; d_shortest_distance = 0.; d_name_list = ["茗荷谷"] };
  { d_name = "後楽園"; d_shortest_distance = 1.8; d_name_list = ["後楽園"; "茗荷谷"] };
];;
print_endline("should update dijkstra_station_list :" ^ string_of_bool test);;
print_endline("----------------------------------" ^ "\n");;

(* separation_shortest_distance_stationメソッドのテスト *)
(* 期待値： 最短経路とその他が、ちゃんと分割されている事 *)
let result = separation_shortest_distance_station dijkstra_station_list;;
print_endline("TEST: separation_shortest_distance_station");;
print_endline("----------------------------------");;
let test = result = (dijkstra_station3, [dijkstra_station1; dijkstra_station2; dijkstra_station4]);;
print_endline("should get separation_distance_list:" ^ string_of_bool test);;
print_endline("----------------------------------" ^ "\n");;

(* ダイグストラアルゴリズムの結果表示に関するテスト *)
(* 期待値： 開始駅、終了駅、最短距離が適切に表示される事 *)
print_endline("TEST: dijstra");;
print_endline("----------------------------------"^ "\n");;
dijkstra "yoyogiuehara" "yoyogikouen";;
dijkstra "yoyogiuehara" "meijijinguumae";;
dijkstra "yoyogiuehara" "kasumigaseki";;
dijkstra "yoyogiuehara" "kokkaigijidomae";;
print_endline("----------------------------------" ^ "\n");;