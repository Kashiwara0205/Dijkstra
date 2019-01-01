open Metoro_network;;
open Station_variables;;

(* ダイグストラアルゴリズムのテスト *)
(* テスト内容はサポートページを参考にした *)

(* テスト用: ダイグストラアルゴリズムの駅レコードのリスト *)
let dijkstra_station1 = { d_name = "池袋";   d_shortest_distance = infinity; d_name_list= [] };;
let dijkstra_station2 = { d_name = "新大塚"; d_shortest_distance = 1.2; d_name_list = ["新大塚"; "茗荷谷"] };;
let dijkstra_station3 = { d_name = "茗荷谷"; d_shortest_distance = 0.; d_name_list = ["茗荷谷"] };;
let dijkstra_station4 = { d_name = "後楽園"; d_shortest_distance = infinity; d_name_list = [] };;
let dijkstra_station_list = [dijkstra_station1; dijkstra_station2; dijkstra_station3; dijkstra_station4;];;

(* テスト用: 駅レコードのリスト *)
let test_all_station =
  [ 
    { name="代々木上原"; kana="よよぎうえはら";     romaji="yoyogiuehara";   belonging_route="千代田線" }; 
    { name="代々木公園"; kana="よよぎこうえん";     romaji="yoyogikouen";    belonging_route="千代田線" }; 
    { name="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; belonging_route="千代田線" }; 
    { name="表参道";     kana="おもてさんどう";    romaji="omotesandou";   belonging_route="千代田線" };
  ];;
  
(* テスト用: 駅間レコードのリスト *)
let distance1 = { starting_point="池袋";   end_point="新大塚"; via="丸ノ内線"; distance=1.8; time=3 };;
let distance2 = { starting_point="新大塚"; end_point="茗荷谷"; via="丸ノ内線"; distance=1.2; time=2 };; 
let distance3 = { starting_point="茗荷谷"; end_point="後楽園"; via="丸ノ内線"; distance=1.8; time=2 };;
let distance_list = [distance1; distance2; distance3;];;

let all_tree = gen_station_tree all_station_interval Empty;;

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

let result =
try convert_romaji_to_name "hoge" all_station with
  No_such_station station_name -> ""
;;
let test = result = "";;
print_endline("should get empty:" ^ string_of_bool test);;

print_endline("----------------------------------" ^ "\n");;

(* get_distanceメソッドのテスト *)
(* 期待値： 駅間の距離が適切に取得できること *)
print_endline("TEST: get_distance");;
print_endline("----------------------------------");;

let result = get_distance "日比谷" "二重橋前" all_tree;;
let test = result = 0.7;;
print_endline("should get distance:" ^ string_of_bool test);;

let result =
try get_distance "日比谷" "一重橋前" all_tree with
  Not_found -> infinity
;;
let test = result = infinity;;
print_endline("should get inifinity:" ^ string_of_bool test);;

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

(* sort_stationメソッドのテスト *)
(* 期待値： 五十音順に並び替えられるた駅型リストが返却されるか *)
print_endline("TEST: sort_station");;
print_endline("----------------------------------");;
let result = sort_station test_all_station;;
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
let result = update_station_interval_list dijkstra_station3 dijkstra_station_list all_tree;;
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
let result = separation_shortest_distance_station dijkstra_station1 [dijkstra_station2; dijkstra_station3; dijkstra_station4];;
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

(* insert_station_nodeのメソッドのテスト *)
(* 期待値: 駅の2分木が適切な形で挿入されること *)
let tree1 = insert_station_node distance1 Empty
let test1 = tree1 = 
  Node (Empty, 
        "新大塚", [("池袋", 1.8)],
         Node (Empty, 
               "池袋", [("新大塚", 1.8)], 
               Empty
         )
  );;

let tree2 = insert_station_node distance2 tree1 
let test2 = tree2 = 
  Node (Empty, 
        "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
        Node (Empty, 
              "池袋", [("新大塚", 1.8)], 
              Node (Empty, 
                    "茗荷谷", [("新大塚", 1.2)], 
                    Empty
              )
        )
  );;

let tree3 = insert_station_node distance3 tree2 
let test3 = tree3 = 
  Node (Node (Empty, 
              "後楽園", [("茗荷谷", 1.8)],
              Empty), 
	      "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
        Node (Empty, 
              "池袋", [("新大塚", 1.8)], 
	            Node (Empty, 
		                "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)], 
              Empty)
        )
  );;
  
print_endline("TEST: both_directions_insert_station_node");;
print_endline("----------------------------------");;
print_endline("should add correct node 1:" ^ string_of_bool test1);;
print_endline("should add correct node 2:" ^ string_of_bool test2);;
print_endline("should add correct node 3:" ^ string_of_bool test3);;
print_endline("----------------------------------"^ "\n");;

(* gen_station_treeのメソッドのテスト *)
(* 期待値: 駅の2分木が適切な形で返却されること *)

(* 作成した関数に使用したのは右畳み込みだったがサポートページで使っていた関数が
   左畳み込みを使っていたので、distanceリストの順番を逆にしてサポートページのテスト結果と
   合わせている *)  
let test = gen_station_tree [distance3; distance2; distance1] Empty = 
  Node (Node (Empty, 
              "後楽園", [("茗荷谷", 1.8)],
              Empty), 
	      "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
        Node (Empty, 
              "池袋", [("新大塚", 1.8)], 
	            Node (Empty, 
		                "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)], 
                    Empty
              )
        )
  );;
print_endline("TEST: gen_station_tree");;
print_endline("----------------------------------");;
print_endline("should get correct tree:" ^ string_of_bool test);;
print_endline("----------------------------------"^ "\n");;