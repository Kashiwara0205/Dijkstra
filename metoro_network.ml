open Station_variables

(* シグネチャ: station -> string *)
(* 処理概要: station型をstring型に変換 *)
let string_of_station recode =
  "---------------------------------"   ^ "\n" ^
  "駅名: " ^ recode.name                ^ "\n" ^
  "かな： " ^ recode.kana                ^ "\n" ^
  "ローマ字: " ^ recode.romaji           ^ "\n" ^
  "所属路線: " ^ recode.belonging_route ^ "\n" ^
  "---------------------------------"   ^ "\n"
  ;;
  
(* シグネチャ: station_interval -> string *)
(* 処理概要 : station_interval型をstring型に変換 *)
let string_of_station_interval recode =
  "---------------------------------" ^ "\n" ^
  "起点: " ^ recode.starting_point    ^ "\n" ^
  "終点： " ^ recode.end_point         ^ "\n" ^
  "経由: " ^ recode.via               ^ "\n" ^
  "距離: " ^ string_of_float(recode.distance)  ^ "\n" ^
  "時間: " ^ string_of_int(recode.time)        ^ "\n" ^
  "---------------------------------" ^ "\n"
  ;;

(* シグネチャ: string_list -> string *)
(* 処理概要 : string型のlistをlistから取り出して、string型にする *)
let rec string_of_string_list = function
  [] -> ""
  | e::l -> e ^ ", " ^ string_of_string_list l
  ;;

(* シグネチャ: dijkstra_station -> string *)
(* 処理概要:  dijkstra_station型をstring型に変換 *)
let string_of_dijkstra_station recode =
  "---------------------------------" ^ "\n" ^
  "駅名: " ^ recode.d_name            ^ "\n" ^
  "最短経路: " ^ string_of_float recode.d_shortest_distance  ^ "\n" ^
  "関わってる路線:" ^ string_of_string_list recode.d_name_list ^ "\n" ^
  "---------------------------------" ^ "\n"
  ;;

(* シグネチャ: string -> station list -> string *)
(* 処理概要 : station型のromajiを名前に変換する *)
let rec convert_romaji_to_name romaji recode_list =
  match recode_list with
    [] -> ""
    | e::l -> if e.romaji = romaji then e.name else convert_romaji_to_name romaji l
    ;;

(* シグネチャ: string -> string -> station_interval list -> float *)
(* 処理概要: 開始点と終点を受け取ると距離を返却 
             開始点と終点がつながってなかったらinfinityを返却*)
let rec get_distance starting_point end_point recode_list  =
  match recode_list with
    [] -> infinity
    | e::l -> if (e.starting_point = starting_point && e.end_point = end_point ||
                  e.starting_point = end_point && e.end_point = starting_point)
              then e.distance else get_distance starting_point end_point l
    ;;

(* シグネチャ: string -> string -> unit *)
(* 処理概要: 開始点と終点を受け取ると距離を表示する
             開始点と終点はローマ字で入ってくる      *)
let disp_distance r_starting_point r_end_point =
  let starting_point = convert_romaji_to_name r_starting_point all_station in
  let end_point = convert_romaji_to_name r_end_point all_station in
  let distance = get_distance starting_point end_point all_station_interval in
  match (starting_point, end_point) with
   | (s, e) when not(s = "" || e = "") -> print_endline(starting_point ^ "から" ^ end_point ^ "は、" ^ 
                                                        string_of_float distance ^ "kmです")
   | _ -> print_endline("存在しない駅が入力されています")
  ;;

(* シグネチャ: station list -> string -> dijkstra_station list  *)
(* 処理概要: ダイグストラアルゴリズムで使用する駅のリストを生成する  *)
let gen_dijkstra_station_list station_list name =
  List.map (fun station -> if station.name = name
                           then  { d_name = station.name; d_shortest_distance = 0.0; d_name_list = [station.name] } 
                           else  { d_name = station.name; d_shortest_distance = infinity; d_name_list = [] } 
  ) station_list
;;

(* シグネチャ： station list -> station list *)
(* 処理概要： 駅型を挿入ソートで並び替える 
             また、nameの重複を排除しなければならないのでnaneが等しい場合は、リストに加えない。
             漢字で比較すると五十音順にならないのでkanaでソートする*)
let rec station_insert_sort station_list =
  let rec station_insert record station_list =
    match station_list with
    [] -> record :: []
    | e::l ->
      if record.kana = e.kana then e :: l
      else if record.kana < e.kana then record :: e :: l
      else e :: station_insert record l
  in
  match station_list with
  [] -> []
  | e::l -> station_insert e(station_insert_sort l)
  ;;

(* シグネチャ: dijkstra_station -> dijkstra_station list -> station_interval list -> dijkstra_station list *)
(* 処理概要： 確定した駅: confirm_station
             未確定の駅リスト: unsettled_station_list
             を引数にとって未確定の駅リストを全て更新して返却する *)
let update_station_interval_list confirm_station unsettled_station_list station_interval_list  =
  let update confirm_station unsettled_station =
    match (confirm_station, unsettled_station) with
      ({d_name = c_name; d_shortest_distance = c_shortest_distance; d_name_list = c_name_list},
       {d_name = u_name; d_shortest_distance = u_shortest_distance; d_name_list = u_name_list})
      -> let distance = get_distance c_name u_name station_interval_list in
      if distance = infinity then unsettled_station
      else if (c_shortest_distance +. distance) < u_shortest_distance then { d_name = u_name; 
                                                                             d_shortest_distance = c_shortest_distance +. distance; 
                                                                             d_name_list = u_name :: c_name_list }
      else unsettled_station
  in
  let update = update confirm_station in
  List.map update unsettled_station_list
  ;;

(* メモ: fold_right  展開し終わった後の挙動(1回目):
                      第一引数に最後に展開されたfirstの要素が入る( 最初に関数が適用される )
                      第二引数に初期値が入る 
                    ２回目:
                      第一引数に展開されたfirstの要素が入る(最後から２番目に展開)
                      第二引数に先ほどの、第一引数の要素が入る ....*)
(* シグネチャ: dijkstra_station list -> dijkstra_station * dijkstra_station list *)
(* 処理概要： ダイグストラアルゴリズムで使っている駅のリスト: d_station_list 
             確定している最短経路の駅と、それ以外の駅の分離をタプルとして返却*)
let rec separation_shortest_distance_station d_station_list =
  List.fold_right(fun first (second, other_station) ->
    match (first, second) with
    ({d_name = fn;  d_shortest_distance = fs; d_name_list = fnl},
     {d_name = sn;  d_shortest_distance = ss; d_name_list = snl}) ->
     if sn = "" then (first, other_station)
     else if fs < ss then (first, second :: other_station)
     else (second, first :: other_station))
  d_station_list
  ({d_name = "" ; d_shortest_distance = infinity; d_name_list = [] }, [])

(* シグネチャ: dijkstra_station list -> station_interval list -> dijkstra_station list *)
(* 処理概要： ダイグストラアルゴリズムのmain
             unsettled_station_list: 未確定駅を表すdijkstra_station型のリスト
             station_interval_list: station_interval型のリスト *)
let rec dijkstra_main unsettled_station_list station_interval_list =
  match unsettled_station_list with
  [] -> []
  | first :: rest ->
    let (confirm_station, other_station) = separation_shortest_distance_station(first :: rest) in
    let station_list = update_station_interval_list confirm_station other_station station_interval_list in
    confirm_station :: dijkstra_main station_list station_interval_list
  ;;

(* 処理概要： ダイグストラアルゴリズムのmain
             starting_point: 開始駅の名前(ローマ字)
             end_point:      終了駅の名前(ローマ字) *)
let rec dijkstra starting_point end_point =
  let sorted_all_station = station_insert_sort all_station in
  let starting_point_name = convert_romaji_to_name starting_point sorted_all_station in
  let end_point_name = convert_romaji_to_name end_point sorted_all_station in
  let init_d_station_list = gen_dijkstra_station_list sorted_all_station starting_point_name in
  let result = dijkstra_main init_d_station_list all_station_interval in
  let rec match_d_name recode d_name =
    match recode with
    [] -> { d_name = ""; d_shortest_distance = infinity; d_name_list= [] }
    | first :: rest -> if first.d_name = d_name then first
                       else match_d_name rest d_name
  in
  let end_point_recode = match_d_name result end_point_name in
  print_endline("開始駅:" ^ starting_point_name ^ "\n" ^
                "終了駅:" ^ end_point_name ^ "\n" ^
                "最短距離:" ^ string_of_float end_point_recode.d_shortest_distance ^ "\n");  
;;