open Station_variables
exception No_disp
exception No_such_station of string

(* シグネチャ: station -> string *)
(* 処理概要:  デバッグ用
              stationレコードをstring型に変換 *)
let string_of_station recode =
  "---------------------------------"   ^ "\n" ^
  "駅名: " ^ recode.name                ^ "\n" ^
  "かな： " ^ recode.kana                ^ "\n" ^
  "ローマ字: " ^ recode.romaji           ^ "\n" ^
  "所属路線: " ^ recode.belonging_route ^ "\n" ^
  "---------------------------------"   ^ "\n"
;;
  
(* シグネチャ: station_interval -> string *)
(* 処理概要 : デバッグ用
              station_intervalレコードをstring型に変換 *)
let string_of_station_interval recode =
  "---------------------------------" ^ "\n" ^
  "起点: " ^ recode.starting_point    ^ "\n" ^
  "終点： " ^ recode.end_point         ^ "\n" ^
  "経由: " ^ recode.via               ^ "\n" ^
  "距離: " ^ string_of_float(recode.distance)  ^ "\n" ^
  "時間: " ^ string_of_int(recode.time)        ^ "\n" ^
  "---------------------------------" ^ "\n"
;;

(* シグネチャ: dijkstra_station -> string *)
(* 処理概要:  デバッグ用
              dijkstra_stationレコードをstring型に変換 *)
let string_of_dijkstra_station recode =
  let rec string_of_string_list = function
    [] -> ""
    | e::l -> e ^ ", " ^ string_of_string_list l 
  in
  "---------------------------------" ^ "\n" ^
  "駅名: " ^ recode.d_name            ^ "\n" ^
  "最短経路: " ^ string_of_float recode.d_shortest_distance  ^ "\n" ^
  "関わってる路線:" ^ string_of_string_list recode.d_name_list ^ "\n" ^
  "---------------------------------" ^ "\n"
;;

(* シグネチャ: station_interval -> dijkstra_station_tree -> dijkstra_station_tree *)
(* 処理概要: 起点と終点、両方向を、それぞれノード化して木に挿入する。
             station_interval: 駅間レコード
             station_tree: 駅を二分木構造にしたもの *)
let insert_station_node station_interval station_tree =
  let rec insert s e d station_tree =
    match station_tree with
    Empty -> Node (Empty, s, [(e, d)], Empty)
    | Node(left, station_name, lst, right) ->
      if station_name = s then Node (left, station_name, (e, d) :: lst, right) 
      else if station_name < s
      then Node (left, station_name, lst, insert s e d right) 
      else Node (insert s e d left, station_name, lst, right) 
  in
  match station_interval with
  { starting_point = s; end_point = e; via = v; distance = d; time = t } ->
  let tmp = insert e s d station_tree in
  insert s e d tmp;;
;;

(* シグネチャ: station_interval list -> station_tree -> station_tree *)
(* 処理概要: 駅間レコードのリストを使い駅の二分木を作成する
             station_interval_list: 駅間レコードのリスト
             station_tree: 駅を二分木構造にしたもの*)
let gen_station_tree station_interval_list station_tree =
  List.fold_right(fun interval tree -> insert_station_node interval tree) station_interval_list station_tree
;;

(* シグネチャ: string -> station list -> string *)
(* 処理概要 : stationレコードのromajiを名前に変換する *)
let rec convert_romaji_to_name romaji recode_list =
  match recode_list with
    [] -> raise (No_such_station(romaji))
    | e::l -> if e.romaji = romaji then e.name else convert_romaji_to_name romaji l
;;

(* シグネチャ: string -> string -> dijkstra_station_tree -> float *)
(* 処理概要: 開始点と終点を受け取ると距離を返却 *)
let rec get_distance starting_point end_point station_tree =
  let get_distance_of_tuples station_name tuples =
    let result = List.filter(fun tuple -> station_name = fst(tuple)) tuples in
    match result with
    [] -> raise Not_found
    | first :: rest -> snd(first)
  in
  match station_tree with
  Empty -> raise Not_found
  | Node(left, station_name, tuples, right) ->
    if station_name = starting_point then get_distance_of_tuples end_point tuples
    else if station_name < starting_point then get_distance starting_point end_point right
    else get_distance starting_point end_point left
;;

(* シグネチャ: string -> string -> unit *)
(* 処理概要: 開始点と終点を受け取ると距離を表示する
             開始点と終点はローマ字で入ってくる      *)
let disp_distance r_starting_point r_end_point =
  let disp r_starting_point r_end_point =
    let starting_point = convert_romaji_to_name r_starting_point all_station in
    let end_point = convert_romaji_to_name r_end_point all_station in
    let all_tree = gen_station_tree all_station_interval Empty in
    match (starting_point, end_point) with
    | (s, e) when not(s = "" || e = "") -> 
        let distance = get_distance starting_point end_point all_tree in
        print_endline(starting_point ^ "から" ^ end_point ^ "は、" ^ string_of_float distance ^ "kmです")
    | _ -> raise No_disp
   in
  try disp r_starting_point r_end_point with
  No_such_station station_name -> print_endline (station_name ^ ": 駅は、存在しません")
  | Not_found -> print_endline "開始点と終点の駅が繋がっていません"
  | No_disp -> print_endline "表示するデータが存在しません"
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
(* 処理概要： 駅レコードを挿入ソートで並び替える 
             また、nameの重複を排除しなければならないのでnaneが等しい場合は、リストに加えない。
             漢字で比較すると五十音順にならないのでkanaでソートする*)
let rec sort_station station_list =
  let rec insert record station_list =
    match station_list with
    [] -> record :: []
    | e::l ->
      if record.kana = e.kana then e :: l
      else if record.kana < e.kana then record :: e :: l
      else e :: insert record l
  in
  match station_list with
  [] -> []
  | e::l -> insert e(sort_station l)
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
      -> 
      try 
        let distance = get_distance c_name u_name station_interval_list in
        if (c_shortest_distance +. distance) < u_shortest_distance then { d_name = u_name; 
                                                                          d_shortest_distance = c_shortest_distance +. distance; 
                                                                          d_name_list = u_name :: c_name_list }
        else unsettled_station
      with
      Not_found -> unsettled_station
  in
  List.map (update confirm_station) unsettled_station_list
;;

(* シグネチャ:  dijkstra_station -> dijkstra_station list -> dijkstra_station * dijkstra_station list *)
(* 処理概要： d_station_list: ダイグストラアルゴリズムで使っている駅のリスト
             確定している最短経路の駅と、それ以外の駅の分離をタプルとして返却*)
let rec separation_shortest_distance_station d_station d_station_list =
  match d_station_list with
  [] -> (d_station, [])
  | first :: rest ->
  let (second, other_station) = separation_shortest_distance_station first rest in
  match (d_station, second) with
    ({d_name = f_dn; d_shortest_distance = f_sd; d_name_list = f_dnl; },
     {d_name = s_dn; d_shortest_distance = s_sd; d_name_list = s_dnl; }) ->
     if f_sd < s_sd then (d_station, second :: other_station)
     else (second, d_station :: other_station)
;;
  
(* シグネチャ: dijkstra_station list -> station_interval list -> dijkstra_station list *)
(* 処理概要： ダイグストラアルゴリズムのmainループ
             unsettled_station_list: 未確定駅を表すdijkstra_stationレコードのリスト
             station_tree: 駅レコードの２分木 *)
let rec dijkstra_main unsettled_station_list station_tree =
  match unsettled_station_list with
  [] -> []
  | first :: rest ->
    let (confirm_station, other_station) = separation_shortest_distance_station first rest in
    let station_list = update_station_interval_list confirm_station other_station station_tree in
    confirm_station :: dijkstra_main station_list station_tree
;;

(* シグネチャ: string -> string -> unit *)
(* 処理概要： ダイグストラアルゴリズムを実行して、その結果を表示する
             starting_point: 開始駅の名前(ローマ字)
             end_point:      終了駅の名前(ローマ字) *)
let dijkstra starting_point end_point =
  let rec execute starting_point end_point =
    let sorted_all_station = sort_station all_station in
    let starting_point_name = convert_romaji_to_name starting_point sorted_all_station in
    let end_point_name = convert_romaji_to_name end_point sorted_all_station in
    let init_d_station_list = gen_dijkstra_station_list sorted_all_station starting_point_name in
    let all_tree = gen_station_tree all_station_interval Empty in
    let result = dijkstra_main init_d_station_list all_tree in
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
  in
  try execute starting_point end_point with
  No_such_station station_name -> print_endline (station_name ^ ": 駅は、存在しません")
;;