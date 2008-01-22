(* dispimg - A very simple image viewer.
   
   Written by Shawn Wagner (shawnw@speakeasy.org) and released into the
   public domain.
   
   Compile with: 
   
   % make
   % make install
   
 *)

let opt_default opt def = match opt with Some v -> v | None -> def
let (|>) x f = f x

let is_directory fn = (Unix.lstat fn).Unix.st_kind = Unix.S_DIR
   
open GMain
   
type scaling = Fit | Fixed_AR of float

type options = { mutable wrap: bool; 
		 mutable fullscreen: bool;
		 mutable twopage: bool;
		 mutable manga: bool;
		 mutable remove_failed: bool; 
		 mutable scale: scaling;
		 mutable rar_exe: string;
	       }
(* TODO: save and load options *)
let opt = { wrap = false; fullscreen = false; 
	    twopage = false; manga = true;
	    remove_failed = true; scale = Fit;
	    rar_exe = "/home/thelema/bin/rar"
	  }

let preload_prio = 150 and show_prio = 115 and scale_prio = 200 and prescale_prio = 190


type book = { title: string; mutable files: string array }
type library = {prev : book Stack.t; next : book Stack.t}
let books = { prev=Stack.create ();
	      next=Stack.create () }

let numeric_compare s1 s2 = 
  let l1 = String.length s1 and l2 = String.length s2 in
  let rec pos_diff i = 
    if i = l1 then -2 else if i = l2 then -1
    else if s1.[i] = s2.[i] then pos_diff (i+1) else i
  and num_end i s =
    try if s.[i] >= '0' && s.[i] <= '9' then num_end (i+1) s else i with _ -> i-1
  in
  if l1 = l2 then String.compare s1 s2 
  else let d = pos_diff 0 in
  if d = -2 then -1 else if d = -1 then 1 else
    let e1 = num_end d s1 and e2 = num_end d s2 in
    if e1 = d || e2 = d then Pervasives.compare s1 s2
(*    else if e1 <> e2 then e1 - e2 else Pervasives.compare s1 s2 *)
    else begin
(*      Printf.eprintf "Compare: %s & %s @ d:%d e1:%d e2:%d->" s1 s2 d e1 e2;*)
      let n1 = Int64.of_string (String.sub s1 d (e1-d))
      and n2 = Int64.of_string (String.sub s2 d (e2-d)) in
(*      Printf.eprintf " %Ld & %Ld\n" n1 n2;*)
      Int64.compare n1 n2
    end

let push_books title files = 
  if List.length files > 0 then 
    let files = files |>  List.sort numeric_compare |> Array.of_list in
    Stack.push {title=title; files=files} books.next

let archive_suffixes = [("rar", `Rar); ("cbr", `Rar); ("zip", `Zip); ("cbz", `Zip); ("7z", `Sev_zip)]
let pic_suffixes = [("jpg", `Jpeg); ("jpeg", `Jpeg); ("gif", `Gif); ("png", `Png);(*any others?*) ]

let fold_sufchecks fn set_acc init suf_list = 
  let folder acc (s, tag) =
    if Filename.check_suffix fn s then set_acc tag else acc in 
  List.fold_left folder init suf_list

let suffix_type suf_list fn = fold_sufchecks fn (fun tag -> tag) `None suf_list
let any_suffix suf_list fn = fold_sufchecks fn (fun _ -> true) false suf_list

let archive_type fn = suffix_type archive_suffixes fn
let is_archive fn = any_suffix archive_suffixes fn
let is_picture fn = any_suffix pic_suffixes fn

let extract_archive file dir =
  match archive_type file with
    | `Rar -> Sys.command (Printf.sprintf "rar x \"%s\" \"%s\"/" file dir)
    | `Zip -> Sys.command (Printf.sprintf "unzip \"%s\" -d \"%s\"" file dir)
    | `Sev_zip -> Sys.command (Printf.sprintf "7za -o%s x \"%s\"" dir file)
    | `None -> assert false

let rec rec_del path = 
  let remove fn = (*Printf.eprintf "Removing: %s"*) Unix.unlink fn in
  Sys.readdir path
    |> Array.map (fun fn -> Filename.concat path fn)
    |> Array.iter (fun fn -> if is_directory fn
		   then rec_del fn
		   else remove fn);
    Unix.rmdir path

let build_books l = 
  let files_in path = assert (is_directory path); Sys.readdir path |> Array.to_list |> List.map (Filename.concat path) in
  let rec expand_list acc = function
      [] -> acc
    | h :: t when not (Sys.file_exists h) -> expand_list acc t
    | h :: t when is_directory h ->
	let title = Filename.basename h in
	let book = files_in h |> List.map (fun fn -> title,fn) in
	expand_list (List.rev_append book acc) t
    | h :: t when is_archive h ->
      let td = Filename.concat Filename.temp_dir_name (Printf.sprintf "coml-%d" (Random.bits ())) in
      (* extract archive to td *)
      let return_code = extract_archive h td in
      (* on quit, remove the extracted archive *)
      at_exit (fun () -> rec_del td);
      if return_code = 0 && Sys.file_exists td then begin
	let contents = files_in td in
	let dirs,files = List.partition is_directory contents
	and title = Filename.basename h in
(*Printf.eprintf "Title:%s\nFiles:\n" title;
List.iter (Printf.eprintf "%s\n") files;
Printf.eprintf "Dirs:\n";
List.iter (Printf.eprintf "%s\n") dirs;
Printf.eprintf "Contents:\n";
List.iter (Printf.eprintf "%s\n") contents;*)
	let acc' = files |> List.map (fun fn -> title,fn) |> List.append acc
	and t' = List.rev_append dirs t in
	  expand_list acc' t'
      end else 
	expand_list acc t
    | h :: t when is_picture h ->
	expand_list ((Filename.basename h,h)::acc) t
    | _ :: t -> (* garbage file *)
	expand_list acc t
  and group_books = function
      [] -> ()
    | (n1,p1)::t as l ->
	let b1,rest = List.partition (fun (n,_) -> n = n1) l in
	push_books n1 (b1|> List.rev_map snd);
	group_books rest
  in
  l |> expand_list [] |> group_books 

let current_book () = Stack.top books.next
let max_index () = Array.length (current_book()).files - 1

let within_book_range x = x >= 0 && x <= max_index ()

let book_count () = Stack.length books.next + Stack.length books.prev
and cur_book_number () = Stack.length books.prev + 1

let get_page idx = (current_book ()).files.(idx)

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:900 ~height:700 ()
let pane = GPack.paned `VERTICAL ~packing:window#add ()
let scroller = GBin.scrolled_window ~packing:pane#add1 ~width:9999 ~height:9999 ()
let _ = scroller#set_hpolicy `AUTOMATIC; scroller#set_vpolicy `AUTOMATIC
let image1 = GMisc.image ~packing:scroller#add_with_viewport ()
let footer = GPack.hbox ~packing:pane#pack2 ()
let file = GMisc.label ~packing:(footer#pack ~expand:true) ()
let _ = GMisc.separator `VERTICAL ~packing:footer#pack ()
let note = GMisc.label ~packing:(footer#pack ~expand:true) ()
let _ = GMisc.separator `VERTICAL ~packing:footer#pack ()
let bbox = GPack.button_box `HORIZONTAL ~packing:footer#pack ~layout:`END ()
(*let _ = let newsty = spread#misc#style#copy in newsty#set_bg [`NORMAL,`BLACK]; spread#misc#set_style newsty (* set the background black *)*)

let set_status str = Printf.eprintf "%.2f: " (Sys.time()); prerr_endline str; note#set_label str

let view_size () = let {Gtk.width=width; height=height} = scroller#misc#allocation in (width-2,height-2)

let pixbuf_size pix = GdkPixbuf.get_width pix, GdkPixbuf.get_height pix

type file = Failed | Not_loaded | Entry of GdkPixbuf.pixbuf
type cache = {mutable pos: int; mutable pics: file array}
let cache_past = ref 1 and cache_future = ref 1
let cache_size () = !cache_past + !cache_future + 1
let cache_null = Not_loaded
let failed_load = GdkPixbuf.create 1 1 ()
let image_cache = { pos=0; pics=Array.make (cache_size ()) cache_null; } 
let cache_last_idx () = min (image_cache.pos + (cache_size ()) - 1) (max_index ())

let within_cache_range idx =
  idx >= image_cache.pos && idx <= cache_last_idx ()

let get_cache' idx =
  try
    match image_cache.pics.(idx - image_cache.pos) with
	Entry cb -> cb
      | Not_loaded -> raise Not_found
      | Failed -> failed_load
  with Invalid_argument _ -> raise Not_found

let set_cache idx v =
  image_cache.pics.(idx - image_cache.pos) <- v

let print_cache () = 
  Printf.eprintf "Cache: p=%d; pics={" image_cache.pos;
  let print_pic = function 
      Not_loaded -> Printf.eprintf "None-" 
    | Failed -> Printf.eprintf "FAIL-" 
    | Entry p -> Printf.eprintf "L-"
  in
  Array.iter print_pic image_cache.pics;
  Printf.eprintf "}\n"

let recenter_cache ctr =
set_status (Printf.sprintf "Recentering cache to %d" ctr);
   let new_pos = max (ctr - !cache_past) 0 in
(*   Printf.eprintf "pos: %d new_pos: %d\n" image_cache.pos new_pos;*)
   print_cache ();
   let ic = image_cache.pics in
   let ic2 = Array.make (cache_size ()) cache_null in
   let new_size = Array.length ic2 and old_size = Array.length ic in
   let old_start, old_end = image_cache.pos, image_cache.pos + (old_size - 1)
   and new_start, new_end = new_pos, new_pos + new_size - 1 in
   let overlap_start = max old_start new_start
   and overlap_end = min old_end new_end in
   Printf.eprintf "blit ic %d ic2 %d %d\n" (overlap_start-old_start) (overlap_start-new_start) (overlap_end - overlap_start + 1);
   if overlap_start <= overlap_end then
     Array.blit ic (overlap_start-old_start) ic2 (overlap_start-new_start)
       (overlap_end - overlap_start + 1);

   image_cache.pics <- ic2;
   image_cache.pos <- new_pos;
   print_cache ()

let recenter_cache l = List.fold_left min max_int l |> recenter_cache

let reset_cache_all () = image_cache.pics <- Array.make (cache_size ()) cache_null

let next_book () = 
  let cur = Stack.pop books.next in
  Stack.push cur books.prev; 
  reset_cache_all ()

let prev_book () = 
  let cur = Stack.pop books.prev in
  Stack.push cur books.next; 
  reset_cache_all ()

let scale_wh (wi,hi) ar =
  let w1 = int_of_float (float_of_int wi *. ar) 
  and h1 = int_of_float (float_of_int hi *. ar) in
  (w1,h1)

let remove_file idx = 
  let cb = current_book () in
  let next i = if i < idx then cb.files.(i) else cb.files.(i+1) in
    cb.files <- Array.init (max_index ()) next

let rec load_cache idx = 
  let page_file = get_page idx in
  try 
    set_status (Printf.sprintf "Loading img %d from %s" idx page_file);
    let pic = GdkPixbuf.from_file page_file in
    set_cache idx (Entry pic)
  with GdkPixbuf.GdkPixbufError(_,msg) ->
    (*    let d = GWindow.message_dialog ~message:msg ~message_type:`ERROR
	  ~buttons:GWindow.Buttons.close ~show:true () in
	  ignore(d#run ()); *)
    set_status (Printf.sprintf "Failed to load %s" page_file);
    if opt.remove_failed then
      ( remove_file idx; load_cache idx )
    else
      set_cache idx Failed
	
let get_cache idx = 
Printf.eprintf "get_c %d\n" idx;
  if not (within_cache_range idx) then (* cache underflow *)
    recenter_cache [idx];
  try get_cache' idx 
  with Not_found -> load_cache idx; get_cache' idx

let preload_pic idx =
  let lc_task () = 
    (if within_cache_range idx then 
      match image_cache.pics.(idx - image_cache.pos) with
	  Entry _ | Failed -> ()
	| Not_loaded -> load_cache idx);
    false
  in
  ignore(Idle.add ~prio:preload_prio lc_task)

let preload_cache () = for i = image_cache.pos to cache_last_idx () do preload_pic i done


let size_diff (w1,h1) (w2,h2) = abs(w1-w2) > 2 || abs(h1-h2) > 2
let has_size size pb = not (size_diff (pixbuf_size pb) size)

let scale_factor (wt,ht) (wi, hi) =
  let wt = float_of_int wt and ht = float_of_int ht
  and wi = float_of_int wi and hi = float_of_int hi in
  let ar_t = wt /. ht and ar_i = wi /. hi in
  if ar_t > ar_i then ht /. hi else wt /. wi

let string_of_int_list prefix list = List.fold_left (fun acc i -> acc ^ "," ^ (string_of_int i)) prefix list
	    
module Spread = struct 
  type t = { idxes: int list;
	     bits : int; (* bits per pixel*)
	     alpha : bool; (* true when alpha channel needed *)
	     display : (GdkPixbuf.pixbuf -> unit);
	     mutable pixbuf: GdkPixbuf.pixbuf; 
	     mutable t_size: (int * int);
	     mutable scaler : Glib.Idle.id option;
	   }
  let equal a b = a.idxes = b.idxes
  let hash a = Hashtbl.hash a.idxes
    
  let null = { idxes = []; pixbuf = GdkPixbuf.create 1 1 (); t_size = (1,1); display = (fun p -> ()); bits=8; alpha=false ; scaler=None}
    
  let scale_idle spread () = 
    let width,height = spread.t_size in
    if not (has_size (width,height) spread.pixbuf) then begin
      set_status (Printf.sprintf "Resizing img (%s) to %dx%d" (string_of_int_list "" spread.idxes) width height);
      let scaled = GdkPixbuf.create ~width ~height ~bits:spread.bits ~has_alpha:spread.alpha () in
      GdkPixbuf.scale ~dest:scaled ~width ~height ~interp:`HYPER spread.pixbuf;
      spread.pixbuf <- scaled;
    end; 
    spread.display spread.pixbuf; 
    spread.scaler <- None; 
    false
      
  let scale ?size spread =
    (match size with None -> () | Some size -> spread.t_size <- size);
    if spread.scaler = None then 
      spread.scaler <- 
	Some (Idle.add ~prio:scale_prio (scale_idle spread))

  let quick_view s = 
    if size_diff s.t_size (pixbuf_size s.pixbuf) then
      let (width, height) = s.t_size in
      let out_b = GdkPixbuf.create ~width ~height ~bits:s.bits ~has_alpha:s.alpha () in
      GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`NEAREST s.pixbuf;
      out_b
    else 
      s.pixbuf

  let show s = 
    s.display (quick_view s)

  let make ~target ~display idxes = 
    assert (idxes <> []);
    set_status (string_of_int_list "Making spread for pages" idxes);
    let pics = idxes |> List.map get_cache in
    
    (* generate the pixbif *)
    let out_w, out_h  = pics |> List.map pixbuf_size |> List.fold_left (fun (wa,ha) (wp,hp) -> (wa+wp,max ha hp)) (0,0) in
    let out_bits = pics |> List.map GdkPixbuf.get_bits_per_sample |> 
	List.fold_left max 0 
    and out_alpha = pics |> List.map GdkPixbuf.get_has_alpha |> 
	List.fold_left (||) false in
    let out_b = GdkPixbuf.create ~width:out_w ~height:out_h 
      ~bits:out_bits ~has_alpha:out_alpha () in
    let rec copier dest_x = function [] -> () | pb::rest ->
      let (w, h) = pixbuf_size pb in
      let dest_y = (out_h - h) / 2 in
      GdkPixbuf.copy_area ~dest:out_b ~dest_x ~dest_y ~width:w ~height:h pb;
      copier (dest_x + w) rest
    in
    copier 0 pics;
    
    set_status (Printf.sprintf "Made spread (%d,%d), %d bits, alpha %b" out_w out_h out_bits out_alpha);
    (* make the t_size *)
    let ar = match opt.scale with
      | Fit -> min 1.0 (scale_factor target (out_w, out_h))
      | Fixed_AR ar -> ar
    in
    let t_size = scale_wh (out_w, out_h) ar in
    
    let s = { idxes = idxes; pixbuf = out_b; 
	      t_size = t_size; display = display idxes; scaler = None;
	      bits = out_bits; alpha = out_alpha } in
    s.scaler <- Some (Idle.add ~prio:scale_prio (scale_idle s));
    show s;
    s

end

module SpreadCache = Weak.Make(Spread)

let sc = SpreadCache.create 3 

let image_idxes = ref [0]

let get_scache idxes = SpreadCache.find sc {Spread.null with Spread.idxes=idxes}

let get_scache_current () = get_scache !image_idxes

let set_t_size size = 
  Spread.scale ~size (get_scache_current ());
  SpreadCache.iter (Spread.scale ~size) sc
  
(* generate simple preview *)
let display idxes = 

  try 
    Spread.show (get_scache idxes);
    ignore (Glib.Main.iteration true)
  with
      Not_found -> 
	let spread = Spread.make ~target:(view_size ()) 
	  ~display:(fun i p -> if i = !image_idxes then image1#set_pixbuf p) 
	  idxes in
	SpreadCache.add sc spread;
	ignore (Glib.Main.iteration true)

  (* draw the screen *)


let show_task = ref None

let show_spread' () =
  file#set_text (try Glib.Convert.filename_to_utf8 (Filename.basename (get_page (List.hd !image_idxes))) with Glib.Convert.Error (_,s) -> s);
  !image_idxes |>
      List.filter (fun i -> i < max_index()) |>
	  display;
  show_task := None;
  false
    
let show_spread () = 
  window#set_title (Printf.sprintf "Image_idx %s of %d, Book %d of %d : %s" (string_of_int_list "" !image_idxes) (max_index()) (cur_book_number ()) (book_count()) (current_book()).title);
  match !show_task with
      None -> 
	show_task := Some (Idle.add ~prio:show_prio show_spread')
    | Some _ -> ()

let new_pos idxes = 
  (* idxes already in reverse order *)
  image_idxes := idxes |> (if opt.manga then (fun x -> x) else List.rev) |> List.filter within_book_range;
  recenter_cache !image_idxes;
  preload_cache ();
  show_spread ()

let is_vert idx = 
  let (w0,h0) = pixbuf_size (get_cache' idx) in
  w0 < h0

let group_pages ~seed:idx ~forward =
  if not opt.twopage then [idx]
  else 
    let p1,p2 = if forward then idx, idx+1 else idx-1,idx in
    try 
      if is_vert p1 && is_vert p2 then [p1;p2]
      else [p1]
    with Not_found -> [p1;p2]

let first_image () = set_status "At beginning of book"; new_pos (group_pages ~seed:0 ~forward:true)

let last_image () = set_status "At end of book"; new_pos (group_pages ~seed:(max_index ()) ~forward:false)

let past_end () = 
  if opt.wrap then first_image () else
    if Stack.length books.next <= 1 
    then (last_image (); set_status "At end of Library")
    else (next_book (); first_image () )

let past_start () = 
  if opt.wrap then last_image () else
    if Stack.length books.prev < 1 
    then ( first_image (); set_status "At start of Library" )
    else ( prev_book (); last_image ())

let prev_image () =
  let cur_min = List.fold_left min max_int !image_idxes in
  let seed_page = cur_min - 1 in
  if seed_page < 0 then past_start ()
  else new_pos (group_pages ~seed:seed_page ~forward:false)
    
let next_image () =
  let cur_max = List.fold_left max min_int !image_idxes in
  let seed_page = cur_max + 1 in
  if seed_page > max_index () then past_end ()
  else new_pos (group_pages ~seed:seed_page ~forward:true)
    
let toggle_twopage () =
  opt.twopage <- not opt.twopage;
  (* increase cache size if twopage *)
  cache_past := if opt.twopage then 2 else 1;
  cache_future := if opt.twopage then 3 else 1;  (* include second page of currint in future *)
  let cur_min = List.fold_left min max_int !image_idxes in
  new_pos (group_pages ~seed:cur_min ~forward:true)

let toggle_fullscreen () =
  if opt.fullscreen then (
    opt.fullscreen <- false;
    footer#misc#show ();
    window#unfullscreen ();
   ) else (
    opt.fullscreen <- true;
    footer#misc#hide ();
    window#fullscreen ();
   );
  show_spread ()

let toggle_manga () =
  opt.manga <- not opt.manga;
  image_idxes := List.rev !image_idxes;
  show_spread()

let go_to_page_dialog () =
  let _ = GWindow.dialog ~parent:window ~title:"Go to page"  () in
  () (* FIXME COMPLETE THIS *)

let zoom ar_val ar_func = 
  opt.scale <-( match opt.scale with
		    Fit -> Fixed_AR ar_val 
		  | Fixed_AR ar -> ar_func ar)
    
(*;
  if can_twopage !image_idx then 
    scale2_for_view (view_size()) (get_cache !image_idx) (get_cache (!image_idx+1))
  else 
    scale_for_view (view_size()) (get_cache !image_idx)*)

let zoom_out () = zoom 0.95 (fun ar -> Fixed_AR (ar *. 0.95))
and zoom_in () = zoom (1.0 /. 0.95) (fun ar -> Fixed_AR (ar /. 0.95))
and toggle_zoom () = zoom 1.0 (fun _ -> Fit)
  
open GdkKeysyms

let actions = [(_q, Main.quit);
	    (_Left, prev_image); (_Up, prev_image); (_BackSpace, prev_image);
	    (_Right, next_image); (_Down, next_image); (_space, next_image);
	    (_f, toggle_fullscreen); (_t, toggle_twopage);
	    (_m, toggle_manga); (_z, toggle_zoom);
	    (_minus, zoom_out); (_plus, zoom_in);
	    (_l, (fun () -> load_cache (List.fold_left min 0 !image_idxes)));
	    (_w, (fun () -> opt.wrap <- not opt.wrap));
	    (_Page_Up, past_start); (_Page_Down, past_end);
	   ]

let handle_key event =
  try 
    let kv = GdkEvent.Key.keyval event in
    (* look up the key value in the actions list *)
    let action = List.assoc kv actions in
      action ();
      true
  with Not_found -> false

let resized event =
  let image_width = GdkEvent.Configure.width event
  and image_height = GdkEvent.Configure.height event in
  Printf.eprintf "RESIZE: %dx%d\n" image_width image_height;
  show_spread ();
  false
  
let main () =
  Random.self_init ();
  let arg_list = Sys.argv |> Array.to_list |> List.tl in
  let arg_list = if List.length arg_list = 0 then ["."] else arg_list in
  build_books arg_list;
  if Stack.is_empty books.next || max_index () = 0 then begin
    Printf.printf "Usage: %s [IMAGEFILE|IMAGEDIR|IMAGEARCHIVE] ...\n" Sys.argv.(0);
    exit 1
  end;


  let prev = GButton.button ~stock:`GO_BACK ~packing:bbox#pack ()
  and next = GButton.button ~stock:`GO_FORWARD ~packing:bbox#pack () in
  ignore (prev#connect#clicked ~callback:prev_image);
  ignore (next#connect#clicked ~callback:next_image);

  let close = GButton.button ~stock:`CLOSE ~packing:bbox#pack () in
  ignore (close#connect#clicked ~callback:Main.quit);

  ignore (window#connect#destroy ~callback:Main.quit);
  ignore (window#event#connect#key_press ~callback:handle_key);
  ignore (window#event#connect#configure ~callback:resized);
(*  ignore (pane#event#connect#configure ~callback:resized);*)
  show_spread ();
  window#show ();
  preload_cache ();
(*  prescale_cache ();*)
  Main.main () (* calls the GTK main event loop *)
    
let _ = main ()
