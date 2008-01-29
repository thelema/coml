(* dispimg - A very simple image viewer.
   
   Written by Shawn Wagner (shawnw@speakeasy.org) and released into the
   public domain.
   
   Compile with: 
   
   % make
   % make install
   
 *)

let opt_default opt def = match opt with Some v -> v | None -> def
let (|>) x f = f x
let string_of_int_list prefix list = prefix ^ (list |> List.map string_of_int |> String.concat ",")

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
	    twopage = true; manga = true;
	    remove_failed = true; scale = Fit;
	    rar_exe = "/home/thelema/bin/rar"
	  }

let preload_prio = 200 and show_prio = 115 and scale_prio = 150

let failed_load = GdkPixbuf.create 1 1 ()

module Spread = struct 
  type t = { pos: int;
	     bits : int; (* bits per pixel *)
	     mutable o_width : int; mutable o_height: int;
	     mutable idxes: int list;
	     mutable pixbuf: GdkPixbuf.pixbuf; 
	     mutable t_size: int * int;
	     mutable scaler : Glib.Idle.id option;
	     get_pic : int -> GdkPixbuf.pixbuf;
	     dir : [`FORWARD | `BACKWARD];
	   }
    
  let null = { pos = -1; pixbuf = failed_load; idxes = []; t_size = (1,1); bits=8; scaler=None; o_width = 1; o_height = 1; get_pic = (fun _ -> failed_load); dir=`FORWARD}

  let get_idxes s = s.idxes
  let get_pos s = s.pos
    
  let pixbuf_size pix = GdkPixbuf.get_width pix, GdkPixbuf.get_height pix
  let size_diff (w1,h1) (w2,h2) = abs(w1-w2) > 2 || abs(h1-h2) > 2
  let has_size size pb = not (size_diff (pixbuf_size pb) size)
    
  let scale_factor (wt,ht) (wi, hi) =
    let wt = float wt and ht = float ht
    and wi = float wi and hi = float hi in
    let ar_t = wt /. ht and ar_i = wi /. hi in
    if ar_t > ar_i then ht /. hi else wt /. wi

  let scale_wh (wi,hi) ar =
    let w1 = int_of_float (float_of_int wi *. ar) 
    and h1 = int_of_float (float_of_int hi *. ar) in
    (w1,h1)

  let t_sizer v_size (s_w,s_h) = 
    let ar = match opt.scale with
      | Fit -> min 1.0 (scale_factor v_size (s_w, s_h))
      | Fixed_AR ar -> ar
    in
    scale_wh (s_w, s_h) ar

  let quick_view s ?(size=s.t_size) () = 
    let width,height = t_sizer s.t_size (s.o_width,s.o_height) in
    if size_diff (width,height) (pixbuf_size s.pixbuf) then
      let out_b = GdkPixbuf.create ~width ~height ~bits:s.bits ~has_alpha:true () in
      GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`NEAREST s.pixbuf;
      out_b
    else 
      s.pixbuf

  let pics_of_idxes get_cache idxes = idxes
	   |> List.map get_cache 
	   |> List.filter (fun p -> p != failed_load) 

  let is_vert pic = let (w0,h0) = pixbuf_size pic in w0 < h0


  (* add a picture to the spread -- doesn't update the pixbuf*)
  let add_pic s pic idx dir = 
    let w1,h1 = pixbuf_size pic in
    s.o_width <- s.o_width + w1; s.o_height <- max s.o_height h1;
    s.idxes <- idx :: s.idxes; 
    if opt.manga then s.idxes <- List.rev s.idxes;
    if dir = `FORWARD then s.idxes <- List.rev s.idxes
      
  let freshen_pixbuf s =
    if List.length s.idxes > 1 then 
      let out_buf = GdkPixbuf.create ~width:s.o_width ~height:s.o_height 
	~bits:s.bits ~has_alpha:true () in
      GdkPixbuf.fill out_buf (0x00000000l);
      
      let rec copier dest_x = function [] -> () | pb::rest ->
	let (w, h) = pixbuf_size pb in
	let dest_y = (s.o_height - h) / 2 in
	GdkPixbuf.copy_area ~dest:out_buf ~dest_x ~dest_y ~width:w ~height:h pb;
	copier (dest_x + w) rest
      in
      copier 0 (s.idxes |> List.map s.get_pic);
      s.pixbuf <- out_buf
    else 
      s.pixbuf <- s.get_pic s.pos

  let scale_idle ~post_scale spread () = 
    if is_vert spread.pixbuf && opt.twopage then begin
      let next_idx = (match spread.dir with `FORWARD -> (+) | `BACKWARD -> (-)) 1 spread.pos in
      let next_pic = spread.get_pic next_idx in
      if is_vert next_pic then begin
	Printf.eprintf "Adding %d to spread %d\n" next_idx spread.pos;
	add_pic spread next_pic next_idx spread.dir
      end;
    end;
    
    let width,height = t_sizer spread.t_size (spread.o_width,spread.o_height) in
    if not (has_size (width,height) spread.pixbuf) then begin
Printf.eprintf "Resizing img (%s) to %dx%d\n" (string_of_int_list "" spread.idxes) width height;
      freshen_pixbuf spread;
      let scaled = GdkPixbuf.create ~width ~height ~bits:spread.bits ~has_alpha:true () in
      GdkPixbuf.scale ~dest:scaled ~width ~height ~interp:`HYPER spread.pixbuf;
      spread.pixbuf <- scaled;
    end;
    (match post_scale with None -> () | Some f -> f spread.pos spread.pixbuf);
    spread.scaler <- None; 
    false
      
  let make pos get_cache dir = 
    let pic = get_cache pos in
    let out_w, out_h  = pixbuf_size pic 
    and out_bits = GdkPixbuf.get_bits_per_sample pic in
    
    { pos = pos; idxes = [pos];
      o_width = out_w; o_height = out_h;
      bits = out_bits; pixbuf = pic;
      t_size = (out_w,out_h); scaler = None; 
      get_pic = get_cache; dir = dir }

  let scale ?post_scale size spread =
    spread.t_size <- size;
    if spread.scaler = None then
      spread.scaler <- 
	Some (Idle.add ~prio:scale_prio (scale_idle ~post_scale spread));
	
end


type book = { title: string; 
	      mutable files: string array; 
	      cache: GdkPixbuf.pixbuf Weak.t;
	      spreads: Spread.t Weak.t}
type library = {prev : book Stack.t; next : book Stack.t}
let books = ref [| |]

let numeric_compare s1 s2 = 
  let l1 = String.length s1 and l2 = String.length s2 in
  let s1 = String.lowercase s1 and s2 = String.lowercase s2 in
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

let make_book title files = 
  let files = files |>  List.sort numeric_compare |> Array.of_list in
  let cache = Weak.create (Array.length files) in
  let scache = Weak.create (Array.length files) in
  {title=title; files=files; cache=cache; spreads = scache}

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
  and group_books acc = function
      [] -> acc
    | (n1,p1)::t as l ->
	let b1,rest = List.partition (fun (n,_) -> n = n1) l in
	let files = b1|> List.rev_map snd|> List.filter is_picture in
	let acc' = (make_book n1 files)::acc in
	group_books acc' rest
  in
  books := l |> expand_list [] |> group_books [] |> Array.of_list

let book_idx = ref 0

let current_book () = (!books).(!book_idx)

let max_index ?(book = current_book()) () = 
  Array.length book.files - 1

let within_book_range ?(book=current_book()) x = 
  x >= 0 && x <= max_index ()

let book_count () = Array.length !books

let get_page ?(book=current_book()) idx = 
  try book.files.(idx) with Invalid_argument _ -> "OOB"


(* GTK WIDGETS *)

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:900 ~height:700 ()
let pane = GPack.paned `VERTICAL ~packing:window#add ()
let contents = GPack.hbox ~packing:pane#add1 ~height:660 ()
let sidebar = GBin.scrolled_window ~packing:contents#pack ~width:100 ()
let _ = sidebar#set_hpolicy `NEVER ; sidebar#set_vpolicy `AUTOMATIC
let scroller = GBin.scrolled_window ~packing:contents#add ~width:800 ()
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

let get_cache' ?(book=current_book()) idx =
  try 
    match Weak.get book.cache idx with
	Some cb -> cb
      | None -> raise Not_found
  with Invalid_argument _ -> failed_load

let set_cache ?(book=current_book()) idx v = 
  Weak.set book.cache idx (Some v)

let print_cache ?(book=current_book()) () = 
  Printf.eprintf "Cache: {";
  let print_pic = function 
      None -> Printf.eprintf "None-" 
    | Some _ -> Printf.eprintf "L-"
  in
  for i = 0 to max_index ~book () do
    print_pic (Weak.get_copy book.cache i);
  done;
  Printf.eprintf "}\n"

let array_without arr idx = 
  let l = Array.length arr in
  let next i = if i < idx then arr.(i) else arr.(i+1) in
  Array.init (l-1) next

let drop_book () =
  books := array_without !books !book_idx;
  if Array.length !books = 0 then begin
    Printf.printf "All Files Invalid\n";
    Printf.printf "Usage: %s [IMAGEFILE|IMAGEDIR|IMAGEARCHIVE] ...\n" Sys.argv.(0);
    Main.quit ()
  end;
  book_idx := min (Array.length !books - 1) !book_idx
	
let remove_file ~book idx = 
set_status (Printf.sprintf "Removing page %d of %d" idx (max_index ~book ()));
  assert (within_book_range ~book idx);
  let next i = if i < idx then book.files.(i) else book.files.(i+1) in
  if max_index ~book () = 0 then 
    drop_book ()
  else 
    book.files <- Array.init (max_index ()) next
      
let rec load_cache ~book idx =
(*set_status (Printf.sprintf "Loading page %d of %d: %b" idx (max_index ~book ()) (within_book_range ~book idx));*)
  if within_book_range ~book idx then 
    let page_file = get_page idx in
    try 
(*      set_status (Printf.sprintf "Loading img %d from %s" idx page_file);*)
      let pic = GdkPixbuf.from_file page_file in
      set_cache idx pic
    with GdkPixbuf.GdkPixbufError(_,msg) | Glib.GError msg ->
      (*    let d = GWindow.message_dialog ~message:msg ~message_type:`ERROR
	    ~buttons:GWindow.Buttons.close ~show:true () in
	    ignore(d#run ()); *)
      set_status (Printf.sprintf "Failed to load %s: %s" page_file msg);
      if opt.remove_failed then
	( remove_file ~book idx; load_cache ~book idx )
      else
	set_cache idx failed_load

let get_cache ?(book=current_book()) idx = 
(*Printf.eprintf "get_c %d\n" idx;*)
  try get_cache' ~book idx 
  with Not_found -> load_cache ~book idx; get_cache' ~book idx

let get_scache ?(book=current_book()) pos = try Weak.get book.spreads pos with Invalid_argument _ -> None

let put_scache ?(book=current_book()) spread = Weak.set book.spreads (Spread.get_pos spread) (Some spread)

let disp_pos = ref 0
let move_dir = ref `FORWARD

let view_size () = let {Gtk.width=width; height=height} = scroller#misc#allocation in (width-2,height-2)

let get_spread ?(book=current_book()) pos = 
  match get_scache ~book pos with
      None -> 
	let s = Spread.make pos get_cache !move_dir in
	put_scache ~book s;
	Printf.eprintf "Cache_miss %d  " pos;
	s
    | Some s -> Printf.eprintf "Cache_hit %d  " pos; s

let idxes_on_disp () = 
  match get_scache !disp_pos with 
      None -> Printf.eprintf "IOD: n=%d  " !disp_pos; [!disp_pos] 
    | Some s -> Printf.eprintf "IOD: S[%s]  " (Spread.get_idxes s |> string_of_int_list ""); Spread.get_idxes s

let display pos pic = 
  if pos = !disp_pos then (
    image1#set_pixbuf pic;
    window#set_title (Printf.sprintf "Image_idx %s of %d, Book %d of %d : %s" (idxes_on_disp () |> string_of_int_list "") (max_index ()) (!book_idx+1) (book_count()) !books.(!book_idx).title);
  )

let prev_page idxes = 
  let i = idxes |> List.fold_left min max_int in
  if opt.twopage then (* try to move back two pages *)
    match get_scache (i-2) with
	None -> i-1
      | Some s when List.mem (i-1) s.Spread.idxes -> i-2
      | _ -> i-1
  else i-1

let next_page idxes = idxes |> List.fold_left max min_int |> (+)1

let preload_spread () = 
  let preload_task () = 
    let idxes = (Spread.get_idxes (get_spread !disp_pos)) in
    let next_pos = 
      (match !move_dir with `FORWARD -> next_page | `BACKWARD -> prev_page) idxes in
    set_status (Printf.sprintf "Preloading: %d (iod: %s)" next_pos (idxes |> string_of_int_list ""));
    let next_spread = get_spread next_pos in
    Spread.scale (view_size ()) next_spread;
    false
  in
  ignore(Idle.add ~prio:preload_prio preload_task)

let show_spread () = 
  let show_taskid = ref None in
  let cur_spread_task () =
    let pos = !disp_pos in
    let spread = get_spread pos in
    Spread.scale ~post_scale:display (view_size ()) spread;
    display pos (Spread.quick_view spread ());
    preload_spread ();

    (* draw the screen *)
    ignore (Glib.Main.iteration true);
    show_taskid := None;
    false
  in
  file#set_text (try Glib.Convert.filename_to_utf8 (Filename.basename (get_page !disp_pos)) with Glib.Convert.Error (_,s) -> s);
  if !show_taskid = None then 
    show_taskid := Some (Idle.add ~prio:show_prio cur_spread_task)
    
let first_page _ = 0
let last_page _ = max_index ()

let rec new_pos (pos,book_i) =
  let b0 = 0 and bmax = Array.length !books - 1 in
  let book_i = min bmax (max b0 book_i) in
  let p0 = 0 and pmax = max_index ~book:!books.(book_i) () in
  if pos < p0 then new_pos 
    (if book_i > b0 then max_int,book_i -1
     else if opt.wrap then max_int,bmax else 0,book_i)
  else if pos > pmax then new_pos 
    ( if pos = max_int then pmax,book_i 
      else if book_i < bmax then 0,book_i + 1
      else if opt.wrap then 0,0 else pmax,book_i)
  else begin
    if pos = 0 then set_status 
      (if book_i = 0 then "At beginning of Library" else "At beginning of book")
    else 
      if pos = max_index () then set_status 
	(if book_i = bmax then "At end of Library" else "At end of book");
    move_dir := if pos=0 then `FORWARD else if pos = pmax then `BACKWARD else if pos > !disp_pos then `FORWARD else `BACKWARD;
    disp_pos := pos;
    book_idx := book_i;
    show_spread ()
  end

let new_page gen_page () = new_pos (gen_page (idxes_on_disp ()),!book_idx)
    
let clear_spread_cache ?(book=current_book())() =
  Weak.fill book.spreads 0 (max_index ~book () + 1) None

let toggle_twopage () =
  opt.twopage <- not opt.twopage;
  clear_spread_cache ();
  show_spread ()

let enter_fullscreen () = 
  opt.fullscreen <- true;
  footer#misc#hide (); sidebar#misc#hide ();
  window#fullscreen ();
  show_spread ()

let exit_fullscreen () =
  opt.fullscreen <- false;
  footer#misc#show (); sidebar#misc#show ();
  window#unfullscreen ();
  show_spread ()

let toggle_fullscreen () =
  if opt.fullscreen then exit_fullscreen () else enter_fullscreen ()

let toggle_manga () =
  opt.manga <- not opt.manga;
  clear_spread_cache ();
  show_spread()

let go_to_page_dialog () =
  let w = GWindow.dialog ~parent:window ~title:"Go to page" ~modal:true ~position:`CENTER () in
  ignore (GMisc.label ~text:"Page: " ~packing:w#vbox#add ());
  let sb = GEdit.spin_button ~packing:w#vbox#add ~digits:0 ~numeric:true () in
  sb#adjustment#set_bounds ~lower:0. ~upper:(float (max_index ())) ~step_incr:1. (); sb#set_value (float !disp_pos); (*sb#set_activates_default true;*)

  let entry = new GEdit.entry (GtkEdit.Entry.cast sb#as_widget) in
  entry#set_activates_default true;

  w#add_button_stock `OK `OK;
  w#add_button_stock `CANCEL `CANCEL;
  w#set_default_response `OK;
  let on_ok () = new_pos (sb#value_as_int,!book_idx); w#destroy () in
  match w#run () with
    | `DELETE_EVENT | `CANCEL -> w#destroy ()
    | `OK -> on_ok ()

let zoom ar_val ar_func = 
  opt.scale <-( match opt.scale with
		    Fit -> Fixed_AR ar_val
		  | Fixed_AR ar -> ar_func ar);
  clear_spread_cache ();
  show_spread()

let zoom_out () = zoom 0.95 (fun ar -> Fixed_AR (ar *. 0.95))
and zoom_in () = zoom (1.0 /. 0.95) (fun ar -> Fixed_AR (ar /. 0.95))
and toggle_zoom () = zoom 1.0 (fun _ -> Fit)


(* key bindings as a list of pairs  (key, function) *)  
open GdkKeysyms

let actions = [(_q, Main.quit);
	    (_Left, new_page prev_page); 
	    (_Up, new_page prev_page); (_BackSpace, new_page prev_page);
	    (_Right, new_page next_page); (_Down, new_page next_page); 
	    (_space, new_page next_page);
	    (_f, toggle_fullscreen); (_F11, toggle_fullscreen);
	    (_Escape, exit_fullscreen); (_t, toggle_twopage);
	    (_m, toggle_manga); (_z, toggle_zoom);
	    (_minus, zoom_out); (_plus, zoom_in);
(*	    (_l, (fun () -> load_cache (List.fold_left min 0 !image_idxes)));*)
	    (_w, (fun () -> opt.wrap <- not opt.wrap));
	    (_Page_Up, new_page first_page); (_Page_Down, new_page last_page);
	    (_g, go_to_page_dialog);
	    (_o, (fun () -> ignore (idxes_on_disp ())));
	   ]

let handle_key event =
  try 
    let kv = GdkEvent.Key.keyval event in
    (* look up the key value in the actions list *)
    let action = List.assoc kv actions in
      action ();
      true
  with Not_found -> false
    
(* Callback when main window gets resized *)
let resized event =
(*  let image_width = GdkEvent.Configure.width event
  and image_height = GdkEvent.Configure.height event in
  Printf.eprintf "RESIZE: %dx%d\n" image_width image_height; *)
  show_spread ();
  false

let main () =
  Random.self_init ();

  (* BUILD BOOKS BASED ON ARGUMENT LIST *)
  let arg_list = Sys.argv |> Array.to_list |> List.tl in
  (* If no args, assume "." as argument *)
  let arg_list = if List.length arg_list = 0 then ["."] else arg_list in
  build_books arg_list;
  if Array.length !books = 0 || max_index () = 0 then begin
    Printf.printf "Usage: %s [IMAGEFILE|IMAGEDIR|IMAGEARCHIVE] ...\n" Sys.argv.(0);
    exit 1
  end;


  let prev = GButton.button ~stock:`GO_BACK ~packing:bbox#pack ()
  and next = GButton.button ~stock:`GO_FORWARD ~packing:bbox#pack () in
  ignore (prev#connect#clicked ~callback:(new_page prev_page));
  ignore (next#connect#clicked ~callback:(new_page next_page));

  let close = GButton.button ~stock:`CLOSE ~packing:bbox#pack () in
  ignore (close#connect#clicked ~callback:Main.quit);

  ignore (window#connect#destroy ~callback:Main.quit);
  ignore (window#event#connect#key_press ~callback:handle_key);
  ignore (window#event#connect#configure ~callback:resized);
  
  new_pos (0,0);
  window#show ();
(*  prescale_cache ();*)
  Main.main () (* calls the GTK main event loop *)
    
let _ = main ()
