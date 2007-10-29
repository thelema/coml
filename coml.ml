(* dispimg - A very simple image viewer.
   
   Written by Shawn Wagner (shawnw@speakeasy.org) and released into the
   public domain.
   
   Compile with: 
   
   % make
   % make install
   
 *)

let opt_default opt def = match opt with Some v -> v | None -> def
   
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
let opt = { wrap = false; fullscreen = false; 
	    twopage = false; manga = true;
	    remove_failed = true; scale = Fit;
	    rar_exe = "/home/thelema/bin/rar"
	  }

let _ = 
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s IMAGEFILE ...\n" Sys.argv.(0);
    exit 1
  end

type book = { path : string; files: string array }
let books = Stack.create ()

let push_books path files = 
  if Array.length files > 0 then 
    Stack.push {path=path; files=files} books

let is_archive file = List.exists (Filename.check_suffix file) ["rar";"zip"]

let rec build_books = function 
    [] -> () 
  | h :: t when Sys.is_directory h ->
      let files = Sys.readdir h in
      push_books h files;
      build_books t
  | h :: t when is_archive h ->
      let td = Filename.concat Filename.temp_dir_name "coml" in
      (* extract archive to td *)
      build_books (td::t)
  | h :: t when not (Sys.file_exists h) -> build_books t
  | h :: t as l -> 
      let p1 = Filename.dirname h in 
      let b1, rest = List.partition (fun f -> Filename.dirname f = p1) l in
      push_books p1 (Array.of_list (List.map Filename.basename b1));
      build_books rest
let _ = build_books (Array.to_list Sys.argv)

let current_book = Stack.pop books
let max_index = ref (Array.length current_book.files - 1)

let get_page idx = Filename.concat current_book.path current_book.files.(idx)
let remove_file idx = 
  if idx <> !max_index then 
    Array.blit current_book.files (idx+1) current_book.files idx (!max_index-idx);
  decr max_index

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:900 ~height:700 ()
let pane = GPack.paned `VERTICAL ~packing:window#add ()
let spread_ebox = GBin.event_box ~packing:pane#add1 ()
let spread = GPack.hbox ~packing:spread_ebox#add ()
let _ = spread#set_homogeneous false; spread#set_spacing 0
let image2 = GMisc.image ~packing:spread#add ()
let image1 = GMisc.image ~packing:spread#add ()
let footer = GPack.hbox ~packing:pane#pack2 ()
let note = GMisc.label ~packing:(footer#pack ~expand:true) ()
let sep = GMisc.separator `VERTICAL ~packing:footer#pack ()
let bbox = GPack.button_box `HORIZONTAL ~packing:footer#pack ~layout:`END ()
(*let _ = let newsty = spread#misc#style#copy in newsty#set_bg [`NORMAL,`BLACK]; spread#misc#set_style newsty (* set the background black *)*)

let widget_size ?cap widget = 
  let {Gtk.x=x0; y=y0; width=width; height=height} = 
    widget#misc#allocation in 
  match cap with
      Some i -> (max width i, max height i)
    | None -> (width, height)

let pixbuf_size pix =
  let w = GdkPixbuf.get_width pix 
  and h = GdkPixbuf.get_height pix in
  (w,h)

let target_size = ref (widget_size spread)

type pic = {scaled: GdkPixbuf.pixbuf option; full: GdkPixbuf.pixbuf}
type file = Failed | Empty | Entry of pic
type cache = {mutable pos: int; mutable pics: file array}
let cache_past = ref 1 and cache_future = ref 1
let cache_size () = !cache_past + !cache_future + 1
let cache_null = Empty
let failed_load = { scaled = Some (GdkPixbuf.create 1 1 ()); 
		    full = GdkPixbuf.create 1 1 ()}
let image_cache = { pos=0; pics=Array.make (cache_size ()) cache_null; } 
let cache_last_idx () = min (image_cache.pos + (cache_size ()) - 1) !max_index

let within_cache_range idx =
  idx >= image_cache.pos && idx <= cache_last_idx ()

let get_cache' idx =
  try
    match image_cache.pics.(idx - image_cache.pos) with
	Entry cb -> cb
      | Empty -> raise Not_found
      | Failed -> failed_load
  with Invalid_argument _ -> raise Not_found

let idle_fill = ref false
exception Cache_modified of int (* should only be raised if !idle_fill = true *)

let set_cache idx v =
  image_cache.pics.(idx - image_cache.pos) <- v;
  if !idle_fill then raise (Cache_modified idx)

let is_vert idx = 
  let (w0,h0) = pixbuf_size (get_cache' idx).full in
  w0 < h0

let can_twopage idx =
  try 
    opt.twopage && is_vert idx && idx < !max_index && is_vert (idx + 1)
  with Not_found -> false

let status_to_string entry = match entry.scaled with Some _ -> "S" | None -> "F"

let print_cache () = 
  Printf.eprintf "Cache: p=%d; pics={" image_cache.pos;
  let print_pic = function 
      Empty -> Printf.eprintf "None-" 
    | Failed -> Printf.eprintf "FAIL-" 
    | Entry p -> Printf.eprintf "%s-" (status_to_string p) 
  in
  Array.iter print_pic image_cache.pics;
  Printf.eprintf "}\n"

let set_status str = Printf.eprintf "%.2f: " (Sys.time()); prerr_endline str; note#set_label str

let recenter_cache ctr =
set_status (Printf.sprintf "Recentering cache to %d" ctr);
   let new_pos = max (ctr - !cache_past) 0 in
   Printf.eprintf "pos: %d new_pos: %d\n" image_cache.pos new_pos;
   print_cache ();
   let ic = image_cache.pics in
   let ic2 = Array.make (cache_size ()) cache_null in
   let new_size = Array.length ic2 and old_size = Array.length ic in
   let old_start, old_end = image_cache.pos, image_cache.pos + (old_size - 1)
   and new_start, new_end = new_pos, new_pos + new_size - 1 in
   let overlap_start = max old_start new_start
   and overlap_end = min old_end new_end in
   if overlap_start <= overlap_end then
     Array.blit ic (overlap_start-old_start) ic2 (overlap_start-new_start)
       (overlap_end - overlap_start + 1);

   image_cache.pics <- ic2;
   image_cache.pos <- new_pos;
   print_cache ()

let scale (wi,hi) ar =
  let w1 = int_of_float (float_of_int wi *. ar) 
  and h1 = int_of_float (float_of_int hi *. ar) in
  (w1,h1)

let rec load_cache idx = 
  try 
set_status (Printf.sprintf "Loading img %d" idx);
(*Printf.eprintf "L:%d=" idx;*)
    let cb = { scaled=None; full = GdkPixbuf.from_file (get_page idx)} in
(*Printf.eprintf "%dx%d  " (fst (pixbuf_size cb.full)) (snd (pixbuf_size cb.full));*)
    set_cache idx (Entry cb)
  with GdkPixbuf.GdkPixbufError(_,msg) ->
(*    let d = GWindow.message_dialog ~message:msg ~message_type:`ERROR
      ~buttons:GWindow.Buttons.close ~show:true () in
    ignore(d#run ()); *)
    set_status (Printf.sprintf "Failed to load %s" (get_page idx));
    if opt.remove_failed then
      (remove_file idx; load_cache idx)
    else
      set_cache idx Failed

let get_cache idx = 
(*Printf.eprintf "get_c %d\n" idx; *)
  if not (within_cache_range idx) then (* cache underflow *)
    recenter_cache idx;
  try get_cache' idx 
  with Not_found -> load_cache idx; get_cache' idx

let load_cache_if_empty idx =
  try ignore(get_cache' idx)
  with Not_found -> load_cache idx

let image_idx = ref 0

let on_screen idx = idx = !image_idx || (idx = (!image_idx + 1) && can_twopage (!image_idx))

let scale_factor (wt,ht) (wi, hi) =
  let wt = float_of_int wt and ht = float_of_int ht
  and wi = float_of_int wi and hi = float_of_int hi in
  let ar_t = wt /. ht and ar_i = wi /. hi in
  if ar_t > ar_i then ht /. hi else wt /. wi

let size_diff (w1,h1) (w2,h2) = abs(w1-w2) > 2 || abs(h1-h2) > 2
let lacks_size' size pb = size_diff (pixbuf_size pb) size
let lacks_size size idx =
  match (get_cache' idx).scaled with
      None -> true
    | Some sc -> lacks_size' size sc


let scaled_size ~target ~image =
  let ar = match opt.scale with
      Fit -> min 1.0 (scale_factor target image)
    | Fixed_AR ar -> ar
  in
  scale image ar

let scaled_pair ~target ~image1 ~image2 =
  let w0,h0 = target in
  let w1,h1 = image1 and w2,h2 = image2 in
  let ar = match opt.scale with
      Fit -> 
	let tar1 = (float_of_int w0) /. (float_of_int (w1 + w2))
	and tar2 = (float_of_int h0) /. (float_of_int h1)
	and tar3 = (float_of_int h0) /. (float_of_int h2) in
	min 1.0 (min tar1 (min tar2 tar3))
    | Fixed_AR ar -> ar
  in
  (scale image1 ar, scale image2 ar)

let reset_cache idx = 
  if within_cache_range idx then
    set_cache idx cache_null

let icf_task = ref None
let show_task = ref None

let rec scale_cache_idle idx size () = 
set_status (Printf.sprintf "Resizing img %d to %dx%d" idx (fst size) (snd size));
  let scale_cache idx size =
    let hyper_scale (width, height) pb =
      let out_b = GdkPixbuf.create width height () in
      GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`HYPER pb;
      out_b
    in
    let pic = get_cache' idx in
    let out = hyper_scale size pic.full in
    set_cache idx ( Entry {pic with scaled = Some out} )
  in
  try 
Printf.eprintf "IS:%d" idx;
    if lacks_size size idx then (
Printf.eprintf "->%dx%d" (fst size) (snd size);
      scale_cache idx size; 
      if on_screen idx then show_spread ();
      if lacks_size size idx
        then 
	  let sw,sh = match (get_cache' idx).scaled with None -> 0,0 | Some pb -> pixbuf_size pb
	  and tw,th = size in
	  Printf.eprintf "FAIL %dx%d <> %dx%d " sw sh tw th;
        else 
	  Printf.eprintf "ok  "
    ) else (
      Printf.eprintf "skip  ";
    );
    false
  with Not_found -> Printf.eprintf "%d NFOUND " idx; false

and idle_scale idx scl_size = 
  ignore (Idle.add ~prio:300 (scale_cache_idle idx scl_size))
and scale_cache_pre idx =
  try 
    let pic = get_cache' idx in
    let full_size = pixbuf_size pic.full in
    let scl_size = scaled_size (!target_size) full_size in
    Printf.eprintf "PS:%d" idx;
    let do_scale = match pic.scaled with
      | None -> true | Some spb -> lacks_size' scl_size spb
    in
    if do_scale then (
      let w0,h0 = pixbuf_size (opt_default pic.scaled pic.full)
      and w1,h1 = scl_size and wt, ht = !target_size in
      Printf.eprintf "is:%dx%d to:%dx%d tgt:%dx%d \n" w0 h0 w1 h1 wt ht;
      idle_scale idx scl_size
    ) else
      Printf.eprintf "skip "
  with Not_found -> ()

and display idx tgt_image = 
  let nearest_scale (width, height) pb =
    let out_b = GdkPixbuf.create width height () in
    GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`NEAREST pb;
    out_b
  in
  (* generate simple preview *)
  let pic = get_cache idx in
  let scl_size = scaled_size ~target:(widget_size ~cap:200 spread) 
    ~image:(pixbuf_size pic.full) in
  let pb = opt_default pic.scaled (nearest_scale scl_size pic.full) in
  tgt_image#set_pixbuf pb;
  ignore (Glib.Main.iteration true);
  target_size := widget_size ~cap:200 spread;
  let scl_size = scaled_size ~target:!target_size ~image:(pixbuf_size pic.full) in
  if lacks_size' scl_size pb then (
    if pic.scaled <> None then Printf.eprintf "RE";
    let w0,h0 = pixbuf_size pb and w1,h1 = scl_size and wt, ht = !target_size in
    Printf.eprintf "SCALE:%d is:%dx%d to:%dx%d tgt:%dx%d \n" idx w0 h0 w1 h1 wt ht;
    idle_scale idx scl_size
  );

and idle_cache_fill () = 
  try 
    idle_fill := true;
    set_status "ICF started";
    
    (* load all pictures into cache *)
    for idx = image_cache.pos to cache_last_idx () do
(*Printf.eprintf "LC: %d," idx;*)
      ignore (load_cache_if_empty idx);
    done;
    (* scale all pictures in cache *)
    for idx = image_cache.pos to cache_last_idx () do
(*Printf.eprintf "SC: %d," idx;*)
      ignore (scale_cache_pre idx);
    done;
    idle_fill := false; 
(*Printf.eprintf "Done filling\n";*)
    icf_task := None;
    false (* we're done filling the cache *)
  with Cache_modified idx -> 
    idle_fill := false; 
    if on_screen idx then (show_spread (); true)
    else true

and show_spread' () =
(*   failwith ("width=" ^ string_of_int width ^ " height=" ^ string_of_int height);*) 
  if can_twopage !image_idx then (
set_status (Printf.sprintf "Displaying img %d,%d" !image_idx (!image_idx+1));
    image2#misc#show ();
    display !image_idx image1;
    display (!image_idx+1) image2;
    window#set_title (Printf.sprintf "Image %d,%d of %d"
		      !image_idx (!image_idx+1) !max_index);
  ) else (
set_status (Printf.sprintf "Displaying img %d - %s" !image_idx (get_page !image_idx));
    image2#misc#hide ();
    display !image_idx image1;
    window#set_title (Printf.sprintf "Image %d of %d" !image_idx !max_index);
  );
  show_task := None;
  false
    (*  window#resize ~width:(max width 260) ~height:(height + 40)*)

and show_spread () = 
  match !show_task with
      None -> 
	show_task := Some (Idle.add ~prio:115 show_spread')
    | Some _ -> ()

let start_icf () =
  match !icf_task with
      None -> 
	icf_task := Some (Idle.add ~prio:250 idle_cache_fill);
    | Some _ -> ()

let new_pos idx = 
  if !image_idx <> idx then begin 
    image_idx := idx;
    recenter_cache !image_idx;
    start_icf ();
    show_spread ()
  end

let first_image () = set_status "At beginning of book"; new_pos 0

let last_image () = set_status "At end of book"; new_pos (if can_twopage (!max_index-1) then !max_index - 1 else !max_index)

let prev_image () =
  let movement = if can_twopage (max (!image_idx-2) 0) then 2 else 1 in
set_status (Printf.sprintf "Going back %s @ %d" (if movement = 2 then "two pages" else "one page")!image_idx);

  if !image_idx - movement < 0 then
    if opt.wrap then last_image () else first_image ()
  else new_pos (!image_idx - movement)
    
let next_image () =
  let movement = if can_twopage !image_idx then 2 else 1 in
set_status (Printf.sprintf "Going forward %s @ %d" (if movement = 2 then "two pages" else "one page")!image_idx);
  
  if !image_idx + movement >= !max_index then
    if opt.wrap then first_image () else last_image()
  else new_pos (!image_idx + movement)
    
let toggle_fullscreen () =
  if opt.fullscreen then (
    opt.fullscreen <- false;
    window#unfullscreen ();

   ) else (
    opt.fullscreen <- true;
    window#fullscreen ();
   );
(*  spread#misc#set_size_request ~width:(Gdk.Screen.width ()) ~height:(Gdk.Screen.height ()) (); *)
  show_spread ()

let toggle_twopage () =
  opt.twopage <- not opt.twopage;
  (* increase cache size if twopage *)
  cache_past := if opt.twopage then 2 else 1;
  cache_future := if opt.twopage then 3 else 1;  (* include second page of currint in future *)
  recenter_cache !image_idx;
  start_icf ();
  show_spread ()

let toggle_manga () =
  opt.manga <- not opt.manga;
  spread#reorder_child image2#coerce (if opt.manga then 0 else 1)

let go_to_page_dialog () =
  let _ = GWindow.dialog ~parent:window ~title:"Go to page"  () in
  () (* FIXME COMPLETE THIS *)

let zoom ar_val ar_func = 
  opt.scale <-( match opt.scale with
		    Fit -> Fixed_AR ar_val 
		  | Fixed_AR ar -> ar_func ar);
  let rescale idx = 
    let cur_size = pixbuf_size (get_cache idx).full in
    let target_size = scaled_size cur_size (widget_size image1) in
    Printf.eprintf "RO:%d->%dx%d " idx (fst target_size) (snd target_size);
    idle_scale idx target_size
  in
  rescale !image_idx; 
  if can_twopage !image_idx then rescale (!image_idx+1)
  
open GdkKeysyms

let handle_key event =
  let kv = GdkEvent.Key.keyval event in
  if kv = _q then Main.quit () else
  if kv = _Left or kv = _Down or kv = _BackSpace then prev_image() else
  if kv = _Right or kv = _Up or kv = _space then next_image() else
  if kv = _f then toggle_fullscreen () else
  if kv = _t then toggle_twopage () else
  if kv = _m then toggle_manga () else
  if kv = _w then opt.wrap <- not opt.wrap else
  if kv = _l then load_cache !image_idx else
  if kv = _z then zoom 1.0 (fun _ -> Fit) else
  if kv = _minus then zoom 0.9 (fun ar -> Fixed_AR (ar *. 0.9)) else
  if kv = _plus then zoom (1.0 /. 0.9) (fun ar -> Fixed_AR (ar /. 0.9))

  ; true

let resized event =
  let image_width = GdkEvent.Configure.width event
  and image_height = GdkEvent.Configure.height event in
  
  Printf.eprintf "RESIZE: %dx%d\n" image_width image_height;
  show_spread ();
  true
  

let main () =

  let prev = GButton.button ~stock:`GO_BACK ~packing:bbox#pack ()
  and next = GButton.button ~stock:`GO_FORWARD ~packing:bbox#pack () in
  ignore (prev#connect#clicked ~callback:prev_image);
  ignore (next#connect#clicked ~callback:next_image);

  let close = GButton.button ~stock:`CLOSE ~packing:bbox#pack () in
  ignore (close#connect#clicked ~callback:Main.quit);

  ignore (window#connect#destroy ~callback:Main.quit);
  ignore (window#event#connect#key_press ~callback:handle_key);
(*  ignore (window#event#connect#configure ~callback:resized);*)
  ignore (pane#event#connect#configure ~callback:resized);
  show_spread ();
  window#show ();
  Main.main ()
    
let _ = Printexc.print main ()
