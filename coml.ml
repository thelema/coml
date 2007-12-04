(* dispimg - A very simple image viewer.
   
   Written by Shawn Wagner (shawnw@speakeasy.org) and released into the
   public domain.
   
   Compile with: 
   
   % make
   % make install
   
 *)

let opt_default opt def = match opt with Some v -> v | None -> def
let (|>) x f = f x
   
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

type book = { path : string; mutable files: string array }
type library = {prev : book Stack.t; next : book Stack.t}
let books = {prev=Stack.create ();
	     next=Stack.create ()}

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

let push_books path files = 
  if Array.length files > 0 then 
    Array.sort numeric_compare files;
    Stack.push {path=path; files=files} books.next

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
    | `Sev_zip -> 255
    | `None -> assert false

let rec rec_del path = 
  let remove fn = (*Printf.eprintf "Removing: %s"*) Sys.remove fn in
  Sys.readdir path
    |> Array.map (fun fn -> Filename.concat path fn)
    |> Array.iter (fun fn -> if Sys.is_directory fn
		  then rec_del fn
		  else remove fn)
  (*remove path*)


let rec build_books = function 
    [] -> () 
  | h :: t when not (Sys.file_exists h) -> build_books t
  | h :: t when Sys.is_directory h ->
      let files = Sys.readdir h in
      build_books (t @ (List.map (Filename.concat h) (Array.to_list files)))
  | h :: t when is_archive h ->
      let td = Filename.concat Filename.temp_dir_name (Printf.sprintf "coml-%d" (Random.bits ())) in
      (* extract archive to td *)
      let return_code = extract_archive h td in
      if return_code = 0 && Sys.file_exists td then begin
	at_exit (fun () -> rec_del td);
	(* on quit, remove the extracted archive *)
	build_books (td::t)
      end else
	build_books t
  | h :: t as l -> 
      let p1 = Filename.dirname h in 
      let b1, rest = List.partition (fun f -> Filename.dirname f = p1) l in
      b1 |>
	List.filter Sys.file_exists |>
	List.map Filename.basename |>
	List.filter is_picture |>
	Array.of_list |>
	push_books p1;
      build_books rest

let current_book () = Stack.top books.next
let max_index () = Array.length (current_book()).files - 1

let book_count () = Stack.length books.next + Stack.length books.prev
and cur_book_number () = Stack.length books.prev + 1

let get_page idx = 
  let cb = current_book () in Filename.concat cb.path cb.files.(idx)

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:900 ~height:700 ()
let pane = GPack.paned `VERTICAL ~packing:window#add ()
let scroller = GBin.scrolled_window ~packing:pane#add1 ()
let _ = scroller#set_hpolicy `AUTOMATIC; scroller#set_vpolicy `AUTOMATIC
let spread = GPack.layout ~packing:scroller#add ~width:9999 ~height:9999 ()
(*let _ = spread#set_homogeneous false; spread#set_spacing 0 *)
let image2 = GMisc.image ~packing:spread#add ()
let image1 = GMisc.image ~packing:spread#add ()
let footer = GPack.hbox ~packing:pane#pack2 ()
let file = GMisc.label ~packing:(footer#pack ~expand:true) ()
let _ = GMisc.separator `VERTICAL ~packing:footer#pack ()
let note = GMisc.label ~packing:(footer#pack ~expand:true) ()
let _ = GMisc.separator `VERTICAL ~packing:footer#pack ()
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

type pic = {full: GdkPixbuf.pixbuf; 
	    mutable scaled: GdkPixbuf.pixbuf option; 
	    mutable t_size: (int * int) option}
type file = Failed | Empty | Entry of pic
type cache = {mutable pos: int; mutable pics: file array}
let cache_past = ref 1 and cache_future = ref 1
let cache_size () = !cache_past + !cache_future + 1
let cache_null = Empty
let failed_load = { scaled = Some (GdkPixbuf.create 1 1 ()); 
		    full = GdkPixbuf.create 1 1 ();
		    t_size = Some (1,1) }
let image_cache = { pos=0; pics=Array.make (cache_size ()) cache_null; } 
let cache_last_idx () = min (image_cache.pos + (cache_size ()) - 1) (max_index ())

let full_size pic = pixbuf_size pic.full
let cur_size pic = pixbuf_size (opt_default pic.scaled pic.full)

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
exception Cache_modified of pic (* should only be raised if !idle_fill = true *)

let set_cache idx v =
  image_cache.pics.(idx - image_cache.pos) <- v

let is_vert idx = 
  let (w0,h0) = full_size (get_cache' idx) in
  w0 < h0

let can_twopage idx =
  try 
    opt.twopage && is_vert idx && idx < max_index () && is_vert (idx + 1)
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

let reset_cache_all () = image_cache.pics <- Array.make (cache_size ()) cache_null

let next_book () = 
  let cur = Stack.pop books.next in
  Stack.push cur books.prev; 
  reset_cache_all ()

let prev_book () = 
  let cur = Stack.pop books.prev in
  Stack.push cur books.next; 
  reset_cache_all ()


let scale (wi,hi) ar =
  let w1 = int_of_float (float_of_int wi *. ar) 
  and h1 = int_of_float (float_of_int hi *. ar) in
  (w1,h1)

let image_idx = ref 0

let remove_file idx = 
  let cb = current_book () in
  if max_index () > 0 then begin
    cb.files <- Array.init (max_index ()) (fun i -> if i < idx then cb.files.(i) else cb.files.(i+1));
    if !image_idx > max_index() then image_idx := max_index()
  end else begin
    ignore (Stack.pop books.next);
    image_idx := 0;
    if Stack.length books.next < 1 then
      Stack.push (Stack.pop books.prev) books.next
  end

let rec load_cache idx = 
  try 
set_status (Printf.sprintf "Loading img %d" idx);
(*Printf.eprintf "L:%d=" idx;*)
    let pic = { full = GdkPixbuf.from_file (get_page idx); scaled=None; t_size=None} in
(*Printf.eprintf "%dx%d  " (fst (pixbuf_size cb.full)) (snd (pixbuf_size cb.full));*)
    set_cache idx (Entry pic);
    if !idle_fill then raise (Cache_modified pic)
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

let on_screen_pics = ref []

let on_screen pic = (* idx = !image_idx || (idx = (!image_idx + 1) && can_twopage (!image_idx)) *) List.memq pic !on_screen_pics 

let size_diff (w1,h1) (w2,h2) = abs(w1-w2) > 2 || abs(h1-h2) > 2
let lacks_size size pb = size_diff (pixbuf_size pb) size
let lacks_t_size pic =
  match pic.scaled,pic.t_size with
    | None,None | Some _, None -> None
    | None,Some s -> Some s
    | Some sc, Some s -> if lacks_size s sc then Some s else None

let scale_factor (wt,ht) (wi, hi) =
  let wt = float_of_int wt and ht = float_of_int ht
  and wi = float_of_int wi and hi = float_of_int hi in
  let ar_t = wt /. ht and ar_i = wi /. hi in
  if ar_t > ar_i then ht /. hi else wt /. wi

let scaled_size ~target ~image =
  let ar = match opt.scale with
    | Fit -> min 1.0 (scale_factor target image)
    | Fixed_AR ar -> ar
  in
  scale image ar
    
let scaled_pair ~target ~image1 ~image2 =
  match opt.scale with
      Fit -> 
	let w0,h0 = target in
	let w1,h1 = image1 and w2,h2 = image2 in
Printf.eprintf "Fitting %dx%d and %dx%d into %dx%d\n" w1 h1 w2 h2 w0 h0;
	let tar_w = min 1.0 ((float_of_int w0) /. (float_of_int (w1 + w2))) in
	if h1 = h2 then 
	  let tar_h = (float_of_int h0) /. (float_of_int h1) in
	  let ar = min tar_w tar_h in
	  (scale image1 ar, scale image2 ar)
	else
	  let tar_h1 = (float_of_int h0) /. (float_of_int h1)
	  and tar_h2 = (float_of_int h0) /. (float_of_int h2) in
	  let ar1 = min tar_w tar_h1 and ar2 = min tar_w tar_h2 in
	  (scale image1 ar1, scale image2 ar2)
    | Fixed_AR ar -> (scale image1 ar, scale image2 ar)
	
let reset_cache idx = if within_cache_range idx then set_cache idx cache_null

let icf_task = ref None
let show_task = ref None

let rec scale_cache_idle pic () = 
  let scale_cache pic (width, height) =
set_status (Printf.sprintf "Resizing img to %dx%d" width height);
    let scaled = GdkPixbuf.create width height () in
    GdkPixbuf.scale ~dest:scaled ~width ~height ~interp:`HYPER pic.full;
    pic.scaled <- Some scaled;
    if !idle_fill then raise (Cache_modified pic)
  in
  begin try 
    match lacks_t_size pic with
	None -> ()
      | Some size ->
	  scale_cache pic size; 
	  if on_screen pic then show_spread ()
  with Not_found -> ()
  end; false
    
and idle_scale pic size = 
  pic.t_size <- Some size;
  ignore (Idle.add ~prio:150 (scale_cache_idle pic))
and scale_cache_pre idx =
  try 
    let pic = get_cache' idx in
    let scl_size = scaled_size (widget_size scroller) (full_size pic) in
    idle_scale pic scl_size
  with Not_found -> ()

and idle_cache_fill () = 
  try 
    idle_fill := true;
    (* load all pictures into cache *)
    for idx = image_cache.pos to cache_last_idx () do
      ignore (load_cache_if_empty idx);
    done;
    (* scale all pictures in cache *)
    for idx = image_cache.pos to cache_last_idx () do
      ignore (scale_cache_pre idx);
    done;
    idle_fill := false; 
    icf_task := None;
    false (* we're done filling the cache *)
  with Cache_modified pic -> 
    idle_fill := false;
    if on_screen pic then (show_spread (); true)
    else true

and nearest_scale (width, height) pb =
    let out_b = GdkPixbuf.create width height () in
    GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`NEAREST pb;
    out_b
and quick_view size pic = 
  let pb1 = (opt_default pic.scaled pic.full) in
  if lacks_size size pb1 
  then nearest_scale size pic.full
  else pb1
and display1 idx = 
  (* generate simple preview *)
  let pic = get_cache idx in
  let w0,h0 = (widget_size scroller) in
Printf.eprintf "disp max_size: %dx%d\n" w0 h0;
  let w1,h1 = scaled_size ~target:(w0,h0) ~image:(full_size pic) in
  let pb = quick_view (w1,h1) pic in
  image1#set_pixbuf pb;
  on_screen_pics := [pic];
  let h_off = (h0 - h1) / 2 and w_off = (w0 - w1) / 2 in
  spread#move image1#coerce w_off h_off;
  ignore (Glib.Main.iteration true);
(*  target_size := widget_size ~cap:200 spread;
  let scl_size = scaled_size ~target:!target_size ~image:(full_size pic) in*)
  idle_scale pic (w1,h1)
and display2 idx1 idx2 = 
  (* generate simple preview *)
  let pic1 = get_cache idx1 and pic2 = get_cache idx2 in
  let w0,h0 = (widget_size scroller) in
Printf.eprintf "disp max_size: %dx%d\n" w0 h0;
  let (w1,h1), (w2,h2) = scaled_pair ~target:(w0,h0) ~image1:(full_size pic1) ~image2:(full_size pic2) in
  let pb1 = quick_view (w1,h1) pic1 and pb2 = quick_view (w2,h2) pic2 in
  image1#set_pixbuf pb1; image2#set_pixbuf pb2;
  on_screen_pics := [pic1; pic2];
  let h_off1 = (h0-h1) / 2 and h_off2 = (h0-h2) / 2 in
  spread#move image1#coerce 0 h_off1;
  spread#move image2#coerce w1 h_off2;
  ignore (Glib.Main.iteration true);
(*  target_size := widget_size ~cap:200 spread;
  let scl_size = scaled_size ~target:!target_size ~image:(full_size pic) in*)
  idle_scale pic1 (w1,h1); idle_scale pic2 (w2,h2)

and show_spread' () =
(*   failwith ("width=" ^ string_of_int width ^ " height=" ^ string_of_int height);*) 
  file#set_text (try Glib.Convert.filename_to_utf8 (get_page !image_idx) with Glib.Convert.Error (_,s) -> s);
  if can_twopage !image_idx then (
    set_status (Printf.sprintf "Displaying img %d,%d" !image_idx (!image_idx+1));
    image2#misc#show ();
    if opt.manga then
      display2 (!image_idx+1) !image_idx
    else
      display2 !image_idx (!image_idx+1);
(*    let w1, h1 = widget_size image1 and w2,h2 = widget_size image2 in
    spread#misc#set_size_request ~width:(w1+w2) ~height:(max h1 h2) (); *)
    window#set_title (Printf.sprintf "Image %d,%d of %d, Book %d of %d : %s"
		      !image_idx (!image_idx+1) (max_index()) (cur_book_number ()) (book_count()) (current_book()).path);
  ) else (
    set_status (Printf.sprintf "Displaying img %d" !image_idx);
    image2#misc#hide ();
    display1 !image_idx;
(*    let w1, h1 = widget_size image1 in
    spread#misc#set_size_request ~width:w1 ~height:h1 ();*)
    window#set_title (Printf.sprintf "Image %d of %d, Book %d of %d: %s" 
			!image_idx (max_index()) (cur_book_number ()) (book_count()) (current_book()).path);
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
    image_idx := idx;
    recenter_cache !image_idx;
    start_icf ();
    show_spread ()

let first_image () = set_status "At beginning of book"; new_pos 0

let last_image () = set_status "At end of book"; new_pos (if can_twopage (max_index()-1) then max_index () - 1 else max_index())

let past_end () = 
  if opt.wrap then first_image () else
  if Stack.length books.next <= 1 then set_status "At end of Library"
  else begin next_book (); first_image () end

let past_start () = 
  if opt.wrap then last_image () else
  if Stack.length books.prev < 1 then set_status "At start of Library"
  else begin prev_book (); last_image () end

let prev_image () =
  let movement = if can_twopage (max (!image_idx-2) 0) then 2 else 1 in
set_status (Printf.sprintf "Going back %s @ %d" (if movement = 2 then "two pages" else "one page")!image_idx);

  if !image_idx - movement < 0 then past_start ()
  else new_pos (!image_idx - movement)
    
let next_image () =
  let movement = if can_twopage !image_idx then 2 else 1 in
set_status (Printf.sprintf "Going forward %s @ %d" (if movement = 2 then "two pages" else "one page")!image_idx);
  
  if !image_idx + movement > max_index () then past_end ()
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
  show_spread()
(*  spread#reorder_child image2#coerce (if opt.manga then 0 else 1)*)

let go_to_page_dialog () =
  let _ = GWindow.dialog ~parent:window ~title:"Go to page"  () in
  () (* FIXME COMPLETE THIS *)

let zoom ar_val ar_func = 
  opt.scale <-( match opt.scale with
		    Fit -> Fixed_AR ar_val 
		  | Fixed_AR ar -> ar_func ar);
  let rescale idx = 
    let pic = get_cache idx in
    let target_size = scaled_size (full_size pic) (widget_size spread) in
    idle_scale pic target_size
  in
  rescale !image_idx; 
  if can_twopage !image_idx then rescale (!image_idx+1)

let zoom_out () = zoom 0.95 (fun ar -> Fixed_AR (ar *. 0.95))
and zoom_in () = zoom (1.0 /. 0.95) (fun ar -> Fixed_AR (ar /. 0.95))
and toggle_zoom () = zoom 1.0 (fun _ -> Fit)
  
open GdkKeysyms

let acts = [(_q, Main.quit);
	    (_Left, prev_image); (_Up, prev_image); (_BackSpace, prev_image);
	    (_Right, next_image); (_Down, next_image); (_space, next_image);
	    (_f, toggle_fullscreen); (_t, toggle_twopage);
	    (_m, toggle_manga); (_z, toggle_zoom);
	    (_minus, zoom_out); (_plus, zoom_in);
	    (_l, (fun () -> load_cache !image_idx));
	    (_w, (fun () -> opt.wrap <- not opt.wrap));
	    (_Page_Up, past_start); (_Page_Down, past_end);
	   ]

let handle_key event =
  let kv = GdkEvent.Key.keyval event in
  try 
    (List.assoc kv acts) (); true
  with Not_found -> false

let resized event =
  let image_width = GdkEvent.Configure.width event
  and image_height = GdkEvent.Configure.height event in
  Printf.eprintf "RESIZE: %dx%d\n" image_width image_height;
  show_spread ();
  false
  

let main () =
  Random.self_init ();
  build_books (Sys.argv |> Array.to_list |> List.tl |> List.rev);
  if max_index () = 0 then begin
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
  start_icf ();
  Main.main ()
    
let _ = main ()
