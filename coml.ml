(* dispimg - A very simple image viewer.
   
   Written by Shawn Wagner (shawnw@speakeasy.org) and released into the
   public domain.
   
   Compile with: 
   
   % make
   % make install
   
 *)
   
open GMain
   
type scaling = Fit | Fixed_AR of float

type options = { mutable wrap: bool; 
		 mutable fullscreen: bool;
		 mutable twopage: bool;
		 mutable manga: bool;
		 mutable remove_failed: bool; 
		 mutable scale: scaling;
	       }
let opt = { wrap = false; fullscreen = false; 
	    twopage = false; manga = true;
	    remove_failed = true; scale = Fit;
	  }

let _ = 
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s IMAGEFILE ...\n" Sys.argv.(0);
    exit 1
  end

let files = Array.init (Array.length Sys.argv - 1) (fun i -> Sys.argv.(i+1))
let max_index = ref (Array.length files - 1)

let get_page idx = files.(idx)
let remove_file idx = 
  if idx <> !max_index then 
    Array.blit files (idx+1) files idx (!max_index-idx);
  decr max_index

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:900 ~height:700 ()
(*let pane = GPack.paned `VERTICAL ~packing:window#add () *)
let cont = GPack.hbox ~packing:window#add ()
let _ = cont#set_homogeneous false; cont#set_spacing 0
let image2 = GMisc.image ~packing:(cont#pack ~expand:true ~fill:true) ()
let image1 = GMisc.image ~packing:cont#add ()

let _ = let newsty = window#misc#style#copy in newsty#set_bg [`NORMAL,`BLACK]; window#misc#set_style newsty (* set the background black *)

let pixbuf_size pix =
  let w = GdkPixbuf.get_width pix 
  and h = GdkPixbuf.get_height pix in
  (w,h)

type status = Full | Scaled 
type pic = {st: status; pb: GdkPixbuf.pixbuf}
type file = Failed | Empty | Entry of pic
type cache = {mutable pos: int; pics: file array}
let cache_radius = ref 4
let cache_size () = 2 * !cache_radius + 1
let cache_null = Empty
let failed_load = { st=Scaled; pb=GdkPixbuf.create 1 1 ()}
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
  let (w0,h0) = pixbuf_size (get_cache' idx).pb in
  w0 < h0

let can_twopage idx =
  try 
    opt.twopage && is_vert idx && idx < !max_index && is_vert (idx + 1)
  with Not_found -> false

let status_to_string = function | Full -> "F" | Scaled -> "S" 

let print_cache () = 
  Printf.eprintf "Cache: p=%d; pics={" image_cache.pos;
  let print_pic = function 
      Empty -> Printf.eprintf "None-" 
    | Failed -> Printf.eprintf "FAIL-" 
    | Entry p -> Printf.eprintf "%s-" (status_to_string p.st) 
  in
  Array.iter print_pic image_cache.pics;
  Printf.eprintf "}\n"

let recenter_cache ctr =
   let new_pos = max (ctr - !cache_radius) 0 in
   Printf.eprintf "pos: %d new_pos: %d\n" image_cache.pos new_pos;
   print_cache ();
   
   let toss = min (abs (new_pos - image_cache.pos)) (cache_size ()) in
   let keep = (cache_size ()) - toss in
   let ic = image_cache.pics in
   if new_pos > image_cache.pos then (
(*     Printf.eprintf "FW - blit ic %d ic %d %d; fill ic %d %d;\n" toss 0 keep keep toss; *)
     Array.blit ic toss ic 0 keep;
     Array.fill ic keep toss cache_null;
   ) else (
(*     Printf.eprintf "BW - blit ic %d ic %d %d; fill ic %d %d;\n" 0 keep keep 0 toss;*)
     Array.blit ic 0 ic keep keep;
     Array.fill ic 0 toss cache_null;
   );
   image_cache.pos <- new_pos;
   print_cache ()

let scale (wi,hi) ar =
  let w1 = int_of_float (float_of_int wi *. ar) 
  and h1 = int_of_float (float_of_int hi *. ar) in
  (w1,h1)

let widget_size ?cap widget = 
  let {Gtk.x=x0; y=y0; width=width; height=height} = 
    widget#misc#allocation in 
  match cap with
      Some i -> (max width i, max height i)
    | None -> (width, height)

let rec load_cache idx = 
  try 
Printf.eprintf "L:%d=" idx;
    let cb = { st = Full; pb = GdkPixbuf.from_file (get_page idx)} in
Printf.eprintf "%dx%d  
" (fst (pixbuf_size cb.pb)) (snd (pixbuf_size cb.pb));
    set_cache idx (Entry cb)
  with GdkPixbuf.GdkPixbufError(_,msg) ->
(*    let d = GWindow.message_dialog ~message:msg ~message_type:`ERROR
      ~buttons:GWindow.Buttons.close ~show:true () in
    ignore(d#run ()); *)
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

let container_size = ref (widget_size ~cap:100 cont)

let scale_factor (wt,ht) (wi, hi) =
  let wt = float_of_int wt and ht = float_of_int ht
  and wi = float_of_int wi and hi = float_of_int hi in
  let ar_t = wt /. ht and ar_i = wi /. hi in
  if ar_t > ar_i then ht /. hi else wt /. wi

let size_diff (w1,h1) (w2,h2) = abs(w1-w2) < 2 && abs(h1-h2) < 2
let has_size pb size = size_diff (pixbuf_size pb) size

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

let rec scale_cache_idle idx size () = 
  let scale_cache idx size =
    let hyper_scale (width, height) pb =
      let out_b = GdkPixbuf.create width height () in
      GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`HYPER pb;
      out_b
    in
    let pic = get_cache' idx in
    match pic.st with
	Full ->
	  let out = hyper_scale size pic.pb in
	  set_cache idx ( Entry {st=Scaled; pb=out} )
      | Scaled -> 
	  load_cache idx;
	  let out = hyper_scale size (get_cache' idx).pb in
	  set_cache idx ( Entry {st=Scaled; pb=out} )
  in
  try 
Printf.eprintf "IS:%d->%dx%d" idx (fst size) (snd size);
    if not (has_size (get_cache' idx).pb size) then (
      scale_cache idx size; 
      if on_screen idx then show_spread ();
      let is_size = has_size (get_cache' idx).pb size in
if is_size then Printf.eprintf "ok \n" else Printf.eprintf "FAIL \n";
    ) else (
      Printf.eprintf "skip ";
    );
    false
  with Not_found -> Printf.eprintf "NFOUND "; false
    
and scale_cache_pre idx =
  if can_twopage idx then
    let pic = get_cache idx and pic2 = get_cache (idx+1) in
    match pic.st,pic2.st with
	Full,Full -> 
	  let cur_size = pixbuf_size pic.pb 
	  and cur_size2 = pixbuf_size pic2.pb in
(*	  let ts1,ts2 = scaled_pair !container_size cur_size cur_size2 in*)
	  let ts1 = scaled_size (widget_size image1) cur_size
	  and ts2 = scaled_size (widget_size image2) cur_size2 in
	  (*Printf.eprintf "twopage scale %dx%d and %dx%d in %dx%d  idx %d,%d by %f\n" w1 h1 w2 h2 w0 h0 idx (idx+1) tar; *)
	  ignore(Idle.add (scale_cache_idle idx ts1));
	  ignore(Idle.add (scale_cache_idle (idx+1) ts2))
      | Scaled, _ | _, Scaled -> ()
  else
    let pic = get_cache idx in
    match pic.st with
	Full -> 
	  let cur_size = pixbuf_size pic.pb in
	  let target_size = scaled_size !container_size cur_size in
Printf.eprintf "PS:%d->%dx%d " idx (fst target_size) (snd target_size);
	  ignore(Idle.add (scale_cache_idle idx target_size))
      | Scaled -> ()

and set_image_from_cache idx tgt_image = 
  let nearest_scale (width, height) pb =
    let out_b = GdkPixbuf.create width height () in
    GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`NEAREST pb;
    out_b
  in
  (* generate simple preview *)
  let pic = get_cache idx in
  match pic.st with
      Scaled -> 
	tgt_image#set_pixbuf pic.pb;
    | Full -> 
	let target = widget_size ~cap:100 tgt_image in
	let scl_size = scaled_size ~target ~image:(pixbuf_size pic.pb) in
	if not (has_size pic.pb scl_size) then (
	  Printf.eprintf "SIC:%d to %dx%d tgt %dx%d \n" idx (fst scl_size) (snd scl_size) (fst target) (snd target);
	  ignore(Idle.add (scale_cache_idle idx scl_size));
	);
	tgt_image#set_pixbuf (nearest_scale scl_size pic.pb)

and idle_cache_fill () = 
  try 
    idle_fill := true;
(*    print_cache ();*)
    ignore (get_cache (!image_idx + 1));
    if can_twopage !image_idx then (
      ignore (get_cache (!image_idx + 2));
      ignore (get_cache (!image_idx + 3));
    );
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
    false (* we're done filling the cache *)
  with Cache_modified idx -> 
    idle_fill := false; 
    if on_screen idx then (show_spread (); false (*because another idle_fill job will be queued by show_spread *))
    else true

and show_spread' () =
(*   failwith ("width=" ^ string_of_int width ^ " height=" ^ string_of_int height);*) 
  container_size := widget_size cont;
  if can_twopage !image_idx then (
    image2#misc#show ();
    set_image_from_cache !image_idx image1;
    set_image_from_cache (!image_idx+1) image2;
    window#set_title (Printf.sprintf "Image %d,%d of %d"
		      !image_idx (!image_idx+1) (Array.length Sys.argv - 1));
  ) else (
    image2#misc#hide ();
    set_image_from_cache !image_idx image1;
  );
  ignore(Idle.add idle_cache_fill);
  false
    (*  window#resize ~width:(max width 260) ~height:(height + 40)*)

and show_spread () = 
(*  Printf.eprintf "csize: %d x %d\n" (fst !container_size) (snd !container_size);*)
  window#set_title (Printf.sprintf "Image %d of %d"
		      !image_idx !max_index);
  ignore(Idle.add show_spread')
    
let prev_image () =
  image_idx := !image_idx - (if can_twopage (max (!image_idx-2) 0) then 2 else 1);
  if !image_idx < 0 then
    image_idx := if opt.wrap then !max_index else 0;
  show_spread ()
    
let next_image () =
  image_idx := !image_idx + (if can_twopage !image_idx then 2 else 1);
  if !image_idx >= !max_index then
    image_idx := if opt.wrap then 0 else !max_index;
  show_spread ()
    
let toggle_fullscreen () =
  if opt.fullscreen then (
    opt.fullscreen <- false;
    window#unfullscreen ();
   ) else (
    opt.fullscreen <- true;
    window#fullscreen ();
   );
  cont#misc#set_size_request ~width:(Gdk.Screen.width ()) ~height:(Gdk.Screen.height ()) ();
  show_spread ()

let toggle_twopage () =
  opt.twopage <- not opt.twopage;
  (* TODO: increase cache size if twopage *)
  show_spread ()

let toggle_manga () =
  opt.manga <- not opt.manga;
  cont#remove image1#coerce; cont#remove image2#coerce;
  if opt.manga then (cont#add image2#coerce; cont#add image1#coerce;)
      else (cont#add image1#coerce; cont#add image2#coerce;)

let go_to_page_dialog () =
  let _ = GWindow.dialog ~parent:window ~title:"Go to page"  () in
  ()

let zoom ar_val ar_func = 
  opt.scale <-( match opt.scale with
		    Fit -> Fixed_AR ar_val 
		  | Fixed_AR ar -> ar_func ar);
  let rescale idx = 
    let cur_size = pixbuf_size (get_cache idx).pb in
    let target_size = scale cur_size ar_val in
    Printf.eprintf "RO:%d->%dx%d " idx (fst target_size) (snd target_size);
    ignore(Idle.add (scale_cache_idle idx target_size))
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
(*  image_width := GdkEvent.Configure.width event;
  image_height := GdkEvent.Configure.height event; *)
(*  container_size := widget_size cont;*)
  cont#misc#set_size_request ();
  show_spread ();
  true
  

let main () =
(*
  let bbox = GPack.button_box `HORIZONTAL ~packing:pane#pack2 ~layout:`END ()
  in
  if Array.length files > 1 then begin
    let prev = GButton.button ~stock:`GO_BACK ~packing:bbox#pack ()
    and next = GButton.button ~stock:`GO_FORWARD ~packing:bbox#pack () in
    ignore (prev#connect#clicked ~callback:prev_image);
    ignore (next#connect#clicked ~callback:next_image)
  end;		   
  let close = GButton.button ~stock:`CLOSE ~packing:bbox#pack () in
  ignore (close#connect#clicked ~callback:Main.quit);
*)
  ignore (window#connect#destroy ~callback:Main.quit);
  ignore (window#event#connect#key_press ~callback:handle_key);
  ignore (window#event#connect#configure ~callback:resized);
  show_spread ();
  window#show ();
  Main.main ()
    
let _ = Printexc.print main ()
