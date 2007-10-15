(* dispimg - A very simple image viewer.
   
   Written by Shawn Wagner (shawnw@speakeasy.org) and released into the
   public domain.
   
   Compile with: 
   
   % make
   % make install
   
 *)
   
open GMain
   
let opt_wrap = ref false
let opt_fullscreen = ref false
let opt_twopage = ref false
let opt_manga = ref false
   
let _  = 
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s IMAGEFILE ...\n" Sys.argv.(0);
    exit 1
  end

let files = Array.init (Array.length Sys.argv - 1) (fun i -> Sys.argv.(i+1))
let max_index = Array.length files - 1

let get_file idx = files.(idx)

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:760 ~height:600 ()
(*let pane = GPack.paned `VERTICAL ~packing:window#add () *)
let cont = GPack.hbox ~packing:window#add ~width:1000 ~height:1000 ()
let _ = cont#set_homogeneous true; cont#set_spacing 0
let image1 = GMisc.image ~packing:(cont#pack ~expand:true ~fill:true) ~width:1000 ~height:1000 ()
let image2 = GMisc.image ~packing:(cont#pack ~expand:true ~fill:true) ~width:1000 ~height:1000 ()

let _ = let newsty = window#misc#style#copy in newsty#set_bg [`NORMAL,`BLACK]; window#misc#set_style newsty (* set the background black *)

type status = Full | Scaled of float (* keep the scale factor *)
type pic = {st: status; w: int; h: int; pb: GdkPixbuf.pixbuf}
type cache = {mutable pos: int; pics: pic option array}
let cache_radius = 2
let cache_size = 2 * cache_radius + 1
let cache_null = None
let image_cache = {pos=0; pics=Array.make cache_size cache_null} (* pos == pix.(0).idx when pix.(0) has definition *)

let status_to_string = function 
    Full -> "F" 
  | Scaled a when a > 1.0 -> "E" 
  | Scaled a when a > 0.9 -> "9" 
  | Scaled a when a > 0.8 -> "8" 
  | Scaled a when a > 0.7 -> "7" 
  | Scaled a when a > 0.6 -> "6" 
  | Scaled a when a > 0.5 -> "5" 
  | Scaled a when a > 0.4 -> "4" 
  | Scaled a when a > 0.3 -> "3" 
  | Scaled a when a > 0.2 -> "2" 
  | Scaled a when a > 0.1 -> "1"
  | Scaled _ -> "0"

let print_cache () = 
  Printf.eprintf "Cache: p=%d; pics={" image_cache.pos;
  let print_pic = function None -> Printf.eprintf "None-" | Some p -> Printf.eprintf "%s.%d.%d-" (status_to_string p.st) p.w p.h in
  Array.iter print_pic image_cache.pics;
  Printf.eprintf "}\n"

let idle_fill = ref false
exception Cache_modified of int (* should only be raised if !idle_fill = true *)

let recenter_cache ctr =
   let new_pos = max (ctr - cache_radius) 0 in
   Printf.eprintf "pos: %d new_pos: %d\n" image_cache.pos new_pos;
   print_cache ();
   
   let toss = min (abs (new_pos - image_cache.pos)) cache_size in
   let keep = cache_size - toss in
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

let pixbuf_size pix =
  let w = GdkPixbuf.get_width pix 
  and h = GdkPixbuf.get_height pix in
  (w,h)

let scale_cache idx ar pic ?tgt_image () =
  let hyper_scale (width, height) pb =
    let out_b = GdkPixbuf.create width height () in
    GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`HYPER pb;
    out_b
  in
  let cache_idx = idx - image_cache.pos in
  if cache_idx < 0 || cache_idx >= cache_size then (* cache underflow *)
    false
  else 
    begin
      let dims = scale (pic.w,pic.h) ar in
      let out = hyper_scale dims pic.pb in
      image_cache.pics.(cache_idx) <- 
	Some { st = Scaled ar; w = fst dims; h = snd dims; pb = out};
      if !idle_fill then raise (Cache_modified idx);
      match tgt_image with
	  None -> false
	| Some tgt_image -> tgt_image#set_pixbuf out; false
    end
	
let scale_factor wt ht wi hi =
  let wt = float_of_int wt and ht = float_of_int ht
  and wi = float_of_int wi and hi = float_of_int hi in
  let ar_t = wt /. ht and ar_i = wi /. hi in
  if ar_t > ar_i then ht /. hi else wt /. wi

let scaled_size wt ht wi hi =
  let ar = scale_factor wt ht wi hi in
  let w1, h1 = scale (wi, hi) ar in
  ar, w1, h1

let get_cache' cache_idx =
  match image_cache.pics.(cache_idx) with
      Some pic -> pic
    | None -> 
	let idx = (image_cache.pos+cache_idx) in
	let pix = 
          try GdkPixbuf.from_file (get_file idx)
          with GdkPixbuf.GdkPixbufError(_,msg) as exn ->
	    let d = GWindow.message_dialog ~message:msg ~message_type:`ERROR
	      ~buttons:GWindow.Buttons.close ~show:true () in
	    ignore(d#run ());
	    raise exn
	in
	let cb = 
	  { st = Full;
	    w = GdkPixbuf.get_width pix; 
	    h = GdkPixbuf.get_height pix;
	    pb = pix }
	in
	image_cache.pics.(cache_idx) <- Some cb;
	if !idle_fill then raise (Cache_modified idx);
	cb

let rec scale_cache_pre idx tgt_image () =
  let w0, h0 = widget_size ~cap:100 tgt_image in
  match get_cache idx with
    | {st=Scaled _; w=_; h=_; pb=_} -> false
    | {st=Full; w=w1; h=h1; pb=pix} as pic -> 
	let ar = scale_factor w0 h0 w1 h1 in
	scale_cache idx ar pic ()

and get_cache idx = 
(*Printf.eprintf "get_c %d\n" idx; *)
   let cache_idx = idx-image_cache.pos in
   if cache_idx < 0 || cache_idx >= cache_size then (* cache underflow *)
     (recenter_cache idx; get_cache idx)
   else
     get_cache' cache_idx

let reset_cache idx = 
  let cache_idx = idx - image_cache.pos in
  if cache_idx < 0 || cache_idx >= cache_size then ()
  else image_cache.pics.(cache_idx) <- cache_null

let image_idx = ref 0
let last_direction = ref 1

let rec set_image_from_cache idx tgt_image = 
  let nearest_scale (width, height) pb =
    let out_b = GdkPixbuf.create width height () in
    GdkPixbuf.scale ~dest:out_b ~width ~height ~interp:`NEAREST pb;
    out_b
  in
  let width, height = widget_size ~cap:100 tgt_image in
  let pix = 
    match get_cache idx with
      | {st=Full; w=w0; h=h0; pb=pix} -> 
	  (* generate simple preview *)
	  let ar, width, height = scaled_size width height w0 h0 in
	  nearest_scale (width,height) pix
      | {st=Scaled _; w=w0; h=h0; pb=pix} -> 
	  let _, width, height = scaled_size width height w0 h0 in
	  if abs(w0 - width) <= 2 && abs(h0-height) <= 2 then
	    pix
	  else ( (* need to load from disk *)
	    Printf.eprintf "Found small cached img: %dx%d, wanted %dx%d\n" w0 h0 width height;
	    reset_cache idx;
	    set_image_from_cache idx tgt_image;
	    (get_cache idx).pb
	  )
  in
  tgt_image#set_pixbuf pix
    
let is_vert idx = 
  let {st=_; w=w0; h=h0; pb=_} = get_cache idx in
  w0 < h0

let can_twopage idx =
  !opt_twopage && is_vert idx && is_vert (idx + 1)

let on_screen idx = idx = !image_idx || (idx = (!image_idx + 1) && can_twopage (!image_idx))

let rec idle_cache_fill () = 
  try 
    idle_fill := true;
    print_cache ();
    (* scale the current picture *)
    ignore (scale_cache_pre !image_idx image1 ());
    (* load all pictures into cache *)
    Array.iteri (fun i _ -> ignore (get_cache' i)) image_cache.pics;
    (* scale all pictures in cache *)
    Array.iteri (fun i _ -> ignore (scale_cache_pre (i+image_cache.pos) image1 () )) image_cache.pics;
    idle_fill := false; false
  with Cache_modified idx -> 
    if on_screen idx then show_spread ();
    idle_fill := false; true

and show_spread' () =
(*   failwith ("width=" ^ string_of_int width ^ " height=" ^ string_of_int height);*) 
  set_image_from_cache !image_idx image1;
  
  if can_twopage !image_idx then (
    image2#misc#show ();
    set_image_from_cache (!image_idx+1) image2;
  ) else (
    image2#misc#hide ();
  );
  ignore(Idle.add idle_cache_fill);
  false
    (*  window#resize ~width:(max width 260) ~height:(height + 40)*)

and show_spread () = 
  window#set_title (Printf.sprintf "Image %d of %d"
		      !image_idx (Array.length Sys.argv - 1));
  ignore(Idle.add show_spread')
    
let prev_image () =
  image_idx := !image_idx - (if can_twopage (max (!image_idx-2) 0) then 2 else 1);
  if !image_idx < 0 then
    image_idx := if !opt_wrap then max_index else 0;
  last_direction := -1;
  show_spread ()
    
let next_image () =
  image_idx := !image_idx + (if can_twopage !image_idx then 2 else 1);
  if !image_idx >= max_index then
    image_idx := if !opt_wrap then 0 else max_index;
  last_direction := 1;
  show_spread ()
    
let toggle_fullscreen () =
  if !opt_fullscreen then (
    opt_fullscreen := false;
    window#unfullscreen ();
   ) else (
    opt_fullscreen := true;
    window#fullscreen ();
   )

let toggle_twopage () =
  opt_twopage := not !opt_twopage;
  (* TODO: increase cache size if twopage *)
  show_spread ()

let toggle_manga () =
  opt_manga := not !opt_manga;
  cont#remove image1#coerce; cont#remove image2#coerce;
  if !opt_manga then (cont#add image2#coerce; cont#add image1#coerce;)
      else (cont#add image1#coerce; cont#add image2#coerce;)

let go_to_page_dialog () =
  let _ = GWindow.dialog ~parent:window ~title:"Go to page"  () in
  ()

open GdkKeysyms

let handle_key event =
  let kv = GdkEvent.Key.keyval event in
  if kv = _q then Main.quit () else
  if kv = _Left or kv = _Down or kv = _BackSpace then prev_image() else
  if kv = _Right or kv = _Up or kv = _space then next_image() else
  if kv = _f then toggle_fullscreen () else
  if kv = _t then toggle_twopage () else
  if kv = _m then toggle_manga () else
  if kv = _w then opt_wrap := not !opt_wrap
; true

let resized event =
(*  image_width := GdkEvent.Configure.width event;
  image_height := GdkEvent.Configure.height event; *)
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
