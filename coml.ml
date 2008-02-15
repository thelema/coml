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
   
type scaling = Fit_w | Fit_h | Fit_both | Zoom of float

type options = { mutable fullscreen: bool;
		 mutable twopage: bool;
		 mutable manga: bool;
		 mutable remove_failed: bool; 
		 mutable fit: scaling;
		 mutable zoom_enlarge: bool;
	       }
(* TODO: save and load options *)
let opt = { fullscreen = false; 
	    twopage = true; manga = true;
	    remove_failed = true; fit = Fit_both; zoom_enlarge = false;
	  }


(* GTK WIDGETS *)

let window_width = 900 and window_height = 700

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:window_width ~height:window_height ()

let pane = GPack.paned `VERTICAL ~packing:window#add ()

 let scroller = GBin.scrolled_window ~packing:pane#add1 ~height:660 ()
 let _ = scroller#set_hpolicy `AUTOMATIC; scroller#set_vpolicy `AUTOMATIC
 let image1 = GMisc.image ~packing:scroller#add_with_viewport ()

 let footer = GPack.hbox ~packing:pane#pack2 ()
  let file = GMisc.label ~packing:(footer#pack ~expand:true) ()
  let _ = GMisc.separator `VERTICAL ~packing:footer#pack ()
  let note = GMisc.label ~packing:(footer#pack ~expand:true) ()
  let _ = GMisc.separator `VERTICAL ~packing:footer#pack ()
  let bbox = GPack.button_box `HORIZONTAL ~packing:footer#pack ~layout:`END ()
(*let _ = let newsty = spread#misc#style#copy in newsty#set_bg [`NORMAL,`BLACK]; spread#misc#set_style newsty (* set the background black *)*)

(* GUI utility functions *)
let set_status str = Printf.eprintf "%.2f: " (Sys.time()); prerr_endline str; note#set_label str
let view_size () = let {Gtk.width=width; height=height} = scroller#misc#allocation in (width-2,height-2)


let preload_prio = 150 and show_prio = 115 and scale_prio = 200 and gen_page_prio = 210

let failed_load = GdkPixbuf.create 1 1 ()

(* PIXBUF RESIZING FUNCTIONS *)

let pixbuf_size pix = GdkPixbuf.get_width pix, GdkPixbuf.get_height pix

let fit_wid (wt,_) (wi,_) = float wt /. float wi
let fit_hei (_, ht) (_, hi) = float ht /. float hi
let fit_both st si = min (fit_wid st si) (fit_hei st si)

let scale_raw (width,height) ~interp pixbuf =
  let out_b = GdkPixbuf.create ~width ~height ~has_alpha:true () in
  GdkPixbuf.scale ~dest:out_b ~width ~height ~interp pixbuf;
  out_b

let mul_size (wi,hi) zoom = int_of_float (float wi *. zoom), int_of_float (float hi *. zoom)

let ar_sizer t_size p_size fit =
  let zoom = 
    match fit with 
	Fit_w -> fit_wid t_size p_size 
      | Fit_h -> fit_hei t_size p_size 
      | Fit_both -> fit_both t_size p_size 
      | Zoom z -> z in
  let zoom = if opt.zoom_enlarge then zoom else min zoom 1.0 in
  mul_size p_size zoom

let scale_ar t_size ?(fit=opt.fit) ?(interp=`NEAREST) pixbuf =
  let ar_size = ar_sizer t_size (pixbuf_size pixbuf) fit in
  scale_raw ar_size ~interp pixbuf


(* SPREAD TYPE AND MANIPULATION OF SUCH *)    
    
  
let size_fits (w1,h1) (w2,h2) = w1-w2 <= 2 && w1-w2 >= -10 && h1-h2 <= 2 && h1-h2>= -10
  
let pics_of_idxes get_cache idxes = idxes
	      |> List.map get_cache 
	      |> List.filter (fun p -> p != failed_load) 
		  
let is_vert pic = let (w0,h0) = pixbuf_size pic in w0 < h0
						     
let numeric_compare s1 s2 = 
  let l1 = String.length s1 and l2 = String.length s2 in
  let s1 = String.lowercase s1 and s2 = String.lowercase s2 in
  let rec pos_diff i = 
    if i = l1 then -2 else if i = l2 then -1
    else if s1.[i] = s2.[i] then pos_diff (i+1) else i
  and num_end i s =
    try 
      if s.[i] >= '0' && s.[i] <= '9' then num_end (i+1) s else i 
    with _ -> i-1
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

type node = { mutable next : node; 
	      mutable prev: node;
	      book : book;
	      mutable pixbuf : GdkPixbuf.pixbuf option;
	      mutable scaler : Glib.Idle.id option;
	      orig_size : int * int;
	      idxes: int list;
	      files : string list; 
	    }
and book = { title: string;
	     mutable first_page : node; 
	     mutable last_page : node; 
	     (* all files not yet made into nodes *)
	     mutable more_files : string list; 
	     page_cache : GdkPixbuf.pixbuf Weak.t;
	   }

let get_page b idx fn =
  match Weak.get b.page_cache idx with 
      Some pb -> pb 
    | None -> let pb = GdkPixbuf.from_file fn in
      Weak.set b.page_cache idx (Some pb);
      pb
    
let get_idxes s = s.idxes
let first_idx s = s.idxes |> List.fold_left min max_int
let last_idx s = s.idxes |> List.fold_left max min_int
let get_pos s = first_idx s

let fits_size_pb size orig pb = 
  let ar_size = ar_sizer size orig opt.fit in
  size_fits (pixbuf_size pb) ar_size

let make_size_match size node maker = 
  match node.pixbuf with 
      Some pb when fits_size_pb size node.orig_size pb -> pb
    | None | Some _ -> maker size node

let fresh_pixbuf s =
  let ow,oh = s.orig_size in
  let out_buf = GdkPixbuf.create ~width:ow ~height:oh ~has_alpha:true () in
  GdkPixbuf.fill out_buf (0x00000000l);
  
  let rec copier dest_x = function [] -> () | pb::rest ->
    let (w, h) = pixbuf_size pb in
    let dest_y = (oh - h) / 2 in
    GdkPixbuf.copy_area ~dest:out_buf ~dest_x ~dest_y ~width:w ~height:h pb;
    copier (dest_x + w) rest
  in
  let pics = List.map2 (get_page s.book) s.idxes s.files in
  let pics = if opt.manga then List.rev pics else pics in
  copier 0 pics;
  out_buf
    
let best_scale size node = 
  Printf.eprintf "Resizing img (%s)\n" (string_of_int_list "" node.idxes);
  scale_ar size ~interp:`HYPER (fresh_pixbuf node)

let scale ?post_scale size node =
  let scale_idle () =
    node.scaler <- None; 
    (*      let pos = get_pos spread in
	    if !disp_pos >= pos && !disp_pos < pos + 4 then begin (* don't bother scaling if far away from current position *)*)
    
    let pb = make_size_match size node best_scale in
    node.pixbuf <- Some pb;
    (match post_scale with None -> () | Some f -> f pb node);
    false
  in
  (match node.scaler with 
       None -> ()
     | Some tid -> Idle.remove tid);
  node.scaler <- 
    Some (Idle.add ~prio:scale_prio scale_idle)

let gen_page b () = 
  let page1 f p = 
    let pos_n = (last_idx b.last_page + 1) in
    let n1 = {next = b.last_page.next; prev=b.last_page; book=b; 
	      pixbuf = None; scaler = None; 
	     idxes = [pos_n]; files=[f]; orig_size = pixbuf_size p;} in
    b.last_page.next <- n1; b.last_page <- n1;
    true
  and page2 fl p1 p2 =
    let pos_n = (last_idx b.last_page + 1) in
    let w1,h1 = pixbuf_size p1 
    and w2,h2 = pixbuf_size p2 in
    let n1 = {next = b.last_page.next; prev=b.last_page; book=b; 
	      pixbuf = None; scaler = None; idxes = [pos_n; pos_n+1];
	     files = fl; orig_size = (w1+w2,max h1 h2);} in
    b.last_page.next <- n1; b.last_page <- n1;
    true
  in
  match b.more_files with [] -> false
    | f1 :: rest -> 
	b.more_files <- rest;
	let p1 = GdkPixbuf.from_file f1 in
	if opt.twopage && is_vert p1 then
	  match b.more_files with 
	      [] -> page1 f1 p1
	    | f2 :: rest -> 
		let p2 = GdkPixbuf.from_file f2 in
		if is_vert p2 
		then (b.more_files <- rest; page2 [f1; f2] p1 p2)
		else page1 f1 p1
	else 
	  page1 f1 p1

let make_book title files = 
  let files = files |> Array.of_list in
  let len = Array.length files in
  Array.sort numeric_compare files; (* TODO: schwarzian transform *)
  let files = files |> Array.to_list in
  match files with [] -> None 
    | p0 :: rest -> 
	let rec n = {next = n; prev = n; book = b; pixbuf = None;
		     scaler = None; idxes = [0]; files = [p0]; 
		     orig_size = pixbuf_size (GdkPixbuf.from_file p0);}
	and b = {title=title; more_files=rest; first_page=n; last_page=n; 
		page_cache = Weak.create len} in
	Some b


let books = ref [| |]
let cur_node = ref (Obj.magic 0)
let idxes_on_disp () = !cur_node.idxes
let max_index ?(cn= !cur_node) () = last_idx cn.book.last_page
let within_book_range ?(cn= !cur_node) x = x >= 0 && x <= max_index ~cn ()

let near_cur_node n = abs (last_idx !cur_node - first_idx n) < 4

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
  Printf.eprintf "Extracting %s to %s\n" file dir; flush stderr;
  match archive_type file with
    | `Rar -> Sys.command (Printf.sprintf "unrar x \"%s\" \"%s\"/" file dir)
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
	let hlen = String.length h in
	let h = if h.[hlen-1] = '/' then String.sub h 0 (hlen-1) else h in
	let title = Filename.basename h in
	let book = files_in h |> List.map (fun fn -> title,fn) in
	expand_list (List.rev_append book acc) t
    | h :: t when is_archive h ->
      let td = Filename.concat Filename.temp_dir_name (Printf.sprintf "coml-%d" (Random.bits ())) in
      (* extract archive to td *)
      let return_code = extract_archive h td in
      (* on quit, remove the extracted archive *)
      at_exit (fun () -> rec_del td);
      if return_code = 0 && Sys.file_exists td then
	let contents = files_in td in
	let dirs,files = List.partition is_directory contents
	and title = Filename.basename h in
(*Printf.eprintf "Title:%s\nFiles:\n" title;
List.iter (Printf.eprintf "%s\n") files;
Printf.eprintf "Dirs:\n";
List.iter (Printf.eprintf "%s\n") dirs;
Printf.eprintf "Contents:\n";
List.iter (Printf.eprintf "%s\n") contents; flush stderr; *)
	let acc' = files |> List.map (fun fn -> title,fn) |> List.append acc
	and t' = List.rev_append dirs t in
	  expand_list acc' t'
      else 
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
	let acc' = 
	  match make_book n1 files with
	      None -> acc
	    | Some b -> b :: acc in
	group_books acc' rest
  in
  books := l |> expand_list [] |> group_books [] |> Array.of_list;
(* link the books together *)
  let last = Array.length !books - 1 in
  for i = 1 to last do
    !books.(i-1).last_page.next <- !books.(i).first_page;
    !books.(i).first_page.prev <- !books.(i-1).last_page;
  done;
  !books.(last).last_page.next <- !books.(0).first_page;
  !books.(0).first_page.prev <- !books.(last).last_page
;;
 

      
let display pic n = 
  image1#set_pixbuf pic;
  window#set_title (Printf.sprintf "Image_idx %s of %d, Book %s" (n.idxes |> string_of_int_list "") (max_index ~cn:n ()) n.book.title);
  (* draw the screen *)
  ignore (Glib.Main.iteration true)


let quick_scale size node = 
  (* queue the good rescale *)
  scale ~post_scale:display (view_size ()) !cur_node;  
  scale_ar size (fresh_pixbuf node)

let unit_task prio f = 
  let tid = ref None in
  let task () = tid := None; f (); false in
  fun () -> if !tid = None then tid := Some (Idle.add ~prio:prio task)

let preload_spread = unit_task preload_prio 
  (fun () -> scale (view_size ()) !cur_node.next)

let show_spread = unit_task show_prio 
  (fun () -> 
     let size = view_size () in
     let pic = make_size_match size !cur_node quick_scale in
     display pic !cur_node;
     preload_spread (); )


let next_page () = cur_node := !cur_node.next
let prev_page () = cur_node := !cur_node.prev
let first_page () = cur_node := !cur_node.book.first_page
let last_page () = cur_node := !cur_node.book.last_page
let find_page i () = 
  assert (within_book_range i);
  let p0 = get_pos !cur_node in
  let not_pg_i n = not (List.mem i n.idxes) in
  if p0 < i then 
    while not_pg_i !cur_node do cur_node := !cur_node.next; done
  else 
    while not_pg_i !cur_node do cur_node := !cur_node.prev; done
  

let new_page set_page () = 
  set_page (); show_spread ()
    
(*
let toggle_twopage () =
  opt.twopage <- not opt.twopage;
  clear_spread_cache ();
  show_spread ()
*)

let enter_fullscreen () = 
  opt.fullscreen <- true;
  footer#misc#hide ();
  window#fullscreen ();
  show_spread ()

let exit_fullscreen () =
  opt.fullscreen <- false;
  footer#misc#show ();
  window#unfullscreen ();
  show_spread ()

let toggle_fullscreen () =
  if opt.fullscreen then exit_fullscreen () else enter_fullscreen ()

let toggle_manga () =
  opt.manga <- not opt.manga;
  show_spread()

let go_to_page_dialog () =
  let w = GWindow.dialog ~parent:window ~title:"Go to page" ~modal:true ~position:`CENTER () in
  ignore (GMisc.label ~text:"Page: " ~packing:w#vbox#add ());
  let sb = GEdit.spin_button ~packing:w#vbox#add ~digits:0 ~numeric:true () in
  sb#adjustment#set_bounds ~lower:0. ~upper:(float (max_index ())) ~step_incr:1. (); sb#set_value (float (get_pos !cur_node)); (*sb#set_activates_default true;*)

  let entry = new GEdit.entry (GtkEdit.Entry.cast sb#as_widget) in
  entry#set_activates_default true;

  w#add_button_stock `OK `OK;
  w#add_button_stock `CANCEL `CANCEL;
  w#set_default_response `OK;
  let on_ok () = new_page (find_page sb#value_as_int) (); w#destroy () in
  match w#run () with
    | `DELETE_EVENT | `CANCEL -> w#destroy ()
    | `OK -> on_ok ()

let zoom ar_val ar_func = 
  opt.fit <-( match opt.fit with
		    Fit_both | Fit_w | Fit_h -> Zoom ar_val
		  | Zoom ar -> ar_func ar);
  show_spread()

let zoom_out () = zoom 0.95 (fun ar -> Zoom (ar *. 0.95))
and zoom_in () = zoom (1.0 /. 0.95) (fun ar -> Zoom (ar /. 0.95))
and toggle_zoom () = zoom 1.0 (fun _ -> Fit_both)


(* key bindings as a list of pairs  (key, function) *)  
open GdkKeysyms

let actions = [(_q, Main.quit);
	    (_Left, new_page prev_page); 
	    (_Up, new_page prev_page); (_BackSpace, new_page prev_page);
	    (_Right, new_page next_page); (_Down, new_page next_page); 
	    (_space, new_page next_page);
	    (_f, toggle_fullscreen); (_F11, toggle_fullscreen);
	    (_Escape, exit_fullscreen); (*(_t, toggle_twopage);*)
	    (_m, toggle_manga); (_z, toggle_zoom);
	    (_minus, zoom_out); (_plus, zoom_in);
(*	    (_l, (fun () -> load_cache (List.fold_left min 0 !image_idxes)));*)
(*	    (_w, (fun () -> opt.wrap <- not opt.wrap));*)
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
  Printf.eprintf "Books built %d\n" (Array.length !books); flush stderr;
  if Array.length !books = 0 then begin
    Printf.printf "No books found\nUsage: %s [IMAGEFILE|IMAGEDIR|IMAGEARCHIVE] ...\n" Sys.argv.(0);
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
  
  new_page (fun () -> cur_node := !books.(0).first_page) ();
  window#show ();
  Array.iteri (fun i b -> ignore(Idle.add ~prio:(gen_page_prio+i) (gen_page b))) !books;
  Main.main () (* calls the GTK main event loop *)
    
let _ = main ()
