(* coml - LablGTK-based Comic-book (CBR,CBZ) viewer 
   
   (original dispimg source) Written by Shawn Wagner (shawnw@speakeasy.org) 
   and released into the public domain.

    Copyright (C) 2008 Eric Norige

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


   
   Compile with community-ocaml 
   (http://repo.or.cz/w/ocaml.git branch community/trunk)
   
   % make
   
 *)

let (|>) x f = f x

let is_directory fn = (Unix.lstat fn).Unix.st_kind = Unix.S_DIR
   
open GMain
   
type scaling = Fit_w | Fit_h | Fit_both | Zoom of float

type options = { mutable fullscreen: bool;
		 mutable twopage: bool;
		 mutable manga: bool;
		 mutable remove_failed: bool; 
		 mutable fit: scaling;
		 mutable zoom_enlarge: bool;
		 mutable wrap : bool;
	       }
(* TODO: save and load options *)
let opt = { 
  fullscreen = false; 
  twopage = true; 
  manga = true;
  remove_failed = true; 
  fit = Fit_both; 
  zoom_enlarge = false;
  wrap = false;
}


let print_scrollers adj1 adj2 =
  Printf.printf "scrollers: (%.0f-%.0f)@%.0f PS,PI: %.0f,%0.f \t(%.0f-%.0f)@%.0f PS,PI %.0f,%0.f\n" adj1#lower adj1#upper adj1#value adj1#page_size adj1#page_increment adj2#lower adj2#upper adj2#value adj2#page_size adj2#page_increment; flush stdout

(* GTK WIDGETS *)

let window_width = 900 and window_height = 700

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:window_width ~height:window_height ()

let pane = GPack.paned `VERTICAL ~packing:window#add ()

 let scroller = GBin.scrolled_window ~packing:pane#add1 ~height:660 ()
 let _ = scroller#set_hpolicy `AUTOMATIC; scroller#set_vpolicy `AUTOMATIC
(*;
   ignore(scroller#hadjustment#connect#changed (fun () -> print_string "H-changed: "; print_scrollers scroller#hadjustment scroller#vadjustment));
   ignore(scroller#hadjustment#connect#value_changed (fun () -> print_string "H-Value changed "; print_scrollers scroller#hadjustment scroller#vadjustment));
   ignore(scroller#vadjustment#connect#changed (fun () -> print_string "V-changed "; print_scrollers scroller#hadjustment scroller#vadjustment));
   ignore(scroller#vadjustment#connect#value_changed (fun () -> print_string "V-Value changed "; print_scrollers scroller#hadjustment scroller#vadjustment)) *)
 let image = GMisc.image ~packing:scroller#add_with_viewport ()
   
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


let preload_prio = 130 and show_prio = 115 
and scale_prio = 140 and gen_page_prio = 150 
and spread_gc_prio = 145

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

let find_zoom t_size p_size fit =
  let zoom = 
    match fit with 
	Fit_w -> fit_wid t_size p_size 
      | Fit_h -> fit_hei t_size p_size 
      | Fit_both -> fit_both t_size p_size 
      | Zoom z -> z in
  if opt.zoom_enlarge then zoom else min zoom 1.0

let scale_ar t_size ?(fit=opt.fit) ?(interp=`NEAREST) pixbuf =
  let orig = pixbuf_size pixbuf in
  let ar_size = mul_size orig (find_zoom t_size orig fit) in
  scale_raw ar_size ~interp pixbuf

let size_fits (w1,h1) (w2,h2) = w1-w2 <= 2 && w1-w2 >= -10 && h1-h2 <= 2 && h1-h2>= -10
  
let numeric_compare s1 s2 = 
  let s1 = String.lowercase s1 and s2 = String.lowercase s2 in
  let l1 = String.length s1 and l2 = String.length s2 in
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

type ifs = {idx : int; filename: string; size: int * int}

type spread = One of ifs | Two of ifs * ifs

type node = { mutable next : node; 
	      mutable prev: node;
	      book : book;
	      mutable pixbuf : GdkPixbuf.pixbuf option;
	      mutable scaler : Glib.Idle.id option;
	      mutable pics : spread; 
	    }
and book = { title: string;
	     mutable first_page : node; 
	     mutable last_page : node; 
	     (* all files not yet made into nodes *)
	     mutable more_files : string list; 
	     page_cache : GdkPixbuf.pixbuf Weak.t;
	   }

let get_page cache idx fn =
  match Weak.get cache idx with 
      Some pb -> pb 
    | None -> let pb = GdkPixbuf.from_file fn in
      Weak.set cache idx (Some pb);
      pb
let get_page_ifs c ifs = get_page c ifs.idx ifs.filename    

let get_idxes s = match s.pics with One i -> [i.idx] | Two (i1,i2) -> [i1.idx;i2.idx]
let first_idx s = match s.pics with One i -> i.idx | Two (i1,i2) -> min i1.idx i2.idx
let last_idx s = match s.pics with One i -> i.idx | Two (i1,i2) -> max i1.idx i2.idx
let get_pos s = first_idx s
let get_size s = match s.pics with One i -> i.size | Two(i1,i2) -> 
  let merge_size (w1,h1) (w2,h2) = (w1 + w2, max h1 h2) in merge_size i1.size i2.size
let is_vert ifs = let w0,h0 = ifs.size in w0 < h0
let gen_ifs i f = {idx=i;filename=f;size=pixbuf_size (GdkPixbuf.from_file f)}

let make_book lib title files = 
  let files = files |> Array.of_list in
  let len = Array.length files in
  Array.sort numeric_compare files; (* TODO: schwarzian transform - sort properly *)
  let files = files |> Array.to_list in
  match files with [] -> lib
    | p0 :: rest -> 
	let rec n = {next = n; prev = n; book = b; pixbuf = None;
		     scaler = None; pics = One(gen_ifs 0 p0);}
	and b = {title=title; more_files=rest; 
		 first_page=n; last_page=n; 
		 page_cache = Weak.create len} in
	Printf.eprintf "Making book %s with %d files\n" title len; flush stderr;
	match lib with
	    None -> 
	      Some (n,n)
	  | Some (n0,nx) ->
	      n.prev <- nx; 
	      nx.next <- n;
	      if opt.wrap then (n.next <- n0; n0.prev <- n );
	      Some (n0,n)

let cur_node = ref (Obj.magic 0) (* gets set to a real value before window gets shown *)
let idxes_on_disp () = get_idxes !cur_node
let max_index ?(cn= !cur_node) () = last_idx cn.book.last_page
let within_book_range ?(cn= !cur_node) x = x >= 0 && x <= max_index ~cn ()

let near_cur_node n = abs (last_idx !cur_node - first_idx n) < 4

let archive_suffixes = [("rar", `Rar); ("cbr", `Rar); ("zip", `Zip); ("cbz", `Zip); ("7z", `Sev_zip)]
let pic_suffixes = [("jpg", `Jpeg); ("jpeg", `Jpeg); ("JPG", `Jpeg); ("gif", `Gif); ("png", `Png);(*any others?*) ]

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

let rec rec_del fn path = 
  Printf.eprintf "Cleaning up %s (from archive %s)\n" path fn;
  let remove fn = (*Printf.eprintf "Removing: %s"*) Unix.unlink fn in
  Sys.readdir path
    |> Array.map (fun fn -> Filename.concat path fn)
    |> Array.iter (fun fn -> if is_directory fn
		   then rec_del "" fn 
		   else remove fn);
    Unix.rmdir path

let build_books l = 
  let files_in path = Sys.readdir path |> Array.to_list |> List.map (Filename.concat path) in
  let ht = Hashtbl.create 50 in (* title -> filename  -- multiple bindings *)
  let titles = ref [] in (* title -> unit -- single-binding per title *)
  let add_entries t fs = List.iter (Hashtbl.add ht t) fs; if not (List.mem t !titles) then titles := !titles @ [t] in
  let rec expand_list = function
    | h when not (Sys.file_exists h) -> ()
    | h when is_directory h ->
	let hlen = String.length h in
	let h = if h.[hlen-1] = '/' then String.sub h 0 (hlen-1) else h in
	let title = Filename.basename h in
	files_in h |> List.filter is_picture |> add_entries title;
    | h when is_archive h ->
      let td = Filename.concat Filename.temp_dir_name (Printf.sprintf "coml-%d" (Random.bits ())) in
      (* extract archive to td *)
      let return_code = extract_archive h td in
      (* on quit, remove the extracted archive *)
      at_exit (fun () -> rec_del h td);
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
	files |> List.filter is_picture |> add_entries title;
	List.iter expand_list dirs;
    | h when is_picture h ->
	add_entries (Filename.basename h) [h];
    | _ -> (* garbage file *) ()
  in
  List.iter expand_list l; (* stores results in hashtables ht & titles *)

  let add_book l t = make_book l t (Hashtbl.find_all ht t) in
  List.fold_left add_book None !titles
;;
 
let fresh_pixbuf s =
  let get_page i = get_page_ifs s.book.page_cache i in
  match s.pics with
      One i -> get_page i
    | Two (i1,i2) -> 
	let ow,oh = get_size s in
	let out_buf = GdkPixbuf.create ~width:ow ~height:oh ~has_alpha:true () in
	GdkPixbuf.fill out_buf (0x00000000l);
  
	let rec copier dest_x = function [] -> () | pb::rest ->
	  let (width, height) = pixbuf_size pb in
	  let dest_y = (oh - height) / 2 in
	  GdkPixbuf.copy_area ~dest:out_buf ~dest_x ~dest_y ~width ~height pb;
	  copier (dest_x + width) rest
	in
	let pics = [get_page i1; get_page i2] in
	let pics = if opt.manga then List.rev pics else pics in
	copier 0 pics;
	out_buf
    
let singleton_task prio f = 
  let tid = ref None in
  let task () = tid := None; f (); false in
  fun () -> if !tid = None then tid := Some (Idle.add ~prio:prio task)
    
let cleanup_task l = singleton_task spread_gc_prio
  (fun () -> 
     let l',rem = List.partition near_cur_node !l in
     List.iter (fun n -> n.pixbuf <- None) rem; l := l'
  )
  
let gen_ifs_cached c i f = 
  let pb = get_page c i f in
  {idx=i; filename=f; size=pixbuf_size pb}

let add_node_after n ifsl = 
  let n1 = {next = n.next; prev=n; book=n.book; 
	    pixbuf = None; scaler = None; 
	    pics = ifsl;} in
  n.next <- n1; n1.next.prev <- n1

let gen_pages b () = 
  let page ifsl = 
    add_node_after b.last_page ifsl; 
    b.last_page <- b.last_page.next; 
    window#set_title (Printf.sprintf "Image_idx %s of %d, Book %s" (get_idxes !cur_node |> String.of_list string_of_int) (max_index ~cn:!cur_node ()) !cur_node.book.title);
    true 
  and gen_ifs n fn = gen_ifs_cached b.page_cache n fn in
  match b.more_files with [] -> false
    | f1 :: rest -> 
	b.more_files <- rest;
	let pos_n = (last_idx b.last_page + 1) in
	let ifs1 = gen_ifs pos_n f1 in
	if opt.twopage && is_vert ifs1 then
	  match b.more_files with 
	      [] -> page (One ifs1)
	    | f2 :: rest -> 
		let ifs2 = gen_ifs (pos_n+1) f2 in
		if is_vert ifs2 
		then (b.more_files <- rest; page (Two (ifs1,ifs2)))
		else page (One ifs1)
	else 
	  page (One ifs1)

let set_pb = 
  let l = ref [] in
  fun n pb -> n.pixbuf <- Some pb; l := n :: !l; if List.length !l > 15 then cleanup_task l ()

let set_node_pixbuf size n interp = 
  let zoom = find_zoom size (get_size n) opt.fit in
  let ar_size = mul_size (get_size n) zoom in
  let display pic = 
    if n == !cur_node then begin
      image#set_pixbuf pic;
      window#set_title (Printf.sprintf "Image_idx %s of %d, Book %s @ %2.2f%%" (get_idxes n |> String.of_list string_of_int) (max_index ~cn:n ()) n.book.title (zoom *. 100.));
      (* draw the screen *)
      ignore (Glib.Main.iteration true);
      let hadj = scroller#hadjustment 
      and vadj = scroller#vadjustment in
(*      print_scrollers hadj vadj;*)
      let hmax = hadj#upper -. hadj#page_size
      and vmax = vadj#upper -. vadj#page_size in
      if hadj#value < hmax || vadj#value < vmax then begin
	vadj#set_value vadj#lower;
	hadj#set_value (if opt.manga then hmax else hadj#lower);
      end else begin
	vadj#set_value vadj#lower;
	hadj#set_value hadj#lower;
      end;
(*      print_scrollers hadj vadj;
*)
    end
  in
  match n.pixbuf with 
      Some pb when size_fits (pixbuf_size pb) ar_size -> display pb
    | None | Some _ -> 
	let pb = (scale_raw ar_size ~interp (fresh_pixbuf n)) in
	set_pb n pb;
	display pb

let idle_scale node =
  let scaler () =
    node.scaler <- None;
    (* don't bother scaling if far away from current position *)
    if near_cur_node node then
      set_node_pixbuf (view_size ()) node `HYPER;
    false
  in
  if node.scaler = None then
     node.scaler <- Some (Idle.add ~prio:scale_prio scaler)

 let preload_spread = singleton_task preload_prio 
   (fun () -> idle_scale !cur_node.next)

 let show_spread () = 
   window#set_title (Printf.sprintf "Image_idx %s of %d, Book %s" (get_idxes !cur_node |> String.of_list string_of_int) (max_index ~cn:!cur_node ()) !cur_node.book.title);
   singleton_task show_prio 
   (fun () -> 
      (* queue the good rescale *)
      idle_scale !cur_node;
      (* do quick rescale - takes care of updating the display *)
      set_node_pixbuf (view_size()) !cur_node `TILES;
      preload_spread (); 
   ) ()

(* PAGE CHANGING FUNCIONS *)
let next_page () = !cur_node.next
let prev_page () = !cur_node.prev
let first_page () = !cur_node.book.first_page
let last_page () = !cur_node.book.last_page
  
let new_page get_page () = 
  cur_node := get_page (); show_spread ()

let magic_next () = 
  let hadj = scroller#hadjustment and vadj = scroller#vadjustment in
  let vupper = vadj#upper -. vadj#page_size
  and hupper = hadj#upper -. hadj#page_size in
(*  print_scrollers hadj vadj; *)
  if vadj#value < vupper -. 1. then begin
(*    Printf.printf "V-step down %.1f < %.1f - 1\n"vadj#value vupper;*)
    vadj#set_value (min (vadj#value +. vadj#page_increment) vupper);
(*    vadj#emit changed TODO: HOW?*)
  end else if (not opt.manga) && hadj#value < hupper -. 1. then begin
(*    Printf.printf "H-step right %.1f < %.1f - 1\n" hadj#value hupper;*)
    if vadj#value != 0. then vadj#set_value 0.;
    hadj#set_value (min (hadj#value +. hadj#page_increment) hupper);
  end else if opt.manga && hadj#value > 1. then begin
(*    Printf.printf "H-step left %.1f > 1\n" hadj#value;*)
    if vadj#value != 0. then vadj#set_value 0.;
    hadj#set_value (max (hadj#value -. hadj#page_increment) 0.);
  end else
    new_page next_page ()
    
let find_page i () = 
  assert (within_book_range i);
  let not_pg_i n = not (List.mem i (get_idxes n)) in
  (* search in correct direction *)
  let next = if get_pos !cur_node < i then next_page else prev_page in
  while not_pg_i !cur_node do cur_node := next() done;
  show_spread ()


(* GROUPING IFSes INTO NODES *)
let dump_pages b =
  let n = ref b.first_page in
  let act n = match n.pics with One i -> Printf.eprintf "(%d)" i.idx | Two (i1,i2) -> Printf.eprintf "[%d,%d]" i1.idx i2.idx in
  while !n != b.last_page do act !n; n := !n.next done; act !n;
  Printf.eprintf "\n"

let del_node n = n.next.prev <- n.prev; n.prev.next <- n.next

let merge n = 
  match n.pics , n.next.pics with 
    | One i1, _ when not (is_vert i1) -> ()
    | One i1, One i2 when is_vert i2-> 
	n.pics <- Two (i1,i2); 
	if !cur_node == n.next then cur_node := n;
	del_node n.next; n.pixbuf <- None; 
    | One i1, Two (i2, i3) -> 
	n.pics <- Two (i1,i2); n.next.pics <- One i3; 
	n.pixbuf <- None; n.next.pixbuf <- None
    | _, One i2 -> ()
    | Two _, _ -> ()
and split n = 
  match n.pics with
      | Two (i1,i2) -> 
	  n.pics <- One i1; n.pixbuf <- None; 
	  add_node_after n (One i2);
      | One _ -> () 

let regroup_pages n0 () = 
  let n = ref n0 in
  let act = if opt.twopage then merge else split in
  while get_pos !n <= get_pos !n.next do act !n; n := !n.next done; act !n

let toggle_twopage () =
  opt.twopage <- not opt.twopage;
  regroup_pages !cur_node.book.first_page ();
  show_spread ()

let toggle_single_page n =
  match n.pics with
    | One _ -> merge n; regroup_pages n.next ()
    | Two _ -> split n; regroup_pages n.next ()

let toggle_cur_twopage () = toggle_single_page !cur_node

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
  let on_ok () = find_page sb#value_as_int (); w#destroy () in
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



(* gets set to first and last books when they're built *)
let first_last = ref None
let toggle_wrap () = 
  opt.wrap <- not opt.wrap;
  match !first_last with 
      None -> Printf.eprintf "Cannot change wrap\n";
    | Some (b0,bx) -> 
	b0.first_page.prev <- if opt.wrap then bx.last_page else b0.first_page;
	bx.last_page.next <- if opt.wrap then b0.first_page else bx.last_page
	  
(* key bindings as a list of pairs  (key, function) *)  
open GdkKeysyms

let actions = [(_q, Main.quit);
	    (_Left, new_page prev_page); 
	    (_Up, new_page prev_page); (_BackSpace, new_page prev_page);
	    (_Right, new_page next_page); (_Down, new_page next_page); 
	    (_space, magic_next);
	    (_f, toggle_fullscreen); (_F11, toggle_fullscreen);
	    (_Escape, exit_fullscreen); (_t, toggle_twopage);
	    (_m, toggle_manga); (_z, toggle_zoom);
	    (_minus, zoom_out); (_plus, zoom_in); (_equal, zoom_in);
(*	    (_l, (fun () -> load_cache (List.fold_left min 0 !image_idxes)));*)
	    (_w, toggle_wrap); (_1, toggle_cur_twopage);
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

let mouse_motion event =
  if Gdk.Convert.test_modifier `BUTTON1 (GdkEvent.Motion.state event) then
    (* in process of dragging *)
    let _ = GdkEvent.Motion.x event and _ = GdkEvent.Motion.y event in
    ()
  
    
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
  
  begin match build_books arg_list with
      None -> 
	Printf.printf "No books found\nUsage: %s [IMAGEFILE|IMAGEDIR|IMAGEARCHIVE] ...\n" Sys.argv.(0);
	exit 1
    | Some (n0,nx) -> 
	new_page (fun () -> n0) ();
	first_last := Some (n0.book,nx.book);
	let rec load_book_pages n i = 
	  ignore(Idle.add ~prio:(gen_page_prio+i) (gen_pages n.book));
	  if n != nx then load_book_pages n.next (succ i)
	in
	load_book_pages n0 0
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
  
  window#show ();
  Main.main () (* calls the GTK main event loop *)
    
let _ = main ()
