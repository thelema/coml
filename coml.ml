(* coml - LablGTK-based Comic-book (CBR,CBZ) viewer 
   
   (original dispimg source) Written by Shawn Wagner (shawnw@speakeasy.org) 
   and released into the public domain.

    Copyright (C) 2008-2010 Eric Norige

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


   % make
   
 *)
open Batteries (* requires batteries *)
open Printf
open GMain

let _ = GtkMain.Main.init ()

(* Utility functions *)
let pretty_print string_of l =
  "["^String.concat "; " (List.map string_of l)^"]"
let string_of_int_list = List.sprint Int.print 


(* config file parameters *)
let home = Sys.getenv "HOME"
let config_file = Filename.concat home ".coml"

(* generate and print usage error message *)
let usage str = 
  Printf.printf "%s\n\
Usage: %s [IMAGEFILE|IMAGEDIR|IMAGEARCHIVE] ...\n" str Sys.argv.(0);
  exit 1

(* how to scale the picture - Width, Height, both (fit in box), or a
   specific zoom factor *)

type scaling = Fit_w | Fit_h | Fit_both | Zoom of float

(* options record type *)
type options = { mutable fullscreen: bool;
		 mutable twopage: bool;
		 mutable manga: bool;
		 mutable remove_failed: bool; 
		 mutable fit: scaling;
		 mutable zoom_enlarge: bool;
		 mutable wrap : bool;
		 mutable debug : bool;
	       }

(* try to read options from file (using sexplib), and if anything
   fails, use defaults *)
let opt = 
  try
    File.with_file_in config_file (fun oh -> Marshal.input oh)
  with _ -> 
    { 
      fullscreen = false; twopage = true; manga = true;
      remove_failed = true; fit = Fit_both; zoom_enlarge = false;
      wrap = false; debug = false;
    }

(* write option variable back to file *)
let save_opts () = 
  File.with_file_out ~mode:[`create;`trunc] config_file (fun oh -> Marshal.output oh opt)

(* GTK WIDGETS *)

let window_width = 900 and window_height = 700

let window = GWindow.window ~allow_shrink:true ~allow_grow:true ~resizable:true ~width:window_width ~height:window_height ()

let pane = GPack.paned `VERTICAL ~packing:window#add ()

let scroller = GBin.scrolled_window ~packing:pane#add1 ~height:660 ()
let _ = scroller#set_hpolicy `AUTOMATIC; scroller#set_vpolicy `AUTOMATIC
let image = GMisc.image ~packing:scroller#add_with_viewport ()
  
let footer = GPack.hbox ~packing:pane#pack2 ()
let file = GMisc.label ~packing:(footer#pack ~expand:true) ()
let _ = GMisc.separator `VERTICAL ~packing:footer#pack ()
let note = GMisc.label ~packing:(footer#pack ~expand:true) ()
let _ = GMisc.separator `VERTICAL ~packing:footer#pack ()
let bbox = GPack.button_box `HORIZONTAL ~packing:footer#pack ~layout:`END ()
(*let _ = let newsty = spread#misc#style#copy in newsty#set_bg [`NORMAL,`BLACK]; spread#misc#set_style newsty (* set the background black *)*)

(* GUI utility functions *)
let set_status str = 
  if opt.debug then Printf.eprintf "%.2f: %s\n" (Sys.time()) str; 
  note#set_label str
let view_size () = 
  let {Gtk.width=width; height=height} = scroller#misc#allocation in 
  (width-2,height-2) (* drop a pixel on each edge *)

(* Priorities for scheduling non-immediate tasks *)
let preload_prio = 130 and show_prio = 115 
and scale_prio = 140 and gen_page_prio = 120
and spread_gc_prio = 145 and spread_clean_prio = 100

let failed_load = GdkPixbuf.create 1 1 ()

(* PIXBUF RESIZING FUNCTIONS *)

let pixbuf_size pix = GdkPixbuf.get_width pix, GdkPixbuf.get_height pix

(* calculate scaling factor (= zoom) for various scaling strategies *)
let fit_wid (wt,_) (wi,_) = float wt /. float wi
let fit_hei (_, ht) (_, hi) = float ht /. float hi
let fit_both st si = min (fit_wid st si) (fit_hei st si)

(* given a scaling factor (= zoom) and a size, calculate the scaled
   size *)
let mul_size (wi,hi) zoom = int_of_float (float wi *. zoom), int_of_float (float hi *. zoom)


(* actually scale a pixbuf to a target size using a specific
   interpolation method *)
let scale_raw (width,height) ~interp pixbuf =
  let out_b = GdkPixbuf.create ~width ~height ~has_alpha:true () in
  GdkPixbuf.scale ~dest:out_b ~width ~height ~interp pixbuf;
  out_b


(* returns the proper zoom factor for a given picture size, window
   size and zoom strategy *)
let find_zoom t_size p_size fit =
  let zoom = 
    match fit with 
	Fit_w -> fit_wid t_size p_size 
      | Fit_h -> fit_hei t_size p_size 
      | Fit_both -> fit_both t_size p_size 
      | Zoom z -> z in
  if opt.zoom_enlarge then zoom 
  else min zoom 1.0 (* cap at 1.0 *)

let scale_ar t_size ?(fit=opt.fit) ?(interp=`NEAREST) pixbuf =
  let orig = pixbuf_size pixbuf in
  let ar_size = mul_size orig (find_zoom t_size orig fit) in
  scale_raw ar_size ~interp pixbuf

let size_fits (w1,h1) (w2,h2) = w1-w2 <= 2 && w1-w2 >= -10 && h1-h2 <= 2 && h1-h2>= -10

(* MAIN DATA STRUCTURES *)

(* A single picture - (index, filename, size) *)
type ifs = {idx : int; filename: string; size: (int * int) Lazy.t}

(* A display unit - either one picture or two pics side-by-side *)
type spread = One of ifs | Two of ifs * ifs

(* Used to lazily read books into memory *)
type future = | Group of Set.NumStringSet.t Map.NumStringMap.t Lazy.t 
	      | Book of ifs list

(* Each spread is put into a node, which keeps track of which spread
   is next / previous, which book that spread is in, and if there's a
   pre-computed pixbuf to display or scaler task running for it *)

type node = { mutable next : node; 
	      mutable prev: node;
	      book : book;
	      mutable pixbuf : GdkPixbuf.pixbuf option;
	      mutable scaler : Glib.Idle.id option;
	      mutable pics : spread; 
	    }

(* Nodes are grouped into books, which have a title, page cache, and a
   future structure for lazily-reading the book's files into nodes. *)

and book = { title: string;
	     mutable first_page : node; 
	     mutable last_page : node; 
	     (* all files not yet made into nodes *)
	     (* [ (title,fn list) ] *)
	     mutable more_files : future;
	     page_cache : GdkPixbuf.pixbuf Weak.t;
	     mutable preloader : Glib.Idle.id option;
	   }


(* UTILITY FUNCTIONS ON NODES *)
let get_page cache idx fn =
  match Weak.get cache idx with 
      Some pb -> pb 
    | None -> let pb = GdkPixbuf.from_file fn in
      Weak.set cache idx (Some pb);
      pb

let get_idxes s = match s.pics with One i -> [i.idx] | Two (i2,i1) when opt.manga -> [i1.idx;i2.idx] | Two (i1,i2) -> [i1.idx;i2.idx]
let first_idx s = match s.pics with One i -> i.idx | Two (i1,i2) -> min i1.idx i2.idx
let last_idx s = match s.pics with One i -> i.idx | Two (i1,i2) -> max i1.idx i2.idx
let get_pos s = first_idx s
let merge_size (w1,h1) (w2,h2) = (w1 + w2, max h1 h2)
let get_isize i = Lazy.force i.size
let get_size s = match s.pics with One i -> get_isize i 
  | Two(i1,i2) ->  merge_size (get_isize i1) (get_isize i2)
let is_vert ifs = let w0,h0 = get_isize ifs in w0 < h0
let get_width ifs = snd (get_isize ifs)
let get_filenames n = match n.pics with
    One ifs -> Filename.basename ifs.filename
  | Two (i2, i1) when opt.manga -> 
      Printf.sprintf "%s,%s" 
	(Filename.basename i1.filename) (Filename.basename i2.filename)
  | Two (i1, i2) -> 
      Printf.sprintf "%s,%s" 
	(Filename.basename i1.filename) (Filename.basename i2.filename)

let archive_suffixes = [("rar", `Rar); ("cbr", `Rar); ("zip", `Zip); ("cbz", `Zip); ("7z", `Sev_zip); ("lzh", `Lha)]
let pic_suffixes = [("jpg", `Jpeg); ("jpeg", `Jpeg); ("gif", `Gif); ("png", `Png);(*any others?*) ]
  
let fold_sufchecks fn set_acc init suf_list = 
  let folder acc (s, tag) =
    if Filename.check_suffix (lowercase fn) s then set_acc tag else acc in 
  List.fold_left folder init suf_list
    
let suffix_type suf_list fn = fold_sufchecks fn (fun tag -> tag) `None suf_list
let any_suffix suf_list fn = fold_sufchecks fn (fun _ -> true) false suf_list
  
let archive_type fn = suffix_type archive_suffixes fn
let is_archive fn = any_suffix archive_suffixes fn
let is_picture fn = any_suffix pic_suffixes fn

let extract_command file dir = 
  match archive_type file with
      `Rar -> Printf.sprintf "unrar x \"%s\" \"%s\"/" file dir
    | `Zip -> Printf.sprintf "unzip \"%s\" -d \"%s\"" file dir
    | `Sev_zip -> Printf.sprintf "7za -o%s x \"%s\"" dir file
    | `Lha -> Unix.mkdir dir 0o700; Printf.sprintf "lha xw=\"%s\" \"%s\"" dir file
    | `None -> assert false

let rec rec_del path = 
  Printf.eprintf "%s, " path;
  let remove fn = if Sys.is_directory fn then rec_del fn else Unix.unlink fn in
  if Sys.file_exists path then begin
    Sys.readdir path
    |> Array.map (Filename.concat path)
    |> Array.iter remove;
    try Unix.rmdir path with _ -> eprintf "***Failed to delete: %s\n" path
  end

let rec_del path = 
  Printf.eprintf "Cleaning up ";
  rec_del path;
  Printf.eprintf "done.\n";
  flush stderr


(*let incremental_del = TODO IMPLEMENT non-blocking delete*)

let dirs_to_cleanup = Queue.create ()

let extract_archive file =
  let td_suff = (Printf.sprintf "coml-%d" (Random.bits ())) in
  let td = Filename.concat Filename.temp_dir_name td_suff in
  set_status (Printf.sprintf "Extracting %s to %s\n" file td);
  let _retval = Sys.command (extract_command file td) in
  Queue.add td dirs_to_cleanup;
(*  if Queue.length dirs_to_cleanup > 5 then
    ignore(Idle.add ~prio:(spread_gc_prio) 
	     (fun () -> rec_del (Queue.take dirs_to_cleanup); false)); *)
  td

let to_clusters filename =   
  let files_in path = Sys.readdir path |> Array.to_list |> List.map (Filename.concat path) in
  (* map is title -> NumStringSet.t *)
  let add_entries t map fs = 
    let s0 = try Map.NumStringMap.find t map 
             with Not_found -> Set.NumStringSet.empty in
    let s1 = List.fold_left (fun s fn -> Set.NumStringSet.add fn s) s0 fs in
    Map.NumStringMap.add t s1 map
  in
  let rec expand_list fn ?(title=fn) map = 
(*printf "Expanding: %s\n" fn;*)
    if not (Sys.file_exists fn) then map
    else if Sys.is_directory fn then 
(*      let fn = Data.Text.String.chomp ~char:'/' fn in*) (* remove trailing / if exists *)
      let contents = files_in fn in
      let dirs,files = List.partition Sys.is_directory contents
      and title = Filename.basename fn in
      let map2 = files |> List.filter is_picture |> add_entries title map in
      List.fold_left (fun map fn -> expand_list fn map) map2 dirs
    else if is_archive fn then
      let td = extract_archive fn in
      expand_list td ~title:fn map
    else if is_picture fn then
      add_entries (Filename.basename fn) map [fn]
    else map
  in
  expand_list filename Map.NumStringMap.empty

let add_node_after n0 ?(book=n0.book) ifsl = 
  let rec n1 = {next = n1; prev=n0; book=book; 
		pixbuf = None; scaler = None; 
		pics = ifsl;} in
  (* If the last  node doesn't point to itself, fix up the pointers *)  
  if n0.next != n0 then (n1.next <- n0.next; n0.next.prev <- n1);
  n0.next <- n1

let del_node n = 
  if n.prev != n && n.next != n then (n.prev.next <- n.next; n.next.prev <- n.prev)
  else if n.prev == n && n.next == n then failwith "Deleting last node"
  else if n.prev == n (* n is self-pointing first *) then n.next.prev <- n.next
  else if n.next == n (* n is self-pointing last *) then n.prev.next <- n.prev

let link from_n to_n = to_n.prev <- from_n; from_n.next <- to_n

let cur_node = ref (Obj.magic 0) (* gets set to a real value before window gets shown *)
let idxes_on_disp () = get_idxes !cur_node
let max_index ?(cn= !cur_node) () = last_idx cn.book.last_page
let within_book_range ?(cn= !cur_node) x = x >= 0 && x <= max_index ~cn ()

let near_cur_node n = abs (last_idx !cur_node - first_idx n) < 4

let rec gen_nodes b () = 
  let gen_ifs_lazy c i f = 
    {idx=i; filename=f; size=lazy(pixbuf_size (get_page c i f))} in
  let gen_ifses c l = List.mapi (fun i fn -> gen_ifs_lazy c (i+1) fn) l in
  let make_book title files = 
    let files = Set.NumStringSet.elements files in
    let count = List.length files in
    eprintf "Making book %s with %d pages\n" 
      title count; flush stderr;
    match files with [] -> ()
      | p0 :: rest -> 
	  let pc = Weak.create count in
	  let rec n0 = {next = n0; prev = n0; book = b1; pixbuf = None;
			scaler = None; pics = One (gen_ifs_lazy pc 0 p0);} 
	  and b1 = {title=title; first_page=n0; last_page=n0; 
		   more_files = Book (gen_ifses pc rest);
		   page_cache = pc; preloader = None } in
	  let n_prev = b.last_page and n_next = b.last_page.next in
	  link n_prev n0; link n0 n_next;
	  ignore(Idle.add ~prio:(gen_page_prio) (gen_nodes b1));
  in
  let page ifsl = 
    add_node_after b.last_page ifsl; 
    b.last_page <- b.last_page.next;
    window#set_title (Printf.sprintf "Image_idx %s of %d, Book %s" (get_idxes !cur_node |> string_of_int_list) (max_index ~cn:!cur_node ()) !cur_node.book.title);
    true 
  and can_join ifs1 ifs2 = opt.twopage && 
    ( (is_vert ifs1 && is_vert ifs2)
      || (get_width ifs1 + get_width ifs2 < snd (view_size ()))
    ) in
  match b.more_files with
    | Book (i1 :: i2 :: rest) when can_join i1 i2 -> 
	b.more_files <- Book rest; page (Two (i1,i2))
    | Book (i1 :: rest) (* can't join with next *)-> 
	b.more_files <- Book rest; page (One i1)
    | Book [] -> false (* done generating this book *)
    | Group bks -> 
	if Lazy.lazy_is_val bks then begin (* already generated this book *)
	  print_endline ("Already generated: " ^ (dump bks));
	  false
	end else
	  let map = Lazy.force bks in
(*	  let count = NumStringMap.fold (fun _ _ -> succ) map 0 in
	  eprintf "generating %d books\n" count; flush stderr;*)
	  Map.NumStringMap.iter make_book map;
	  ( try del_node b.last_page;
	    with Failure _ -> usage "Can't expand any books" );
	  false

let make_book lib file =
  let n = 
    let get_p0 = lazy(pixbuf_size (GdkPixbuf.from_file file)) in
    let ifs = {idx = 0; filename=file; size=get_p0} in
    let rec n0 = {next = n0; prev = n0; book = b; pixbuf = None;
		 scaler = None; pics = One ifs;} 
    and b = {title=file; first_page=n0; last_page=n0; 
	     more_files = Group (Lazy.lazy_from_fun(fun () ->to_clusters file));
	     page_cache = Weak.create 1; preloader = None} in
    n0
  in
  let link from_n to_n = to_n.prev <- from_n; from_n.next <- to_n in
  match lib with
    | None -> Some (n,n)
    | Some (n0,nx) ->
	link nx n; (* put n after nx *)
	if opt.wrap then link n n0; (* put n0 after n *)
	Some (n0,n)

let build_books l = List.fold_left make_book None l
;;
 
let fresh_pixbuf s =
  let get_page i = get_page s.book.page_cache i.idx i.filename in
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
  
let cleanup_all_task l = singleton_task spread_clean_prio
  (fun () -> List.iter (fun n -> n.pixbuf <- None) !l; l := []; )

let set_pb,reset_pb = 
  let l = ref [] in
  (fun n pb -> n.pixbuf <- Some pb; l := n :: !l; if List.length !l > 15 then cleanup_task l ()),
  (fun () -> cleanup_all_task l ())

let set_node_pixbuf size n interp = 
  let display pic zoom = 
    if n == !cur_node then begin
      image#set_pixbuf pic;
      window#set_title (Printf.sprintf "Image_idx %s of %d, Book %s @ %2.2f%%" (get_idxes n |> string_of_int_list) (max_index ~cn:n ()) n.book.title (zoom *. 100.));
      (* re-draw the screen *)
      ignore (Glib.Main.iteration true);
      let hadj = scroller#hadjustment 
      and vadj = scroller#vadjustment in
      let hmax = hadj#upper -. hadj#page_size
      and vmax = vadj#upper -. vadj#page_size in
      if hadj#value < hmax || vadj#value < vmax then begin
	vadj#set_value vadj#lower;
	hadj#set_value (if opt.manga then hmax else hadj#lower);
      end else begin
	vadj#set_value vadj#lower;
	hadj#set_value hadj#lower;
      end;
    end
  in
  let zoom = find_zoom size (get_size n) opt.fit in
  let ar_size = mul_size (get_size n) zoom in
  set_status (get_filenames !cur_node);
  match n.pixbuf with 
      Some pb when size_fits (pixbuf_size pb) ar_size -> display pb zoom
    | None | Some _ -> 
	let pb = (scale_raw ar_size ~interp (fresh_pixbuf n)) in
	set_pb n pb;
	display pb zoom

let rec idle_scale node =
  let scaler () =
    node.scaler <- None;
    (* don't bother scaling if far away from current position *)
    if near_cur_node node then begin
      try 
	set_node_pixbuf (view_size ()) node `HYPER;
      with GdkPixbuf.GdkPixbufError(_,msg) | Glib.GError msg -> 
(*	eprintf "Error loading pixmap for page %s\n" (Obj.dump n.pics);
	flush stderr; *)
	(match node.book.more_files with 
	     Group _ -> ignore(gen_nodes node.book ());
	   | Book (_::_) -> ignore(gen_nodes node.book ()); del_node node
	   | Book _ -> del_node node );
	if node.next != node then idle_scale node.next
    end;
    false
  in
  if node.scaler = None then
     node.scaler <- Some (Idle.add ~prio:scale_prio scaler)

let preload_spread = singleton_task preload_prio 
  (fun () -> idle_scale !cur_node.next)

let dump_pages b =
  let n = ref b.first_page in
  let act n = match n.pics with One i -> eprintf "(%d)" i.idx | Two (i1,i2) -> eprintf "[%d,%d]" i1.idx i2.idx in
  while !n != b.last_page do act !n; n := !n.next done; act !n;
  eprintf "\n"; flush stderr

let show_spread () = 
   window#set_title (Printf.sprintf "Image_idx %s of %d, Book %s" (get_idxes !cur_node |> string_of_int_list) (max_index ~cn:!cur_node ()) !cur_node.book.title);
   let rec show () = 
     let n = !cur_node in
     (* queue the good rescale *)
     idle_scale n;
     try 
       (* do quick rescale - takes care of updating the display *)
       set_node_pixbuf (view_size()) n `TILES;
       preload_spread (); 
     with GdkPixbuf.GdkPixbufError(_,msg) | Glib.GError msg -> 
(*       eprintf "Error loading pixmap for page %s: %s\n" 
	 (Obj.dump n.pics) (Obj.dump n.book.more_files);
       flush stderr;*)
       (* delete bad node, after processing any lazy pages from its book *)
       (match n.book.more_files with  
	   Group _ -> ignore(gen_nodes n.book ());
	 | Book (_::_) -> ignore(gen_nodes n.book ()); del_node n
	 | Book [] -> del_node n ); 
       cur_node := if n.next == n then n.prev else n.next;
       show ()
   in
   singleton_task show_prio show ()

(* PAGE CHANGING FUNCIONS *)
let next_page () = !cur_node.next
let prev_page () = !cur_node.prev
let first_page () = !cur_node.book.first_page
let last_page () = !cur_node.book.last_page
let next_book () = !cur_node.book.last_page.next
let prev_book () = !cur_node.book.first_page.prev
  
let new_page page_f () = 
  cur_node := page_f (); 
  if not (!cur_node.next == !cur_node || 
	    !cur_node.next.prev == !cur_node) 
  then eprintf "Linking failure: %s:%d/%d\n" (!cur_node.book.title) 
    (get_pos !cur_node) (get_pos !cur_node.book.last_page);
  show_spread ()


let magic_next () = 
  let hadj = scroller#hadjustment and vadj = scroller#vadjustment in
  let set_clamp adj new_val = 
    let clamped_val = max 0. (min (adj#upper -. adj#page_size) new_val) in
    adj#set_value clamped_val in
  let scroll_top () = vadj#set_value 0. in
  if vadj#value < vadj#upper -. vadj#page_size then 
    set_clamp vadj (vadj#value +. vadj#page_increment)
  else if opt.manga && hadj#value > 0. then begin
    if vadj#value != 0. then scroll_top ();
    set_clamp hadj (hadj#value -. hadj#page_increment)
  end else if (not opt.manga) && 
    hadj#value < hadj#upper -. hadj#page_size then begin
    if vadj#value != 0. then scroll_top ();
    set_clamp hadj (hadj#value +. hadj#page_increment)
  end else
    new_page next_page ()
    
let find_page i () = 
  assert (within_book_range i);
  let not_pg_i n = not (List.mem i (get_idxes n)) in
  (* search in correct direction *)
  let move = if get_pos !cur_node < i then next_page else prev_page in
  while not_pg_i !cur_node do cur_node := move () done;
  show_spread ()


(* GROUPING IFSes INTO NODES *)
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
  while get_pos !n < get_pos !n.next do act !n; n := !n.next done; act !n

let toggle_twopage () =
  opt.twopage <- not opt.twopage;
  regroup_pages !cur_node.book.first_page ();
  show_spread ()

let toggle_single_page n =
  (match n.pics with | One _ -> merge n | Two _ -> split n); 
  regroup_pages n.next ();
  show_spread ()

let toggle_cur_twopage () = toggle_single_page !cur_node

let handle_fullscreen () = 
  if opt.fullscreen then begin
    footer#misc#hide ();
    window#fullscreen ();
  end else begin
    footer#misc#show ();
    window#unfullscreen ();
  end;
  show_spread ()


let toggle_fullscreen () = 
  opt.fullscreen <- not opt.fullscreen;
  handle_fullscreen ()

let exit_fullscreen () = opt.fullscreen <- false; handle_fullscreen ()

let toggle_manga () =
  opt.manga <- not opt.manga; 
  reset_pb (); 
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

(* type scaling = Fit_w | Fit_h | Fit_both | Zoom of float *)
let zoom ar_func = 
  let cur_zoom = find_zoom (view_size ()) (get_size (!cur_node)) opt.fit in
  opt.fit <- (ar_func cur_zoom);
  show_spread()

let zoom_out () =   
  scroller#set_hpolicy `AUTOMATIC;
  scroller#set_vpolicy `AUTOMATIC;
  zoom (fun ar -> Zoom (ar *. 0.95))
and zoom_in () = 
  scroller#set_hpolicy `AUTOMATIC;
  scroller#set_vpolicy `AUTOMATIC;
  zoom (fun ar -> Zoom (ar /. 0.95))
and zoom_full () =        
  scroller#set_hpolicy `AUTOMATIC;
  scroller#set_vpolicy `AUTOMATIC;
  zoom (fun _ -> Zoom 1.)
and zoom_height () = 
  scroller#set_hpolicy `NEVER;
  scroller#set_vpolicy `AUTOMATIC;
  zoom (fun _ -> Fit_h)
and zoom_width () = 
  scroller#set_hpolicy `AUTOMATIC;
  scroller#set_vpolicy `NEVER;
  zoom (fun _ -> Fit_w)
and zoom_both () = 
  scroller#set_hpolicy `NEVER;
  scroller#set_vpolicy `NEVER;
  zoom (fun _ -> Fit_both)


(* gets set to first and last books when they're built *)
let first_last = ref None
let toggle_wrap () = 
  opt.wrap <- not opt.wrap;
  match !first_last with 
      None -> eprintf "Cannot change wrap\n";
    | Some (b0,bx) -> 
	b0.first_page.prev <- if opt.wrap then bx.last_page else b0.first_page;
	bx.last_page.next <- if opt.wrap then b0.first_page else bx.last_page
	  
let quit () = 
  window#misc#hide ();
  save_opts ();
  Queue.iter rec_del dirs_to_cleanup;
  Main.quit ()

(* key bindings as a list of pairs  (key, function) *)  
open GdkKeysyms

let actions = [(_q, quit);
	    (_Left, new_page prev_page); 
	    (_Up, new_page prev_page); (_BackSpace, new_page prev_page);
	    (_Right, new_page next_page); (_Down, new_page next_page); 
	    (_space, magic_next);
	    (_f, toggle_fullscreen); (_F11, toggle_fullscreen);
	    (_Escape, exit_fullscreen); (_t, toggle_twopage);
	    (_m, toggle_manga); (_s, zoom_both);
	    (_w, zoom_width); (_h, zoom_height);
	    (_minus, zoom_out); (_plus, zoom_in); (_equal, zoom_full);
(*	    (_l, (fun () -> load_cache (List.fold_left min 0 !image_idxes)));*)
	    (_w, toggle_wrap); (_1, toggle_cur_twopage);
	    (_Home, new_page first_page); (_End, new_page last_page);
	    (_Page_Up, new_page prev_book); (_Page_Down, new_page next_book);
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
  let arg_list = if arg_list = [] then ["."] else arg_list in
  
  begin match build_books arg_list with
      None -> usage "No books found"
    | Some (n0,nx) -> 
	new_page (fun () -> n0) ();
	first_last := Some (n0.book,nx.book)
  end;

  let prev = GButton.button ~stock:`GO_BACK ~packing:bbox#pack ()
  and next = GButton.button ~stock:`GO_FORWARD ~packing:bbox#pack () in
  ignore (prev#connect#clicked ~callback:(new_page prev_page));
  ignore (next#connect#clicked ~callback:(new_page next_page));

  let close = GButton.button ~stock:`CLOSE ~packing:bbox#pack () in
  ignore (close#connect#clicked ~callback:quit);

  ignore (window#connect#destroy ~callback:quit);
  ignore (window#event#connect#key_press ~callback:handle_key);
  ignore (window#event#connect#configure ~callback:resized);
  
  handle_fullscreen ();
  window#show ();
  Main.main () (* calls the GTK main event loop *)
    
let _ = main ()
