(* TODO: Should probably take as argument the contents of a cown *)
type thunk = unit -> unit

(* TODO: Should be polymorphic somehow (possibly as an opaque
   something something modules) *)
type cown =
  Cown of
    {value: int ref;
     identity: int;
     last: (request option) ref
    }

(* TODO: What about behaviors grabbing multiple cowns *)
and behavior =
  Behavior of
    {thunk: thunk;
     count: int ref;
     requests: request list
    }

and request =
  Request of
    {next: (behavior option) ref;
     scheduled: bool ref;
     target: cown
    }

let uid = ref 0

let new_cown value =
  uid := !uid + 1;
  Cown {value = ref value; identity = !uid; last = ref None}

let get_value = function
  | Cown {value} -> !value

let set_value cown new_value =
  match cown with
  | Cown {value} -> value := new_value

let set_count behavior new_count =
  match behavior with
  | Behavior{count} -> count := new_count

let new_request cown =
  Request {next = ref None;
           scheduled = ref false;
           target = cown
    }

let new_behavior thunk cowns =
  Behavior {thunk = thunk;
            count = ref 0;
            requests = List.map new_request cowns}

let get_last = function
  | Cown {last} -> !last

let set_last cown request =
  match cown with
  | Cown {last} -> last := request

let set_next request behavior =
  match request with
  | Request {next} -> next := Some behavior

let is_scheduled = function
  | Request {scheduled} -> !scheduled

let rec release = function
  | Request {next; target} as request ->
     match !next with
     | None ->
        (* TODO: This should be a compare and swap *)
        assert (match get_last target with | None -> false | Some req -> req == request);
        set_last target None
     | Some req ->
        resolve_one req

and resolve_one = function
  | Behavior {thunk; count; requests} ->
     (* TODO: This should be atomic *)
     count := !count - 1;
     if !count = 0 then
       (* TODO: This should be spawned in a new thread *)
       let _ = thunk() in
       let _ = List.map release requests in
       ()

let start_append_req behavior = function
  | Request {target} as request ->
     (* TODO: These two should be atomic *)
     let prev = get_last target in
     let _ = set_last target (Some request) in
     match prev with
     | None ->
        resolve_one behavior
     | Some req ->
        (assert (is_scheduled req);
         set_next req behavior
        )

let finish_append_req = function
  | Request {scheduled} -> scheduled := true

let schedule cown thunk =
  let sorted_cowns = [cown] in
  let behavior = new_behavior thunk sorted_cowns in
  set_count behavior (List.length sorted_cowns + 1);
  (match behavior with
   | Behavior {requests} ->
      let _ = List.map (start_append_req behavior) requests in
      let _ = List.map finish_append_req requests in ());
  resolve_one behavior

let when_ cown thunk = schedule cown thunk

let () =
  let c = new_cown 42 in
  when_ c (fun () ->
      Printf.printf "Hello! My value is %d\n" (get_value c)
  );
  when_ c (fun () ->
      set_value c 4711
  );
  when_ c (fun () ->
      Printf.printf "Hello again! My value is %d\n" (get_value c)
  )