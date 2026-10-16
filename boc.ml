(* TODO: Should probably take as argument the contents of a cown *)
type thunk = unit -> unit

(* TODO: Should be polymorphic somehow (possibly as an opaque
   something something modules) *)
type cown =
  {value: int ref;
   identity: int;
   last: (request option) ref
  }

(* TODO: What about behaviors grabbing multiple cowns *)
and behavior =
  {thunk: thunk;
   count: int ref;
   requests: request list
  }

and request =
  {next: (behavior option) ref;
   scheduled: bool ref;
   target: cown
  }

(* TODO: This should be atomic *)
let uid = ref 0

let new_cown value : cown =
  begin
    uid := !uid + 1;
    {value = ref value; identity = !uid; last = ref None}
  end

let new_request cown : request =
  {next = ref None;
   scheduled = ref false;
   target = cown}

let new_behavior thunk cowns : behavior =
  {thunk = thunk;
   count = ref 0;
   requests = List.map new_request cowns}

let rec release request : unit =
  match !(request.next) with
  | None ->
     (* TODO: This should be a compare and swap *)
     begin
       assert (match !(request.target.last) with | None -> false | Some req -> req == request);
       request.target.last := None
     end
  | Some req ->
     resolve_one req

and resolve_one behavior : unit =
  (* TODO: This should be atomic *)
  behavior.count := !(behavior.count) - 1;
  if !(behavior.count) = 0 then
    (* TODO: This should be spawned in a new thread *)
    begin
      behavior.thunk();
      List.iter release behavior.requests
    end

let start_append_req behavior request : unit =
  (* TODO: These two should be atomic *)
  let prev = !(request.target.last) in
  begin
    request.target.last := Some request;
    match prev with
    | None ->
       resolve_one behavior
    | Some req ->
       begin
         assert (!(req.scheduled));
         req.next := Some behavior
       end
  end

let finish_append_req request : unit =
  request.scheduled := true

(* TODO: Should be able to schedule behavior with multiple cowns *)
let schedule cown thunk : unit =
  let sorted_cowns = [cown] in
  let behavior = new_behavior thunk sorted_cowns in
  begin
    behavior.count := (List.length sorted_cowns + 1);
    List.iter (start_append_req behavior) behavior.requests;
    List.iter finish_append_req behavior.requests;
    resolve_one behavior
  end

let when_ : cown -> thunk -> unit = schedule

let () =
  let c = new_cown 42 in
  when_ c (fun () ->
      Printf.printf "Hello! My value is %d\n" !(c.value)
  );
  when_ c (fun () ->
      c.value := 4711
  );
  when_ c (fun () ->
      Printf.printf "Hello again! My value is %d\n" !(c.value)
  )