type _ Effect.t +=
  | When : int ref -> int Effect.t

type 'a cown = {
  counter : 'a ref;
}

(* A minimal handler: log that an effect happened *)
let boc (label : string) (f : unit -> unit) : unit =
  Effect.Deep.match_with f ()
    { retc = (fun () -> ());
      exnc = (fun exn -> raise exn);
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | When r ->
             Some
               (fun (k : (a, unit) Effect.Deep.continuation) ->
                 Printf.printf "[%s] read %d\n%!" label !r;
                 r := !r + 1;
                 Printf.printf "[%s] wrote %d\n%!" label !r;
                 Effect.Deep.continue k !r)
          | _ -> None);
    }

(* A shared cell, and two domains that read/write through it. *)
let counter = ref 0

let awhen (r : int ref) : int =
  Effect.perform (When r)

let worker label n () =
  boc label (fun () ->
      for _ = 1 to n do
        counter := awhen(counter)
      done)

let () =
  let d1 = Domain.spawn (worker "domain-a" 10) in
  (* let d2 = Domain.spawn (worker "domain-b" 10) in *)
  Domain.join d1;
  (* Domain.join d2; *)
  Printf.printf "final value: %d\n" (!counter)