open Tezos_client_012_Psithaca
open Tezos_protocol_012_Psithaca.Protocol
open Tezos_error_monad.Error_monad.Lwt_result_syntax
(* open Tezos_012_Psithaca_test_helpers *)

let context_init_memory ~rng_state =
  let open Tezos_012_Psithaca_test_helpers in
  let open Tezos_error_monad.Error_monad in
  Context.init
    ~rng_state
    ~initial_balances:
      [
        4_000_000_000_000L;
        4_000_000_000_000L;
        4_000_000_000_000L;
        4_000_000_000_000L;
        4_000_000_000_000L;
      ]
    5
  >>=? fun (block, accounts) ->
  match accounts with
  | [bs1; bs2; bs3; bs4; bs5] ->
    return (`Mem_block (block, (bs1, bs2, bs3, bs4, bs5)))
  | _ -> assert false

let make ~rng_state =
  let open Tezos_012_Psithaca_test_helpers in
  let open Tezos_error_monad.Error_monad in
  let open Tezos_protocol_012_Psithaca.Protocol in
  context_init_memory ~rng_state >>=? fun context ->
  let amount = Alpha_context.Tez.one in
  let chain_id = Tezos_crypto.Chain_id.zero in
  let now = Alpha_context.Script_timestamp.of_zint Z.zero in
  let level = Alpha_context.Script_int.zero_n in
  let open Script_interpreter in
  (match context with
   | `Mem_block (block, (bs1, bs2, bs3, _, _)) ->
     let source = bs1 in
     let payer = bs2 in
     let self = bs3 in
     let step_constants =
       {source; payer; self; amount; chain_id; now; level}
     in
     return (block, step_constants)
   | `Disk_block (block, source) ->
     let step_constants =
       {source; payer = source; self = source; amount; chain_id; now; level}
     in
     return (block, step_constants))
  >>=? fun (block, step_constants) ->
  Incremental.begin_construction
    ~timestamp:(Tezos_base.Time.Protocol.add block.header.shell.timestamp 30L)
    block
  >>=? fun vs ->
  let ctxt = Incremental.alpha_ctxt vs in
  let ctxt =
    (* Required for eg Create_contract *)
    Alpha_context.Contract.init_origination_nonce
      ctxt
      Tezos_crypto.Operation_hash.zero
  in
  return (ctxt, step_constants)


module S = Set.Make(struct
    type t = string
    let compare = compare
  end)
let () =
  Lwt_main.run begin
    let rng_state = Random.State.make [||] in

    let* source =
      let open Lwt.Syntax in
      let fn = "./contract.tz" in
      let* fd = Lwt_unix.(openfile fn [ O_RDONLY; O_NONBLOCK; O_CLOEXEC ] 0) in
      let read_fd fd =
        let buffer_size = 4096 in
        let buffer = Bytes.create buffer_size in
        let rec read_chunks sum =
          let* n = Lwt_unix.read fd buffer 0 buffer_size in
            if n == 0 then return sum
            else read_chunks (sum ^ Bytes.to_string buffer) in
        read_chunks "" in
      read_fd fd in

    let* (ctxt, _) = make ~rng_state in
    let* (program, _) = Client_proto_programs.Program.of_source source in
    let* (type_map, _) =
      let open Tezos_error_monad.Error_monad.Lwt_syntax in
      let* t = Script_ir_translator.typecheck_code ~legacy:false ~show_types:true ctxt program.expanded in
      match t with Ok t -> return (Ok t) | _ -> failwith "a" in
    (* Format.printf "%a\n" Michelson_v1_emacs.print_type_map (program, type_map); *)
    let program = Michelson_v1_printer.inject_types type_map program in
    Format.printf "%a\n" Tezos_micheline.Micheline_printer.print_expr program;
    return ()
  end |> function
  | Ok _ -> () | Error _ -> failwith ""
