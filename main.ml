open Tezos_client_012_Psithaca
open Tezos_protocol_012_Psithaca.Protocol
open Tezos_error_monad.Error_monad.Lwt_result_syntax
(* open Tezos_012_Psithaca_test_helpers *)

(* Copied from tezos/src/proto_012_Psithaca/lib_benchmark/execution_context.ml *)
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

let read_file fn =
  let open Lwt.Syntax in
  let* fd = Lwt_unix.(openfile fn [ O_RDONLY; O_NONBLOCK; O_CLOEXEC ] 0) in
  let read_fd fd =
    let buffer_size = 4096 in
    let buffer = Bytes.create buffer_size in
    let rec read_chunks sum =
      let* n = Lwt_unix.read fd buffer 0 buffer_size in
      if n = 0 then return sum
      else
        let str = Bytes.to_string
            (if n < buffer_size then Bytes.sub buffer 0 n else buffer) in
        read_chunks (sum ^ str) in
    read_chunks "" in
  read_fd fd

let typecheck program =
  let rng_state = Random.State.make [||] in
  let* (ctxt, _) = make ~rng_state in
  let open Tezos_error_monad.Error_monad.Lwt_syntax in
  let* t = Script_ir_translator.typecheck_code ~legacy:false ~show_types:true ctxt program.expanded in
  match t with
  | Ok (type_map, _) -> return (Ok type_map)
  | Error e ->
    Format.eprintf "%a" Environment.Error_monad.pp_trace e;
    failwith "typecheck failure"

let () =
  let f =
    let fn = "./contract.tz" in
    let* source = read_file fn in

    let* (program, _) = Client_proto_programs.Program.of_source source in
    let* type_map = typecheck program in
    (* Format.printf "%a\n" Michelson_v1_emacs.print_type_map (program, type_map); *)
    let program = Michelson_v1_printer.inject_types type_map program in
    Format.printf "%a\n" Tezos_micheline.Micheline_printer.print_expr program;
    return () in
  Lwt_main.run f |> function
  | Ok _ -> () | Error _ -> failwith ""
