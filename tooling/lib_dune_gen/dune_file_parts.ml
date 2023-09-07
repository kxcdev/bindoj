(* Copyright 2022-2023 Kotoi-Xie Consultancy, Inc. This file is a part of the

==== Bindoj (https://kxc.dev/bindoj) ====

software project that is developed, maintained, and distributed by
Kotoi-Xie Consultancy, Inc. (https://kxc.inc) which is also known as KXC.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy
of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required
by applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
OF ANY KIND, either express or implied. See the License for the specific
language governing permissions and limitations under the License.
                                                                              *)
(* Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.
                                                                              *)
module Ffm = Fmt
open Kxclib

let mangle_library_name s =
  s |> String.map (function
           | '-' -> '_'
           | c -> c)

module Fcomb = struct
  include Ffm

  module Ops = struct
    let (&) f a = f a
    let (&.) f a = f [a]
  end

  include Ops

  let str s = const string s
  let wrap_lead lead xs = match lead with
    | None -> xs
    | Some lead -> str lead :: xs
  let hpbox ?lead = parens % hbox % concat ~sep:sp % wrap_lead lead
  let vpbox ?lead = parens % vbox % concat ~sep:sp % wrap_lead lead
  let hvpbox ?lead = parens % hvbox % concat ~sep:sp % wrap_lead lead
  let hovpbox ?lead = parens % hvbox % concat ~sep:sp % wrap_lead lead
  let cfmt f x = const (fmt f) x
  let leadbox ?lead = hovbox % concat ~sep:sp % wrap_lead lead
end open Fcomb

module Deps = struct
  let mk xs = hvpbox ~lead:"deps" xs
  let atomic s = str s
  let named var_name value = hvpbox ~lead:(":"^var_name) &. value
end

let promote_until_clean () = str "(mode (promote (until-clean)))"

let subdir dir stanzas =
  vpbox & (leadbox ~lead:"subdir" &. str dir) :: stanzas

let alias s = str (sprintf "(alias %s)" s)
let targets xs = hvpbox ~lead:"targets" xs
let target s = hvpbox ~lead:"target" &. str s
let target' x = hvpbox ~lead:"target" &. x

let many (* for each, create one stanza / clauses *) xs = list ~sep:cut xs

let aggregate stanzas = vbox & concat ~sep:cut stanzas

let modules' xs = hovpbox ~lead:"modules" xs
let modules xs = (modules' % List.map str) xs
let libraries' xs = hovpbox ~lead:"libraries" xs
let libraries xs = (libraries' % List.map str) xs
let modes' xs = hvpbox ~lead:"modes" xs
let modes xs = (modes' % List.map str) xs
let preprocess' xs = hvpbox ~lead:"preprocess" xs
let preprocess xs = (preprocess' % List.map str) xs

open struct
  let add_clause f' xs xs' msg clauses: 'a t list =
    match xs, xs' with
    | None, None -> clauses
    | Some xs, None -> (f' % List.map str) xs :: clauses
    | None, Some xs' -> f' xs' :: clauses
    | _ -> failwith "%s" msg
end

module Library = struct
  let mk ~name
        ?libraries:libs ?libraries':libs'
        ?modules:mods ?modules':mods'
        ?modes:mos ?modes':mos'
        ?preprocess:prep ?preprocess':prep'
        clauses =
    let clauses =
      let error_msg label = sprintf "Library.mk cannot specify both ?%s and ?%s'" label label in
      clauses
      |> add_clause libraries' libs libs' (error_msg "libraries")
      |> add_clause modules' mods mods' (error_msg "modules")
      |> add_clause modes' mos mos' (error_msg "modes")
      |> add_clause preprocess' prep prep' (error_msg "preprocess")
    in
    vpbox ~lead:"library" (hpbox ~lead:"name" [name] :: clauses)
end

module Executable = struct
  let mk ~name
        ?libraries:libs ?libraries':libs'
        ?modules:mods ?modules':mods'
        ?modes:mos ?modes':mos'
        ?preprocess:prep ?preprocess':prep'
        clauses =
    let clauses =
      let error_msg label = sprintf "Executable.mk cannot specify both ?%s and ?%s'" label label in
      clauses
      |> add_clause libraries' libs libs' (error_msg "libraries")
      |> add_clause modules' mods mods' (error_msg "modules")
      |> add_clause modes' mos mos' (error_msg "modes")
      |> add_clause preprocess' prep prep' (error_msg "preprocess")
    in
    vpbox ~lead:"executable" (hpbox ~lead:"name" [name] :: clauses)
end

module Action = struct
  let copy src dest =
    hovpbox ~lead:"copy" [ src; dest ]

  let with_stdout_to spec clause =
    vpbox &
      (leadbox ~lead:"with-stdout-to" &.
          match spec with
          | `target -> str "%{target}"
          | `expr e -> e)
      :: [clause]

  let with_stdout_to_piped spec clauses =
    with_stdout_to spec &
      vpbox ~lead:"pipe-stdout" clauses

  let progn ?stdout_to clauses =
    let body = hvpbox ~lead:"progn" clauses in
    match stdout_to with
    | None -> body
    | Some spec -> with_stdout_to spec body

  let with_accepted_exit_codes codes clause =
    parens % vbox % concat ~sep:sp & [
        hovbox & concat ~sep:sp [str "with-accepted-exit-codes"; codes];
        clause;
      ]

  let with_accepted_exit_codes_progn codes clauses =
    with_accepted_exit_codes codes (progn clauses)

  let with_accepted_exit_code code clause =
    vpbox ~lead:(sprintf "with-accepted-exit-codes %d" code) &. clause

  let with_accepted_exit_code_progn code clauses =
    with_accepted_exit_code code (progn clauses)

  let run ?(compact=false) bin args =
    (if compact then hpbox else hovpbox)
      ~lead:"run" (str bin :: args)

  let run_npx ?compact prog_name args =
    run ?compact "%{bin:npx}" ([
        str "--prefix";
        str "%{workspace_root}/with_js";
        str prog_name;
      ] @ args)

  let cat ?(compact=false) x =
    (if compact then hpbox else hovpbox)
      ~lead:"cat" [x]

  let mk ?stdout_to clause =
    vpbox ~lead:"action" &.
      match stdout_to with
      | None -> clause
      | Some spec -> with_stdout_to spec clause

  let mk_progn ?stdout_to clauses =
    let body = match stdout_to with
      | Some spec -> with_stdout_to_piped spec clauses
      | None -> progn clauses in
    mk body

  let mk_copy src dest =
    (* NB: we intentionally use [hpbox] here which is different from what {!copy} uses *)
    mk & hpbox ~lead:"copy" [ src; dest ]
end

module Rule = struct
  let mk ?alias:alias_ ?(promote : [`until_clean] option) clauses =
    let clauses = match promote with
      | None -> clauses
      | Some `until_clean -> promote_until_clean () :: clauses in
    let clauses = match alias_ with
      | None -> clauses
      | Some a -> alias a :: clauses in
    vpbox ~lead:"rule" clauses

  let mkp ?alias =
    mk ?alias ~promote:`until_clean

  let mkp_gen xs = mkp ~alias:"gen" xs

  let mk_gen ?promote = mk ~alias:"gen" ?promote
  let mk_gen_promote xs = mk_gen ~promote:`until_clean xs

  let mk_copy ?alias ?promote src dest =
    mk ?alias ?promote &. Action.mk_copy src dest

  let mkp_copy ?alias =
    mk_copy ?alias ~promote:`until_clean

  let mkp_copy_gen xs = mkp_copy ~alias:"gen" xs
end

let alias_gen xs = alias "gen" xs
