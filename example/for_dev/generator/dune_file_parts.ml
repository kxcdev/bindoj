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

    let (&) f a = f a
    let (&.) f a = f [a]

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

  module Library = struct
    let libraries' xs = hovpbox ~lead:"libraries" xs
    let libraries xs = (libraries' % List.map str) xs

    let modules' xs = hovpbox ~lead:"modules" xs
    let modules xs = (modules' % List.map str) xs

    let mk ~name
          ?libraries:libs ?libraries':libs'
          ?modules:mods ?modules':mods'
          clauses =
      let clauses = match libs', libs with
        | None, None -> clauses
        | None, Some libs -> libraries libs :: clauses
        | Some libs', None -> libraries' libs' :: clauses
        | _ -> failwith "Library.mk cannot specify both ?libraries and ?libraries'"
      in
      let clauses = match mods', mods with
        | None, None -> clauses
        | None, Some mods -> modules mods :: clauses
        | Some mods', None -> modules' mods' :: clauses
        | _ -> failwith "Library.mk cannot specify both ?modules and ?modules'"
      in
      vpbox ~lead:"library" (hpbox ~lead:"name" [str name] :: clauses)
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
