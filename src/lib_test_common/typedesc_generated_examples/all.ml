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
let all : (string * (module Util.Ex_generated)) list = [
  "ex_coretype", (module Ex_coretype);
  "ex_alias", (module Ex_alias);
  "ex_record", (module Ex_record);
  "ex_variant", (module Ex_variant);
  "ex_mangling", (module Ex_mangling);
  "ex_optional", (module Ex_optional);
  "ex_ident", (module Ex_ident);
  "ex_nested", (module Ex_nested);
  "ex_nested_multiply", (module Ex_nested_multiply);
  "ex_version_substring", (module Ex_version_substring);
]
