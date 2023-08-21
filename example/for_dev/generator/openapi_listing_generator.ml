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
open Kxclib

module Template_parts = struct
  let prelude = {_html|<!DOCTYPE html>
<html style="height: calc(100vh - 20px); padding: 0">
<head>
  <meta charset='utf-8'>
  <meta http-equiv='X-UA-Compatible' content='IE=edge'>
  <title>Bindoj example/for_dev/apidir_examples/openapi/html</title>
  <meta name='viewport' content='width=device-width, height=device-height, initial-scale=1'>
  <style>
    html, body {
      overflow-y: hidden;
    }
    body {
      position: relative;
    }
    body * {
      margin: 0;
    }
  </style>
  <script>
    (() => {
      var handleHash;
      var listingContainer;
      var previewIframe;
      var previewContainer;
      var previewNavigateLink;

      const $$ = id => document.getElementById(id);

      addEventListener("hashchange", handleHash = ((evt) => {
        const hash = window.location.hash;
        console.debug("handleHash: ", hash, "; event: ", evt);
        if (hash && !!hash.trim()) {
          const loc = hash.substring(1);
          listingContainer.style.height = "calc(20% - 20px)";
          previewIframe.src = loc;
          previewNavigateLink.href = loc;
          previewContainer.hidden = false;
        } else {
          listingContainer.style.height = "100%";
          previewContainer.hidden = true;
        }
      }));
      addEventListener("DOMContentLoaded", (evt) => {
        listingContainer = $$("listing-container");
        previewIframe = $$("preview-iframe");
        previewContainer = $$("preview-container");
        previewNavigateLink = $$("preview-navigate-link");

        var hash;
        if (!!(hash = window.location.hash)) handleHash(evt);
      });
    })();
  </script>
</head>
<body style="height: calc(100vh - 20px);">
<ul id="listing-container" style="overflow: scroll;">
|_html}

  let postlude = {_html|
</ul>
<div hidden id="preview-container" style="height: 80%; width: 100%;">
  <span style="display: block; position: relative; float: right;">
    <a id="preview-navigate-link" style="color: inherit;" href="#">[navigate]</a>
    <span style="display: inline-block; width: 0.4em;"></span>
    <a style="color: inherit;" href="#">(x) close preview</a>
  </span>
  <iframe id="preview-iframe" src="./sample_apidir_01.openapi.html" width="100%" height="100%"></iframe>
</div>
</body>
</html>
|_html}

  let entry_listing_item ~title ~href ~json_href ~typescript_metainfo_href =
    sprintf {_tmpl|<li><a href="%s">%s</a> (<a href="#%s">preview</a>, <a href="%s">json</a>, <a href="%s">typescript-metainfo</a>)</li>|_tmpl}
      href title href json_href typescript_metainfo_href
end

let generate_listing_index_html sample_names =
  let open Template_parts in
  let body =
    sample_names
    |&> (fun name ->
      "  "
      ^entry_listing_item ~title:name
         ~href:("./"^name^".openapi.html")
         ~json_href:("../"^name^".json")
          ~typescript_metainfo_href:("../../typescript-metainfo/"^name^".ts"))
    |> String.concat "\n" in
  prelude ^ body ^ postlude

let () =
  let sample_names =
    ArgOptions.(get_option_exn (StringOption "-samples"))
    |> String.split_on_char ':' in
  generate_listing_index_html sample_names
  |> print_endline
