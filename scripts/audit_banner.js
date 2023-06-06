#!/usr/bin/env node

const fs = require('fs')
const process = require('process')
const path = require('path')
const assert = require('assert')

const banner = [
`Copyright 2022 Kotoi-Xie Consultancy, Inc. This file is a part of the

==== Bindoj (https://kxc.dev/bindoj) ====

software project that is developed, maintained, and distributed by
Kotoi-Xie Consultancy, Inc. (https://kxc.inc) which is also known as KXC.

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy
of the License at http://www.apache.org/licenses/LICENSE-2.0. Unless required
by applicable law or agreed to in writing, software distributed under the
License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
OF ANY KIND, either express or implied. See the License for the specific
language governing permissions and limitations under the License.`,

`Acknowledgements  --- AnchorZ Inc. ---  The current/initial version or a
significant portion of this file is developed under the funding provided by
AnchorZ Inc. to satisfy its needs in its product development workflow.`
]

const sourceDirs = [
  "./doc/tests_src",
  "./src",
  "./with_js/src",
  "./with_js/apidir-tests",
  "./with_js/compile-tests",
  "./with_js/apidir-typescript-tests/gen",
  "./with_js/jsoo-integration-tests",
  "./with_js/jsoo-gen/gen",
]

const ignorePatterns = [
  /.+_gen\.[A-Za-z]+$/,
  /.*README\.md$/,
  /src\/lib_kxclib_testlib/,
]

const filetypes = new Map([
  [".mli", { lParen: "(*",   rParen: "*)"  }],
  [".ml",  { lParen: "(*",   rParen: "*)"  }],
  [".ts",  { lParen: "/*",   rParen: "*/"  }],
  [".md",  { lParen: "<!--", rParen: "-->" }]
])

const oldBanners = [
  [`Copyright 2022 Kotoi-Xie Consultancy, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.`,
  `Acknowledgements - AnchorZ Inc.
The initial version or a significant portion of this file is developed
under the funding of AnchorZ Inc. to satisfy its needs in
product development.`],
]

const regexChars = /[-\/\\^$*+?.()|[\]{}]/g;
function regexEscape(str) { return str.replace(regexChars, '\\$&'); }
function bannerPattern(banner, ft) {
  const lp = regexEscape(ft.lParen);
  const rp = regexEscape(ft.rParen);
  const spaces = "\\s*";
  function comment(str) {
    return lp + spaces + regexEscape(str).replace(/\n/g, '\\s*') + spaces + rp;
  };
  const patternStr = banner.map(s => comment(s)).join(spaces);
  const pattern = new RegExp(patternStr);
  return pattern;
}

function createOldBannersMap() {
  const fts = Array.from(filetypes.entries());
  return new Map(
    fts.map(ft => {
      var pats = oldBanners.map(banner => bannerPattern(banner, ft[1]));
      pats.push(bannerPattern(banner, ft[1]));
      return [ft[0], pats]
    })
  );
}
const oldBannersMap = createOldBannersMap();

function createBannerMap() {
  const fts = Array.from(filetypes.entries());
  return new Map(
    fts.map(ft => {
      let finalLine = ' '.repeat(80 - ft[1].rParen.length) + ft[1].rParen;
      return [ft[0], banner.map(s => `${ft[1].lParen} ${s}\n` + finalLine).join('\n')]
    })
  );
}
const bannerMap = createBannerMap();
function hasBanner(content, ext) {
  let banner = bannerMap.get(ext);
  if (!banner) return true;
  return content.indexOf(banner) == 0;
}

function isIgnored(filename) {
  return ignorePatterns.some(pat => pat.test(filename))
}

function colored(str, color=31) {
  return '\x1b[' + color + 'm' + str + '\x1b[0m';
}

function getBanner(ext) {
  return bannerMap.get(ext);
}

function matchOldBanner(contentStr, ext) {
  let pats = oldBannersMap.get(ext);
  if (!pats || pats.length == 0) return undefined;
  else return pats.find(pat => pat.test(contentStr));
}

function validate(filename) {
  if (isIgnored(filename)) return null;
  let ext = path.extname(filename);
  let content = fs.readFileSync(filename);
  if (hasBanner(content, ext)) return null;
  let contentStr = content.toString();
  let matchedPattern = matchOldBanner(contentStr, ext);
  let banner = getBanner(ext);
  assert(banner);
  return { content, contentStr, matchedPattern, banner };
}

function audit(filename) {
  let result = validate(filename);
  if (!result) return true;
  if (result.matchedPattern) {
    console.error(colored(`audit_banner: ${filename} has an outdated banner.`));
    console.error(`-> run 'make promote-audit' to fix it automatically.`);
  }
  else {
    console.error(colored(`audit_banner: ${filename} has a wrong banner.`));
    console.error(`-> append the following to the top of the file:`);
    console.error();
    console.error(colored(banner, 32));
    console.error();
  }
  process.exit(1);
}

function update(filename) {
  let result = validate(filename);
  if (!result) return;
  if (!result.matchedPattern) {
    console.error(colored(`audit_banner: ${filename} could not be updated automatically.`));
    return;
  };
  let rest = result.contentStr.replace(result.matchedPattern, "").trimStart();
  let newContentString = result.banner + '\n' + rest;
  fs.writeFileSync(filename, newContentString);
  console.log(`audit_banner: updated ${filename}.`);
}

function walk(dir, f) {
  fs.readdir(dir, (err, files) => {
    if (err != null) console.error(`error: ${err.message}`);
    files.forEach(file => {
      let filePath = path.resolve(dir, file);
      fs.stat(filePath, (err, stat) => {
        if (err != null) console.error(`error: ${err.message}`);
        if (!stat) return;
        if (stat.isDirectory()) walk(filePath, f);
        if (stat.isFile()) f(filePath);
      });
    });
  });
}

const argv = process.argv.slice(2);
process.chdir(path.join(__dirname, "../"))

switch (argv[0]) {
  case 'update':
    sourceDirs.forEach(dir => walk(dir, update));
    break;
  case 'audit':
  default:
    sourceDirs.forEach(dir => walk(dir, audit));
}
