#!/usr/bin/env node

const fs = require('fs')
const process = require('process')
const path = require('path')

const banner = [
  `Copyright 2022 Kotoi-Xie Consultancy, Inc.
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
product development.`
]
const bannerLines = banner.flatMap(s => s.split('\n'))

const sourceDirs = [
  "./doc/tests_src",
  "./src",
  "./with_js/src",
  "./with_js/compile-tests",
  "./with_js/jsoo-integration-tests"
]

const ignorePatterns = [
  /.*with_js\/jsoo-integration-tests\/import\/.+$/,
  /.+_gen\.[A-Za-z]+$/,
  /.*README\.md$/,
]

const filetypes = new Map([
  [".mli", { lParen: "(*",   rParen: "*)"  }],
  [".ml",  { lParen: "(*",   rParen: "*)"  }],
  [".ts",  { lParen: "/*",   rParen: "*/"  }],
  [".md",  { lParen: "<!--", rParen: "-->" }]
])

function isIgnored(filename) {
  return ignorePatterns.some(pat => pat.test(filename))
}

function hasBanner(content) {
  var index = -1;
  return bannerLines.every(s => {
    let newIndex = content.indexOf(s);
    let isValid = newIndex > index;
    index = newIndex;
    return isValid;
  });
}

function getExampleBanner(ft) {
  return banner.map(s => `${ft.lParen} ${s} ${ft.rParen}`).join("\n\n");
}

function audit(filename) {
  if (isIgnored(filename)) return true;
  let ext = path.extname(filename);
  let ft = filetypes.get(ext);
  if (!ft) return true;
  let content = fs.readFileSync(filename);
  if (hasBanner(content)) return true;
  let banner = getExampleBanner(ft);
  console.error(`audit_banner: ${filename} has a wrong banner.`);
  console.error(`append the following to the top of the file:`);
  console.error();
  console.error(banner);
  console.error();
  process.exit(1);
}

function walk(dir) {
  fs.readdir(dir, (err, files) => {
    if (err != null) console.error(`error: ${err.message}`);
    files.forEach(file => {
      let filePath = path.resolve(dir, file);
      fs.stat(filePath, (err, stat) => {
        if (err != null) console.error(`error: ${err.message}`);
        if (!stat) return;
        if (stat.isDirectory()) walk(filePath);
        if (stat.isFile()) audit(filePath);
      });
    });
  });
}

process.chdir(path.join(__dirname, "../"))
sourceDirs.forEach(dir => walk(dir));
