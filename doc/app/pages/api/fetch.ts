// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import fs from "fs";
import path from "path";

type Doc = {
  filename: string;
  content: string;
};

const docsDirectory = path.join(process.cwd(), "../dist");

export function getDocsFileName() {
  const allDirents = fs.readdirSync(docsDirectory, { withFileTypes: true });
  return allDirents
    .filter((dirent) => dirent.isFile() && path.extname(dirent.name).toLowerCase() === '.md')
    .map(({ name }) => name);
}

export function getDocByFileName(filename: string) {
  const fullPath = path.join(docsDirectory, filename);

  const item: Doc = {
    filename: filename,
    content: fs.readFileSync(fullPath, "utf8"),
  };

  return item;
}

export function getAllDocs() {
  return getDocsFileName().map(getDocByFileName)
}
