// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import fs from "fs";
import path from "path";

export type Doc = {
  filename: string;
  content: string;
};


export function getDocsFileName(directory: string, ext: string) {
  const docsDirectory = path.join(process.cwd(), directory);
  const allDirents = fs.readdirSync(docsDirectory, { withFileTypes: true });
  return allDirents
    .filter((dirent) => dirent.isFile() && path.extname(dirent.name).toLowerCase() === ext)
    .map(({ name }) => name);
}

export function getDocByFileName(directory: string, filename: string) {
  const docsDirectory = path.join(process.cwd(), directory);
  const fullPath = path.join(docsDirectory, filename);

  const item: Doc = {
    filename: filename,
    content: fs.readFileSync(fullPath, "utf8"),
  };

  return item;
}

export function getAllDocs(directory: string, ext: string) {
  return getDocsFileName(directory, ext).map((name) => getDocByFileName(directory, name));
}
