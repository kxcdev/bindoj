import { remark } from "remark";
import remarkHtml from "remark-html";
import remarkPrism from "remark-prism";

const markdownToHtml = async (markdown: string) => {
  const result = await remark()
  .use(remarkHtml, { sanitize: false })
  .use(remarkPrism)
  .process(markdown);
  return result.toString();
};

export default markdownToHtml;
