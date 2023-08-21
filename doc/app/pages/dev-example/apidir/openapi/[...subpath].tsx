import { NextPage, InferGetStaticPropsType } from "next";
import { useRouter } from "next/router";
import ErrorPage from "next/error";
import Head from "next/head";
// import styles from "../../styles/Home.module.css";
import {Doc, getAllDocs, getDocByFileName} from "../../../api/fetch";
import "prismjs/themes/prism-tomorrow.css";
import "prismjs/plugins/line-numbers/prism-line-numbers.css";

type Props = InferGetStaticPropsType<typeof getStaticProps>;

const srcdir = "../../example/for_dev/apidir_examples/openapi";

export const getStaticPaths = async () => {
  const docs = [
    ...getAllDocs(srcdir, ".json").map<[string, Doc]>(doc => ["", doc]),
    ...getAllDocs(`${srcdir}/html`, ".html").map<[string, Doc]>(doc => ["html/", doc]),
  ];
  console.log("da.getStaticPaths docs:", docs.map(([prefix, doc]) => prefix + doc.filename));
  return {
    paths: docs.map(([prefix, doc]) => {
      const filename = prefix + doc.filename;
      return {
        params: {
          subpath: filename.split(/\//),
        },
      };
    }),
    fallback: false,
  };
};

export const getStaticProps = async ({ params }: any) => {
  console.log("da.getStaticProps docs:", params);
  const filename = params.subpath.join('/');
  const doc = getDocByFileName(srcdir, filename);
  var type;
  if (filename.endsWith(".html")) {
    type = "html";
  } else if (filename.endsWith(".json")) {
    type = "json";
  }
  return {
    props: {
      type, doc,
    },
  };
};

const Doc: NextPage<Props> = ({ type, doc }) => {
  const router = useRouter();

  if (!router.isFallback && !doc?.filename) {
    return <ErrorPage statusCode={404} />;
  }

  switch (type) {
    case "html":
      return <>
        <Head>
          <style>
            {"#__next{height: 100%;}"}
          </style>
        </Head>
        <div style={{height: "100%"}} dangerouslySetInnerHTML={{ __html: doc.content }} /></>;
    case "json":
      return <pre>{ doc.content }</pre>;
    default:
      return <>{ doc.content }</>;
  }
};

export default Doc;
