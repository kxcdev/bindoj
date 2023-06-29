import { NextPage, InferGetStaticPropsType } from "next";
import { useRouter } from "next/router";
import ErrorPage from "next/error";
import Head from "next/head";
// import styles from "../../styles/Home.module.css";
import { getAllDocs, getDocByFileName } from "../api/fetch";
import "prismjs/themes/prism-tomorrow.css";
import "prismjs/plugins/line-numbers/prism-line-numbers.css";

type Props = InferGetStaticPropsType<typeof getStaticProps>;

const docsDirectory = "../example";

export const getStaticPaths = async () => {
  const docs = getAllDocs(docsDirectory, ".html");
  return {
    paths: docs.map((doc) => {
      return {
        params: {
          filename: doc.filename,
        },
      };
    }),
    fallback: false,
  };
};

export const getStaticProps = async ({ params }: any) => {
  const doc = getDocByFileName(docsDirectory, params.filename);
  return {
    props: {
      doc: doc,
    },
  };
};

const Doc: NextPage<Props> = ({ doc }) => {
  const router = useRouter();

  if (!router.isFallback && !doc?.filename) {
    return <ErrorPage statusCode={404} />;
  }

  return <div dangerouslySetInnerHTML={{ __html: doc.content }} />;
};

export default Doc;
