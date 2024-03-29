import { NextPage, InferGetStaticPropsType } from "next";
import { useRouter } from "next/router";
import ErrorPage from "next/error";
import Head from "next/head";
import styles from "../styles/Home.module.css";
import { getAllDocs, getDocByFileName } from "./api/fetch";
import markdownToHtml from "../utils/markdownToHtml";
import "prismjs/themes/prism-tomorrow.css";
import "prismjs/plugins/line-numbers/prism-line-numbers.css";

type Props = InferGetStaticPropsType<typeof getStaticProps>;

const docsDirectory = "../dist";

export const getStaticPaths = async () => {
  const docs = getAllDocs(docsDirectory, ".md");
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
  const content = await markdownToHtml(doc.content);
  return {
    props: {
      doc: {
        ...doc,
        content,
      },
    },
  };
};

const Doc: NextPage<Props> = ({ doc }) => {
  const router = useRouter();

  if (!router.isFallback && !doc?.filename) {
    return <ErrorPage statusCode={404} />;
  }

  return (
    <div className={styles.container}>
      <Head>
        <title>Hello world!</title>
        <meta name="description" content="Generated by create next app" />
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <main className={styles.main}>
        <article>
          <h1 className={styles.title}>{doc.filename}</h1>
          <div className={styles.grid}>
            <div>
              <div dangerouslySetInnerHTML={{ __html: doc.content }} />
            </div>
          </div>
        </article>
      </main>
    </div>
  );
};

export default Doc;
